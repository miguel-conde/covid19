# https://timchurches.github.io/blog/posts/2020-03-10-modelling-the-effects-of-public-health-interventions-on-covid-19-transmission-part-1/#the-epimodel-package-for-r

# LIBRARIES and SOURCES ---------------------------------------------------

source("get_clean_data.R", encoding = "UTF8")
source("utils.R", encoding = "UTF8")

library(lubridate)
library(EpiModel)

# AUXILIARY FUNCTIONS -----------------------------------------------------

run_sir_sim <- function(inf_prob, act_rate, 
                        pop_size = 1000, 
                        i_num = 3, 
                        n_steps = 365, 
                        n_sims = 10, 
                        si_mean = 7.5, 
                        si_sd = 3.4) {
  
  # set up simulation parameters
  param <- param.icm(inf.prob = inf_prob, 
                     act.rate = act_rate, 
                     rec.rate = 1/20, 
                     a.rate = (10.5/365)/1000, 
                     ds.rate = (7/365)/1000, 
                     di.rate = (14/365)/1000, 
                     dr.rate = (7/365)/1000)
  
  init <- init.icm(s.num = pop_size - i_num, 
                   i.num = i_num, 
                   r.num = 0)
  
  control <- control.icm(type = "SIR", nsteps = n_steps, nsims = n_sims)
  
  # run the simulation
  sim <- icm(param, init, control)
  
  # collect the relevant results in a data frame
  incidence_rates <- as.data.frame(sim, out = "mean") %>% 
    select(time, si.flow, i.num) %>% 
    mutate(act_rate = act_rate, 
           inf_prob = inf_prob, 
           total_cases = sum(si.flow), 
           max_prev = max(i.num, na.rm = TRUE))
  
  # use the data frame of results to create an incidence()
  # object
  local_case_dates <- incidence_rates %>% 
    filter(time <= 300, act.rate == act_rate, inf.prob == inf_prob) %>% 
    select(time, si.flow) %>% 
    uncount(si.flow) %>% pull(time)
  
  if (length(local_case_dates) > 0) {
    
    local_cases <- local_case_dates %>% incidence(.)
    
    # find the incidence peak from the incidence object
    peaky_blinder <- find_peak(local_cases)
    
    # recreate the incidence object using data only up to the
    # peak
    local_growth_phase_case_dates <- incidence_rates %>% 
      filter(time <= peaky_blinder) %>% 
      select(time, si.flow) %>% 
      uncount(si.flow) %>% pull(time)
    
    local_growth_phase_cases <- local_growth_phase_case_dates %>% 
      incidence(., last_date = peaky_blinder)
    
    # get a MLE estimate of the basic reproduction number, R0
    res <- get_R(local_growth_phase_cases, 
                 si_mean = si_mean, 
                 si_sd = si_sd)
    
    # add that as a column to the data frame of results
    incidence_rates <- incidence_rates %>% mutate(mle_R0 = res$R_ml)
  } else {
    # can't calculate R0 so just set to NA
    incidence_rates <- incidence_rates %>% mutate(mle_R0 = NA)
  }
  
  # return the data frame (which has just one row)
  return(incidence_rates)
}  # end function definition

# SIMULATION --------------------------------------------------------------

control <- control.icm(type = "SIR", nsteps = 100, nsims = 10)
# Condiciones iniciales
init <- init.icm(s.num = 997, i.num = 3, r.num = 0)
# Parametros
param <- param.icm(
  # Probability of infection per transmissible act between a susceptible and an 
  # infected person. 
  inf.prob = 0.05, 
  # Average number of transmissible acts per person per unit time.
  act.rate = 10, 
  # Average rate of recovery with immunity (in SIR models) or re-susceptibility 
  # (in SIS models)
  rec.rate = 1/20, 
  # Arrival or entry rate (new arrivals per person per unit time). 
  a.rate = (10.5/365)/1000, 
  # Departure or exit rate for susceptible.
  ds.rate = (7/365)/1000, 
  # Departure or exit rate for infected. 
  di.rate = (14/365)/1000, 
  # Departure or exit rate for recovered
  dr.rate = (7/365)/1000
)

sim <- icm(param, init, control)

sim

# Prevalences
plot(sim)

# Incidence
plot(sim, y = "si.flow", mean.col = "red", qnts.col = "red")



# R0 ----------------------------------------------------------------------

library(incidence)
library(earlyR)

incidence_counts <- as.data.frame(sim, out="mean") %>%
  select(time, si.flow)

incident_case_dates <- incidence_counts %>%
  uncount(si.flow) %>%
  pull(time) 

incidence_all <- incident_case_dates %>%
  incidence(.)

peak_of_epidemic_curve <- find_peak(incidence_all)

incident_case_dates_growth_phase <- incidence_counts %>%
  filter(time <= peak_of_epidemic_curve) %>%
  select(time, si.flow) %>%
  uncount(si.flow) %>%
  pull(time)

incidence_growth_phase <- incident_case_dates_growth_phase %>%
  incidence(., last_date=peak_of_epidemic_curve)

si_mean <- 7.5
si_sd <- 3.4
res <- get_R(incidence_growth_phase, si_mean = si_mean, si_sd = si_sd)

plot(res, "R")


# INTERVENTIONS -----------------------------------------------------------

# set up an empty data frame to which to append results from
# each simulation
sims_incidence_rates <- 
  tibble(time = integer(0), 
         si.flow = numeric(0), 
         i.num = numeric(0), 
         act_rate = numeric(0), 
         inf_prob = numeric(0), 
         total_cases = numeric(0),
         max_prev = numeric(0),
         mle_R0 = numeric(0))

# the parameters to step through
act.rates <- c(10, 5, 2)
inf.probs <- c(0.05, 0.025, 0.01)

# loop through the parameter space
for (act.rate in act.rates) {
  for (inf.prob in inf.probs) {
    sims_incidence_rates <- 
      sims_incidence_rates %>% bind_rows(run_sir_sim(inf.prob, act.rate))
  }
}

# create facet columns as descending ordered factors
sims_incidence_rates <- sims_incidence_rates %>% 
  mutate(act_rate_facet_label = paste(act_rate, "exposures per day"), 
         inf_prob_facet_label = paste("Probability of infection\nat each exposure:", 
                                      inf_prob)) %>% 
  arrange(desc(act_rate)) %>% 
  mutate_at(vars(act_rate_facet_label), 
            funs(factor(., levels = unique(.)))) %>% 
  arrange(desc(inf_prob)) %>% 
  mutate_at(vars(inf_prob_facet_label), 
            funs(factor(., levels = unique(.)))) %>% 
  arrange(desc(act_rate), desc(inf_prob), time)

# add annotation text for each facet
sims_incidence_rates_facet_annotations <- sims_incidence_rates %>% 
  mutate(label = paste("R0 =", format(mle_R0, digits = 3), 
                       "\n", round(100 * total_cases/1000, digits = 0), 
                       "% of population infected")) %>% 
  select(inf_prob_facet_label, act_rate_facet_label, label) %>% 
  distinct()

# Prevalences plot
sims_incidence_rates %>% filter(time <= 365) %>% 
  ggplot(aes(x = time, y = si.flow)) + 
  geom_line(colour = "blue", size = 1.5) + 
  facet_grid(inf_prob_facet_label ~ act_rate_facet_label) + 
  geom_text(data = sims_incidence_rates_facet_annotations, 
            mapping = aes(x = 50, 
                          y = 0.8 * max(sims_incidence_rates$si.flow,
                                        na.rm = TRUE), 
                          label = label), 
            parse = FALSE, 
            hjust = 0, 
            vjust = 0, 
            size = 3) +
  labs(x = "Days since start of epidemic", 
       y = "New cases per day", 
       title = "Modelling of new cases of COVID-19 per day: incidence rate", 
       subtitle = paste("with varying levels of social mixing (exposures per day)", 
                        "and probabilities of infection at each exposure")) + 
  theme(legend.position = "top", strip.text = element_text(size = 14))

# Incidences plot
sims_incidence_rates %>%
  filter(time <= 365) %>%
  ggplot(aes(x=time, y=i.num)) +
  geom_line(colour="orange", size=1.5) +
  facet_grid(inf_prob_facet_label ~ act_rate_facet_label) +
  geom_text(data=sims_incidence_rates_facet_annotations,
            mapping=aes(x = 50, 
                        y = 0.8*max(sims_incidence_rates$i.num, na.rm=TRUE),
                        label = label), parse=FALSE,
            hjust   = 0, vjust   = 0, size=3) +
  labs(x="Days since start of epidemic", y="Prevalent (current number of active) cases",
       title="Modelling of new cases of COVID-19 per day: case prevalence",
       subtitle=paste("with varying levels of social mixing (exposures per day)",
                      "and probabilities of infection at each exposure")) +
  theme(legend.position = "top", 
        strip.text = element_text(size=12)) 

# Alternative incidences
sims_incidence_rates %>%
  filter(time <= 365) %>%
  ggplot(aes(x=time, y=i.num, fill=act_rate_facet_label)) +
  geom_area(stat="identity", alpha=0.6) +
  facet_grid(inf_prob_facet_label ~ .) +
  labs(x="Days since start of epidemic", y="Prevalent (current number of active) cases",
       title="Modelling of prevalence of COVID-19 cases per day",
       subtitle=paste("with varying levels of social mixing (exposures per day)",
                      "and probabilities of infection at each exposure")) +
  theme_minimal() +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        strip.text = element_text(size=12)) +
  scale_fill_brewer(type="seq", palette="Oranges")

# Alternatives incidences II
library(ggridges)
# create one column for both intervention parameters
sims_incidence_rates <- sims_incidence_rates %>%
  mutate(intervention_level_label = paste(act_rate,
                                          "exp/day,",
                                          inf_prob*100,
                                          "% inf risk/exp")) %>%
  arrange(max_prev, time) %>%
  mutate_at(vars(intervention_level_label), funs(factor(., levels=unique(.), ordered=TRUE)))
sims_incidence_rates %>%
  filter(time <= 365) %>%
  ggplot(aes(x=time, y=intervention_level_label, height=i.num, fill=intervention_level_label)) +
  geom_density_ridges(stat="identity", show.legend=FALSE) +
  labs(x="Days since start of epidemic", y="Prevalent (current number of active) cases",
       title="Modelling of COVID-19 transmission in 1,000 simulated people",
       subtitle=paste("with varying levels of social mixing (exposures per day)",
                      "and risk of infection at each exposure,\n",
                      "ordered by descending maximum number of prevalent cases per day")) +
  theme_minimal() +
  theme(legend.position = "top", 
        strip.text = element_text(size=12)) +
  scale_fill_brewer(type="seq", palette="Oranges")


## Does simulation size matter?

# set up an empty data frame to which to append 
# results from each simulation 
big_sims_incidence_rates <- tibble(time=integer(0),
                                   si.flow=numeric(0),
                                   i.num=numeric(0),
                                   act_rate=numeric(0),
                                   inf_prob=numeric(0),
                                   total_cases=numeric(0),
                                   max_prev=numeric(0),
                                   mle_R0=numeric(0))
# the parameters to step through
act.rates <- c(10, 5, 2)
inf.probs <- c(0.05, 0.035, 0.02)
# loop through the parameter space
for (act.rate in act.rates) {
  for (inf.prob in inf.probs) {
    big_sims_incidence_rates <- big_sims_incidence_rates %>%
      bind_rows(run_sir_sim(inf.prob, act.rate,
                            pop_size=100000))
  }
}


# create one column for both intervention parameters
big_sims_incidence_rates <- big_sims_incidence_rates %>%
  mutate(intervention_level_label = paste(act_rate,
                                          "exp/day,",
                                          inf_prob*100,
                                          "% inf risk/exp")) %>%
  arrange(max_prev, time) %>%
  mutate_at(vars(intervention_level_label), funs(factor(., levels=unique(.), ordered=TRUE)))
big_sims_incidence_rates %>%
  filter(time <= 365) %>%
  ggplot(aes(x=time, y=intervention_level_label, height=i.num, fill=intervention_level_label)) +
  geom_density_ridges(stat="identity", show.legend=FALSE) +
  labs(x="Days since start of epidemic", y="Prevalent (current number of active) cases",
       title="Modelling of COVID-19 transmission in 100,000 simulated people",
       subtitle=paste("with varying levels of social mixing (exposures per day)",
                      "and risk of infection at each exposure,\n",
                      "ordered by descending maximum number of prevalent cases per day")) +
  theme_minimal() +
  theme(legend.position = "top", 
        strip.text = element_text(size=12)) +
  scale_fill_brewer(type="seq", palette="Oranges")
