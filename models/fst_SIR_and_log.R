# https://timchurches.github.io/blog/posts/2020-02-18-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-1/#fitting-an-sir-model-to-the-hubei-province-data

# LIBRARIES and SOURCES ---------------------------------------------------

source("get_clean_data.R", encoding = "UTF8")
source("utils.R", encoding = "UTF8")

library(lubridate)

# AUXILIARY FUNCTIONS -----------------------------------------------------


sir_model <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta_x_C * I * S/N
    dI <- beta_x_C * I * S/N - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

# define a function to calculate the residual sum of squares
# (RSS), passing in parameters beta and gamma that are to be
# optimised for the best fit to the incidence data
RSS <- function(parameters) {
  names(parameters) <- c("beta_x_C", "gamma")
  out <- ode(y = init, times = n_dia, func = sir_model, parms = parameters)
  fit <- out[, 3]
  sum((infectados - fit)^2)
}


clean_datos_min <- get_sp_clean_data()


datos_incidencia <- 
  datos_min_ccaa(clean_datos_min, "ES") %>% 
  select(fecha, casos, activos)

# put the daily cumulative incidence numbers for Hubei from
# 15th Jan to 30th Jan into a vector called Infected
sir_start_date <- min(datos_incidencia$fecha)
sir_end_date <- sir_start_date + 15

infectados <- datos_incidencia %>% filter(fecha >= sir_start_date, 
                                          fecha <= sir_end_date) %>% 
  pull(casos)

# % infectados  detectados
# ascertainment_rate <- .2
ascertainment_rate <- 1
infectados <- infectados / ascertainment_rate


# Create an incrementing Day vector the same length as our
# cases vector
n_dia <- 1:(length(infectados))

# now specify initial values for S, I and R
N <- 47500000
init <- c(S = N - infectados[1], I = infectados[1], R = 0)

# now find the values of beta and gamma that give the
# smallest RSS, which represents the best fit to the data.
# Start with values of 0.5 for each, and constrain them to
# the interval 0 to 1.0
Opt <- optim(c(0.5, 0.5), RSS, 
             method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1))

# check for convergence
Opt$message

Opt_par <- setNames(Opt$par, c("beta_x_C", "gamma"))
Opt_par


# time in days for predictions
t <- 1:as.integer(today() - ymd(sir_start_date))
# get the fitted values from our SIR model
fitted_cumulative_incidence <- data.frame(ode(y = init, 
                                              times = t, 
                                              func = sir_model, 
                                              parms = Opt_par))

# add a Date column and join the observed incidence data
fitted_cumulative_incidence <- fitted_cumulative_incidence %>% 
  mutate(fecha = ymd(sir_start_date) + days(t - 1)) %>% 
  left_join(datos_incidencia, by = "fecha") # %>% 
  # select(fecha, casos)

# plot the data
fitted_cumulative_incidence %>% filter(fecha <= sir_end_date) %>% 
  ggplot(aes(x = fecha)) + 
  geom_line(aes(y = I), colour = "red") + 
  geom_point(aes(y = casos), colour = "orange") + 
  labs(y = "Cumulative incidence", 
       title = "COVID-19 fitted vs observed cumulative incidence, Hubei province", 
       subtitle = "(red=fitted incidence from SIR model, orange=observed incidence)")

# ??
R0 <- Opt_par["beta_x_C"] / Opt_par["gamma"] %>% as.numeric
R0

## PREDICCIONES
fitted_cumulative_incidence %>%
  ggplot(aes(x=fecha)) + geom_line(aes(y=I), colour="red") +
  geom_point(aes(y=casos), colour="orange") +
  scale_y_continuous(labels = scales::comma) +
  labs(y="Cumulative incidence", 
       title="COVID-19 fitted vs observed cumulative incidence, Hubei province",
       subtitle="(red=fitted incidence from SIR model, orange=observed incidence)")

# time in days for predictions
t <- 1:70
# get the fitted values from our SIR model
fitted_cumulative_incidence <- data.frame(ode(y = init, 
                                              times = t, 
                                              func = sir_model, 
                                              parms = Opt_par))

# add a Date column and join the observed incidence data
fitted_cumulative_incidence <- fitted_cumulative_incidence %>% 
  mutate(fecha = ymd(sir_start_date) + days(t - 1)) %>% 
  left_join(datos_incidencia, by = "fecha") #%>% 
  # select(fecha, casos)
# plot the data
fitted_cumulative_incidence %>% 
  ggplot(aes(x = fecha)) + 
  geom_line(aes(y = I),  colour = "red") + 
  geom_line(aes(y = S), colour = "black") + 
  geom_line(aes(y = R), colour = "green") + 
  geom_point(aes(y = casos), colour = "orange") + 
  scale_y_continuous(labels = scales::comma) + 
  labs(y = "Persons", 
       title = "COVID-19 fitted vs observed cumulative incidence, Hubei province") + 
  scale_colour_manual(name = "", 
                      values = c(red = "red", black = "black", 
                                 green = "green", orange = "orange"), 
                      labels = c("Susceptible", "Recovered", 
                                 "Observed incidence", "Infectious"))

fitted_cumulative_incidence %>%
  ggplot(aes(x = fecha)) + 
  geom_line(aes(y = I),  colour = "red") + 
  geom_line(aes(y = S), colour = "black") + 
  geom_line(aes(y = R), colour = "green") + 
  geom_point(aes(y = casos), colour = "orange") +
  scale_y_log10(labels = scales::comma) +
  labs(y = "Persons", 
       title = "COVID-19 fitted vs observed cumulative incidence, Hubei province") + 
  scale_colour_manual(name = "", 
                      values = c(red = "red", black = "black", 
                                 green = "green", orange = "orange"), 
                      labels = c("Susceptible", "Recovered", 
                                 "Observed incidence", "Infectious"))


# Log linear models
library(incidence)

ES_incidence_function_data <- 
  fitted_cumulative_incidence %>% 
  select(fecha, casos) %>% 
  mutate(casos = c(NA, diff(casos))) %>% 
  drop_na() %>%  
  uncount(casos)

ES_incidence_object <- incidence(ES_incidence_function_data$fecha)

ES_incidence_peak <- find_peak(ES_incidence_object)
plot(ES_incidence_object) + 
  geom_vline(xintercept = ES_incidence_peak, col = "red", lty = 2) +
  labs(title="Daily incidence of lab-confirmed cases, Hubei province",
       subtitle = "(red line indicates date of peak incidence)")


ES_incidence_fit <- incidence::fit(ES_incidence_object, 
                                   split = ES_incidence_peak)

# plot the incidence data and the model fit
plot(ES_incidence_object) %>% add_incidence_fit(ES_incidence_fit) + 
  labs(title = "Observed and modelled incidence of COVID-19 cases", 
       subtitle = "Hubei province, 2020")

incidence::get_info(ES_incidence_fit)

# growth rate before and decay rate after the peak
incidence::get_info(ES_incidence_fit, "r")
# 95% CIs
incidence::get_info(ES_incidence_fit, "r.conf")

# doubling time before and halving time after the peak
incidence::get_info(ES_incidence_fit, "doubling")
incidence::get_info(ES_incidence_fit, "doubling.conf")

incidence::get_info(ES_incidence_fit, "halving")
incidence::get_info(ES_incidence_fit, "halving.conf")

library(distcrete)
library(epitrix)

mu <- 7.5  # days
sigma <- 3.4  # days
param <- gamma_mucv2shapescale(mu, sigma/mu)

w <- distcrete("gamma", interval = 1, shape = param$shape, scale = param$scale, 
               w = 0)

growth_R0 <- lm2R0_sample(ES_incidence_fit$before$model, w)
hist(growth_R0, col = "grey", border = "white", main = "Distribution of R0")

decay_R0 <- lm2R0_sample(ES_incidence_fit$after$model, w)
hist(decay_R0, col = "grey", border = "white", main = "Distribution of R0")

library(EpiEstim)
# custom results plotting function to avoid the ugly
# TableGrob messages returned by the plotting function in the
# EpiEstim package
plot_Ri <- function(estimate_R_obj) {
  p_I <- plot(estimate_R_obj, "incid", add_imported_cases = TRUE)  # plots the incidence
  p_SI <- plot(estimate_R_obj, "SI")  # plots the serial interval distribution
  p_Ri <- plot(estimate_R_obj, "R")
  return(gridExtra::grid.arrange(p_I, p_SI, p_Ri, ncol = 1))
}

ES_confirmed_cases <- datos_incidencia  %>%
  mutate(casos = c(NA, diff(casos))) %>% 
  mutate(imported=ifelse(fecha == sir_start_date+1, casos, 0),
         local=ifelse(fecha == sir_start_date+1, 0, casos)) %>%
  select(fecha, local, imported) %>%
  rename(dates=fecha) %>% 
  drop_na()

ES_res_parametric_si <- estimate_R(ES_confirmed_cases,
                                      method = "parametric_si",
                                   config = make_config(list(mean_si = 7.5,
                                                             std_si = 3.4)))
# ES_res_parametric_si <- estimate_R(ES_confirmed_cases, 
#                                    method = "parametric_si", 
#                                    config = make_config(list(mean_si = 2.3, 
#                                                              std_si = 1.4)))
# ES_res_parametric_si <- estimate_R(ES_confirmed_cases, 
#                                    method = "parametric_si", 
#                                    config = make_config(list(mean_si = 1/Opt_par["beta_x_C"], 
#                                                              std_si = 1)))

plot_Ri(ES_res_parametric_si)

ES_res_parametric_si$R$`Mean(R)` %>% mean


ES_res_uncertain_si <- estimate_R(ES_confirmed_cases, 
                                   method = "uncertain_si", 
                                   config = make_config(list(mean_si = 7.5, 
                                                             std_mean_si = 2, 
                                                             min_mean_si = 1, 
                                                             max_mean_si = 8.4, 
                                                             std_si = 3.4, 
                                                             std_std_si = 1, 
                                                             min_std_si = 0.5, 
                                                             max_std_si = 4, 
                                                             n1 = 1000, 
                                                             n2 = 1000)))

plot_Ri(ES_res_uncertain_si)


library(projections)

set.seed(1)
pred_fwd_days <- 10
date_range <- 1:(which(get_dates(ES_incidence_object) == ES_incidence_peak) - 
                   pred_fwd_days)

test_pred_growth <- project(ES_incidence_object[date_range], 
                            R = median(growth_R0), 
                            si = w, 
                            n_days = pred_fwd_days, 
                            n_sim = 1000)

# convert the test_pred_growth matrix to a data frame and get
# the median incidence for all the simulations for each date
test_pred_growth_median_counts <- test_pred_growth %>% 
  as.data.frame() %>% 
  pivot_longer(-dates, names_to = "simulation", values_to = "incidence") %>% 
  group_by(dates) %>% summarise(incident_cases = as.integer(median(incidence))) %>% 
  mutate(data_type = "projection")

test_pred_growth_median_counts %>% 
  bind_rows(tibble(dates = get_dates(ES_incidence_object),
                   incident_cases = get_counts(ES_incidence_object), 
                   data_type = "observed")) %>% 
  ggplot(aes(x = dates, y = incident_cases, colour = data_type)) + 
  geom_point() + 
  geom_line() + 
  labs(x = "", 
       y = "Daily incident confirmed cases", 
       title = "Observed versus growth-phase projection of incident cases\nin Hubei province",
       subtitle = paste("(projection based on observed case counts up to", 
                        format(ES_incidence_peak - days(pred_fwd_days), 
                               "%d %B %Y"), ")")) + 
  theme(legend.position = "top", legend.title = element_blank())



set.seed(1)
pred_fwd_days <- 0 # 5

date_range <- which(get_dates(ES_incidence_object) == ES_incidence_peak):(length(get_dates(ES_incidence_object)) - pred_fwd_days)

date_range <- which(get_dates(ES_incidence_object) == ES_incidence_peak):(length(get_dates(ES_incidence_object)) - pred_fwd_days)

test_pred_decay <- project(ES_incidence_object[date_range],
                           R = median(decay_R0),
                           si = w,
                           n_days = 60, n_sim = 1000)

# convert the test_pred_decay matrix to a data frame and get the median 
# incidence for all the simulations for each date
test_pred_decay_median_counts <- test_pred_decay %>% 
  as.data.frame() %>%
  pivot_longer(-dates, 
               names_to="simulation", 
               values_to="incidence") %>%
  group_by(dates) %>%
  summarise(incident_cases=as.integer(median(incidence))) %>%
  mutate(data_type = "projection")

test_pred_decay_median_counts %>%
  bind_rows(tibble(dates=get_dates(ES_incidence_object),
                   incident_cases=get_counts(ES_incidence_object),
                   data_type="observed")) %>%
  ggplot(aes(x=dates, y=incident_cases, colour=data_type)) +
  geom_point() +
  geom_line() +
  labs(x="", y="Daily incident confirmed cases",
       title="Observed versus decay-phase projection of incident cases\nin ES province",
       subtitle=paste("(projection based on observed case counts in decay phase up to", 
                      format(get_dates(ES_incidence_object)[(length(get_dates(ES_incidence_object)) - pred_fwd_days)], "%d %B %Y"),
                      ")")) +
  theme(legend.position="top", legend.title = element_blank())
