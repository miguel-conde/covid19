
# LIBRARIES and SOURCES ---------------------------------------------------
library(tvReg)

source("get_clean_data.R", encoding = "UTF8")
source("utils.R", encoding = "UTF8")

# CONSTANTS ---------------------------------------------------------------

# AUXILIARY FUNCTIONS -----------------------------------------------------

# COUNTRIES ---------------------------------------------------------------

clean_data_list <- get_jhu_clean_data()

china_data <- get_cntry_region_ttss("China", 
                                    clean_data_list = clean_data_list, 
                                    pop_data = wld_pop_data)

s_korea_data <- get_cntry_region_ttss("Korea, South", 
                                      clean_data_list = clean_data_list, 
                                      pop_data = wld_pop_data)

spain_data <- get_cntry_region_ttss("Spain", 
                                    clean_data_list = clean_data_list, 
                                    pop_data = wld_pop_data)

italy_data <- get_cntry_region_ttss("Italy", 
                                    clean_data_list = clean_data_list,
                                    pop_data = wld_pop_data)

plot(italy_data %>% select(date, confirmed, deaths, recovered, active))

plot(italy_data %>% select(date, confirmed), type = "l")

lines(spain_data %>% select(date, confirmed), type = "l", col = "blue")


spain_last_data <- spain_data %>% tail(1)
italy_past <- italy_data %>%
  filter(confirmed < (spain_last_data %>% pull(confirmed))) %>%
  tail(1) %>%
  pull(date)

spain_last_date <- spain_last_data %>% pull(date)

delay_spain_italy <- italy_past - spain_last_date

delay_spain_italy

Conento::descriptivos_n_variables(china_data %>% select(-Lat, -Long))
Conento::descriptivos_n_variables(italy_data %>% select(-Lat, -Long))
Conento::descriptivos_n_variables(spain_data %>% select(-Lat, -Long))
Conento::descriptivos_n_variables(s_korea_data %>% select(-Lat, -Long))

china_data_per_100K <- get_cntry_region_ttss("China", 
                                              clean_data_list = clean_data_list, 
                                              pop_data = wld_pop_data,
                                              per_100K = TRUE)

spain_data_per_100K <- get_cntry_region_ttss("Spain", 
                                              clean_data_list = clean_data_list, 
                                              pop_data = wld_pop_data,
                                              per_100K = TRUE)

italy_data_per_100K <- get_cntry_region_ttss("Italy", 
                                              clean_data_list = clean_data_list,
                                              pop_data = wld_pop_data,
                                              per_100K = TRUE)

Conento::descriptivos_n_variables(china_data_per_100K %>% select(-Lat, -Long))
Conento::descriptivos_n_variables(italy_data_per_100K %>% select(-Lat, -Long))
Conento::descriptivos_n_variables(spain_data_per_100K %>% select(-Lat, -Long))

spain_last_data_per_100K <- spain_data_per_100K %>% tail(1)
italy_past_per_100K <- italy_data_per_100K %>%
  filter(confirmed < (spain_last_data %>% pull(confirmed))) %>%
  tail(1) %>%
  pull(date)

spain_last_date_per_100K <- spain_last_data_per_100K %>% pull(date)

delay_spain_italy_per_100K <- italy_past_per_100K - spain_last_date_per_100K

delay_spain_italy_per_100K


# ESPAÑA ------------------------------------------------------------------

clean_datos_min <- get_sp_clean_data()


datos_fallecidos <- 
  datos_min_ccaa_col(clean_datos_min, fallecidos)
datos_fallecidos_var_units <- 
  datos_min_ccaa_col(clean_datos_min, fallecidos, "units")

datos_fallecidos_per_100K <- 
  datos_min_ccaa_col(clean_datos_min, fallecidos, per_100K = TRUE)
datos_fallecidos_per_100K_var_units <- 
  datos_min_ccaa_col(clean_datos_min, fallecidos, "perc", per_100K = TRUE)

datos_fallecidos_var_perc <- datos_min_ccaa_col(clean_datos_min, fallecidos, "perc")

datos_fallecidos_var_units

spain_deaths <- datos_fallecidos %>%
  select(fecha, fallecidos = "ES")

datos_min_ccaa(clean_datos_min, "ES")

### pLOTS
### 
hc_min_ccaa(clean_datos_min, "MD", c("casos", 
                                     "fallecidos", 
                                     "altas",
                                     "activos",
                                     "hospitalizados",
                                     "uci"))
hc_min_ccaa(clean_datos_min, "MD", c("casos_per_100K", 
                         "fallecidos_per_100K", 
                         "altas_per_100K",
                         "activos_per_100K",
                         "hospitalizados_per_100K",
                         "uci_per_100K"))
hc_min_ccaa(clean_datos_min, "MD", c("casos_var_perc", 
                                   "fallecidos_var_perc", 
                                   "altas_var_perc",
                                   "activos_var_perc",
                                   "hospitalizados_var_perc",
                                   "uci_var_perc"))

hc_min_ccaa_col(clean_datos_min, fallecidos, per_100K = FALSE)
hc_min_ccaa_col(clean_datos_min, fallecidos, per_100K = TRUE)
hc_min_ccaa_col(clean_datos_min, fallecidos, per_100K = TRUE, orig_t = 5)


# SPEED -------------------------------------------------------------------

aux <- spain_data %>% mutate(new_confirmed = c(NA, diff(log(confirmed))),
                             new_active = c(NA, diff(log(active))),
                             new_deaths = c(NA, diff(log(deaths)))) %>% 
  filter(date > as.Date("2020-02-15"))

aux %>% select(date, new_confirmed) %>% plot(type = "l")
aux_lm <- lm(new_confirmed ~ date, aux %>% filter(!is.infinite(new_confirmed)))
summary(aux_lm)
abline(aux_lm, col = "blue")

aux %>% select(date, new_active) %>% plot(type = "l")
aux_lm <- lm(new_active ~ date, aux %>% filter(!is.infinite(new_active)))
summary(aux_lm)
abline(aux_lm, col = "blue")

aux %>% select(date, new_deaths) %>% plot(type = "l")
aux_lm <- lm(new_deaths ~ date, aux %>% filter(!is.infinite(new_deaths)))
summary(aux_lm)
abline(aux_lm, col = "blue")


# FROM 1st DEATH ----------------------------------------------------------

china_pruned_data <- china_data %>% 
  filter(active > 100) # %>% 
# select(active) 
china_pruned_data <- china_pruned_data %>% 
  mutate(n_day = 1:nrow(china_pruned_data))

italy_pruned_data <- italy_data %>% 
  filter(active > 100) # %>% 
# select(active) 
italy_pruned_data <- italy_pruned_data %>% 
  mutate(n_day = 1:nrow(italy_pruned_data))

spain_pruned_data <- spain_data %>% 
  filter(active > 100) # %>% 
# select(active) 
spain_pruned_data <- spain_pruned_data %>% 
  mutate(n_day = 1:nrow(spain_pruned_data))


china_pruned_data %>% 
  full_join(italy_pruned_data, by = "n_day", suffix = c("_ch", "_it") )%>% 
  full_join(spain_pruned_data, by = "n_day")

aux <- italy_pruned_data %>% 
  full_join(spain_pruned_data, by = "n_day", suffix = c("_it", "_sp") )

library(ggplot2)
aux %>% ggplot(aes(x = n_day)) + 
  geom_line(aes(y = active_it, colour = "active_it")) + 
  geom_line(aes(y = active_sp, colour = "active_sp")) +
  xlab("Días tras 100 activos") + 
  ylab("Enfermos activos")

aux %>% ggplot(aes(x = n_day)) + 
  geom_line(aes(y = deaths_it, colour = "deaths_it")) + 
  geom_line(aes(y = deaths_sp, colour = "deaths_sp")) +
  xlab("Días tras 100 activos") + 
  ylab("Muertes")


# DIFFs -------------------------------------------------------------------

spain_data <- spain_data %>% 
  mutate_at(vars(confirmed, deaths, recovered, active),
            list(new = ~ c(NA, diff(.))))

p <- spain_data %>% ggplot(aes(x = date)) + 
  geom_line(aes(y = deaths_new, colour = "deaths_new")) + 
  geom_line(aes(y = recovered_new, colour = "recovered_new"))  + 
  geom_line(aes(y = active_new, colour = "active_new")) +
  xlab("Fecha") + 
  ylab("Nº")
p

# p + geom_line(aes(x = seq.Date(from = as.Date("2020-03-13"), 
#                                length.out = 8,
#                                by = 1),
#                   y = 15000/36*(1:8) + 582),
#               colour = "black")


# INE ---------------------------------------------------------------------

# https://www.ine.es/jaxiT3/Tabla.htm?t=14819&L=0


# ESTIMATING EXP ----------------------------------------------------------

data_exp <- spain_deaths %>% 
  filter(fallecidos > 0) 
data_exp <- data_exp %>% 
  mutate(n_day = 1:nrow(data_exp))

sp_lm <- lm(log(fallecidos) ~ n_day, data_exp)
summary(sp_lm)
plot(sp_lm)

plot(data_exp %>% select(n_day, deaths), type = "l")
k <- coef(sp_lm)[1]
a <- exp(k)
lines(data_exp$n_day, a^data_exp$n_day, col = "blue", lty = 2)
lines(data_exp$n_day, exp(k*data_exp$n_day), col = "red", lty = 3)


tvp_sp_lm <- tvLM(log(deaths) ~ n_day, 
                  data = data_exp)
summary(tvp_sp_lm)
plot(tvp_sp_lm)


plot(data_exp %>% select(n_day, deaths), type = "l")
k_t <- coef(tvp_sp_lm)[,2]
beta_0 <- coef(tvp_sp_lm)[,1]
lines(data_exp$n_day, exp(beta_0) * exp(k_t*data_exp$n_day), col = "red", lty = 3)

plot(1:length(k_t), log(2)/k_t, type = "l",
     # ylim = c(0, 2),
     main = "Days to double deaths",
     xlab = "Days",
     ylab = "Days to double deaths")

##Obtain the 90% confidence interval of the coefficients for an object of the class attribute tvlm
model.tvLM.90 <- confint (tvp_sp_lm, level = 0.90, runs = 50)

##Obtain the 95% confidence interval of the same object. This will reused the resamples of object model.tvLM.90. So the second confidence interval calculation is faster
model.tvLM.95 <- confint(model.tvLM.90)

##Plot coefficient estimates and confidence intervals (if calculated) of objects of the class attribute tvlm
plot(model.tvLM.90)
plot(model.tvLM.95)

fcst_n_day <- nrow(data_exp) + (1:10)
fcst_date <- (data_exp %>% tail(1) %>% pull(date)) + (1:10)
fcst_deaths <- forecast(tvp_sp_lm, 
                        n.ahead = 10, 
                        newx = matrix(fcst_n_day, byrow = FALSE)) %>% 
  exp()
fcst <- tibble(date = fcst_date, n_day = fcst_n_day, deaths = fcst_deaths)

data_exp %>% select(n_day, deaths) %>% 
  plot(type = "l", xlim = c(0,30), ylim = c(0,10000))

lines(fcst %>% select(n_day, deaths), lty = 2, col = "red", type = "o")

aux <- bind_rows(data_exp %>% select(date, n_day, deaths),
                 fcst) %>% 
  mutate(new_deaths = c(0, diff(deaths)))

plot(aux %>% select(date, new_deaths), type = "o")


# EST EXP II --------------------------------------------------------------

tvp_sp_lm <- tvLM(log(fallecidos) ~ log(n_day), 
                  data = data_exp)
summary(tvp_sp_lm)
plot(tvp_sp_lm)


plot(data_exp %>% select(n_day, fallecidos), type = "l")
k_t <- coef(tvp_sp_lm)[,2]
beta_0 <- coef(tvp_sp_lm)[,1]
lines(data_exp$n_day, exp(beta_0) * data_exp$n_day^k_t, 
      col = "red", lty = 3)

plot(1:length(k_t), (2^(1/k_t) - 1) * data_exp$n_day, type = "l",
     # ylim = c(0, 2),
     main = "Days to double deaths",
     xlab = "Days",
     ylab = "Days to double deaths")

fcst_n_day <- nrow(data_exp) + (1:10)
fcst_date <- (data_exp %>% tail(1) %>% pull(fecha)) + (1:10)
fcst_deaths <- forecast(tvp_sp_lm, 
                        n.ahead = 10, 
                        newx = matrix(log(fcst_n_day), byrow = FALSE)) %>% 
  exp()
fcst <- tibble(fecha = fcst_date, n_day = fcst_n_day, fallecidos = fcst_deaths)

data_exp %>% select(fecha, fallecidos) %>% 
  plot(type = "l", xlim = c(min(data_exp$fecha), max(fcst$fecha)), 
       ylim = c(0, max(fcst$fallecidos)))

lines(fcst %>% select(fecha, fallecidos), lty = 2, col = "red", type = "o")

aux <- bind_rows(data_exp %>% select(fecha, n_day, fallecidos),
                 fcst) %>% 
  mutate(new_deaths = c(0, diff(fallecidos)))

plot(aux %>% select(fecha, new_deaths), type = "o")

# dy / dt
dy_dt <- c(NA, diff(k_t))*(1:length(k_t)) + k_t + c(NA, diff(beta_0))
dy_dt <- dy_dt * data_exp$fallecidos
plot(dy_dt, type = "l")
abline(h = 0, lty = 2)
