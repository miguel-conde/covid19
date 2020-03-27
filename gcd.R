
# LIBRARIES and SOURCES ---------------------------------------------------
library(tvReg)
library(tidyverse)

source("utils.R")

# CONSTANTS ---------------------------------------------------------------


WORLD_POP_URL <- "http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv"

# POP_FILE <- "API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv"
POP_FILE <- "API_SP.POP.TOTL_DS2_en_csv_v2_887275.csv"

# CONFIRMED_TS_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
# DEATHS_TS_URL    <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
# RECOVERED_TS_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
CONFIRMED_TS_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
DEATHS_TS_URL    <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
RECOVERED_TS_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

# MINISTERIO SANIDAD
URL_MIN <- "https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/documentos/Actualizacion_XX_COVID-19.pdf" 
CSV_MIN <- "https://covid19.isciii.es/resources/serie_historica_acumulados.csv"

# AUXILIARY FUNCTIONS -----------------------------------------------------

library(tibbletime)

roll_IA_14 <- rollify(function(x) sum(diff(x)), window = 14)
roll_mean_14 <- rollify(mean, window = 14)

get_cntry_region_ttss <- function(cntry_reg,
                                  raw_data_list,
                                  prov_st = NULL,
                                  pop_data = NULL,
                                  perc_100K = FALSE) {
  # browser()
  ts_cofirmed <- raw_data_list[["confirmed_ts"]] %>%
    filter(country_region == cntry_reg)
  ts_deaths <- raw_data_list[["deaths_ts"]] %>%
    filter(country_region == cntry_reg)
  ts_recovered <- raw_data_list[["recovered_ts"]] %>%
    filter(country_region == cntry_reg)
  
  if (!is.null(prov_st)) {
    ts_cofirmed <- ts_cofirmed %>%
      filter(province_state == prov_st)
    
    ts_deaths <- ts_deaths %>%
      filter(country_region == prov_st)
    
    ts_recovered <- ts_recovered %>%
      filter(country_region == prov_st)
  }
  
  ts_cofirmed <- ts_cofirmed %>%
    gather("date", "confirmed",
           -c("province_state", "country_region", "Lat", "Long")) %>%
    mutate(date = lubridate::mdy(date))
  
  ts_deaths <- ts_deaths %>%
    gather("date", "deaths",
           -c("province_state", "country_region", "Lat", "Long")) %>%
    mutate(date = lubridate::mdy(date))
  
  ts_recovered <- ts_recovered %>%
    gather("date", "recovered",
           -c("province_state", "country_region", "Lat", "Long")) %>%
    mutate(date = lubridate::mdy(date))
  
  out <- ts_cofirmed %>% full_join(ts_deaths) %>% 
    full_join(ts_recovered) %>% 
    mutate(active = confirmed - deaths - recovered)
  
  if(!is.null(pop_data)) {
    out <- out %>% 
      full_join(pop_data, by = c("country_region" = "country_name"))
  }
  
  if(is.null(prov_st)) {
    if(!is.null(pop_data)) {
      out <- out %>% 
        group_by(date) %>% 
        summarise(Lat = mean(Lat),
                  Long = mean(Long),
                  confirmed = sum(confirmed),
                  deaths = sum(deaths),
                  recovered = sum(recovered),
                  active = sum(active),
                  pop = mean(x2018)) %>% 
        ungroup()
    } else {
      out <- out %>% 
        group_by(date) %>% 
        summarise(Lat = mean(Lat),
                  Long = mean(Long),
                  confirmed = sum(confirmed),
                  deaths = sum(deaths),
                  recovered = sum(recovered),
                  active = sum(active)) %>% 
        ungroup()
    }
    
  } 
  
  if(!is.null(pop_data)) {
    out <- out %>%
      mutate(IA_14 = 
               (roll_IA_14(confirmed) - 
                  roll_IA_14(deaths) - 
                  roll_IA_14(recovered)) / pop * 1e5) %>% 
      drop_na
    
    if(perc_100K == TRUE) {
      out <- out %>% 
        mutate_at(vars(confirmed, deaths, recovered, active), 
                  ~ .x/pop*100000)
    }
    
    out <- out %>% select(-pop)  %>% 
      mutate_at(vars(confirmed, deaths, recovered, active),
                list(new = ~ c(NA, diff(.))))
  }
  
  out
}

# WORLD POP DATA ----------------------------------------------------------

temp <- tempfile(fileext = ".zip")
download.file(WORLD_POP_URL, temp, mode="wb")
unzip(temp, POP_FILE)
mydata <- read_csv(POP_FILE, skip = 3)
mydata <- mydata %>% mutate(X65 = NULL) %>% 
  janitor::clean_names() %>% 
  drop_na(x2018) %>% 
  select(country_name, x2018)
unlink(temp)
unlink(POP_FILE)

# COV19 DATA --------------------------------------------------------------


confirmed_ts <- read_csv(file = CONFIRMED_TS_URL)
deaths_ts    <- read_csv(file = DEATHS_TS_URL)
recovered_ts <- read_csv(file = RECOVERED_TS_URL)

raw_data_list <- list(confirmed_ts = confirmed_ts,
                      deaths_ts = deaths_ts,
                      recovered_ts = recovered_ts)

raw_data_list <- raw_data_list %>% 
  lapply(function(x) {
    x %>% rename_if(is.character, janitor::make_clean_names)
  })


# COUNTRIES ---------------------------------------------------------------

china_data <- get_cntry_region_ttss("China", 
                                    raw_data_list = raw_data_list, 
                                    pop_data = mydata)

spain_data <- get_cntry_region_ttss("Spain", 
                                    raw_data_list = raw_data_list, 
                                    pop_data = mydata)

italy_data <- get_cntry_region_ttss("Italy", 
                                    raw_data_list = raw_data_list,
                                    pop_data = mydata)

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

china_data_perc_100K <- get_cntry_region_ttss("China", 
                                              raw_data_list = raw_data_list, 
                                              pop_data = mydata,
                                              perc_100K = TRUE)

spain_data_perc_100K <- get_cntry_region_ttss("Spain", 
                                              raw_data_list = raw_data_list, 
                                              pop_data = mydata,
                                              perc_100K = TRUE)

italy_data_perc_100K <- get_cntry_region_ttss("Italy", 
                                              raw_data_list = raw_data_list,
                                              pop_data = mydata,
                                              perc_100K = TRUE)

spain_last_data_perc_100K <- spain_data_perc_100K %>% tail(1)
italy_past_perc_100K <- italy_data_perc_100K %>%
  filter(confirmed < (spain_last_data %>% pull(confirmed))) %>%
  tail(1) %>%
  pull(date)

spain_last_date_perc_100K <- spain_last_data_perc_100K %>% pull(date)

delay_spain_italy_perc_100K <- italy_past_perc_100K - spain_last_date_perc_100K

delay_spain_italy_perc_100K


# ESPAÑA ------------------------------------------------------------------

# datos <- get_reports_lst()
# raw_datos_min <- datos %>% map_dfr(~ .x, .id = "date") %>%
#   mutate(date = as.Date(date)) %>%
#   as_tibble
datos <- read.csv2(CSV_MIN, sep = ",", 
                   stringsAsFactors = FALSE) %>% 
  janitor::clean_names() %>% 
  mutate(fecha = lubridate::dmy(fecha))


raw_datos_min <- CCAA_CODIGO_ISO %>% 
  full_join(datos,
            by = "ccaa_codigo_iso")  %>% 
  filter(ccaa %in% CCAA_CODIGO_ISO$ccaa) %>% 
  mutate(ccaa = ifelse(is.na(ccaa),
                       "ESPAÑA",
                       ccaa),
         ccaa_codigo_iso = ifelse(is.na(ccaa_codigo_iso),
                                  "ES",
                                  ccaa_codigo_iso)) 

datos_min <- raw_datos_min %>% 
  select(fecha, ccaa, fallecidos) %>% 
  spread("ccaa", "fallecidos")
datos_min <- datos_min %>% 
  mutate(`ESPAÑA` = rowSums(datos_min %>% select_if(is.numeric)))

res_impute <- datos_min %>% 
  right_join(tibble(fecha = seq(from = min(datos_min$fecha),
                               to = max(datos_min$fecha),
                               by = 1)),
             by = "fecha")

# impute()

spain_deaths <- res_impute %>%
  select(fecha, fallecidos = "ESPAÑA")

# SPEED -------------------------------------------------------------------

aux <- spain_data %>% mutate(new_confirmed = c(NA, diff(log(confirmed))),
                             new_active = c(NA, diff(log(active))),
                             new_deaths = c(NA, diff(log(deaths))))

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
  filter(deaths > 0) 
data_exp <- data_exp %>% 
  mutate(n_day = 1:nrow(data_exp))

sp_lm <- lm(log(deaths) ~ n_day, data_exp)
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

tvp_sp_lm <- tvLM(log(deaths) ~ log(n_day), 
                  data = data_exp)
summary(tvp_sp_lm)
plot(tvp_sp_lm)


plot(data_exp %>% select(n_day, deaths), type = "l")
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
fcst_date <- (data_exp %>% tail(1) %>% pull(date)) + (1:10)
fcst_deaths <- forecast(tvp_sp_lm, 
                        n.ahead = 10, 
                        newx = matrix(log(fcst_n_day), byrow = FALSE)) %>% 
  exp()
fcst <- tibble(date = fcst_date, n_day = fcst_n_day, deaths = fcst_deaths)

data_exp %>% select(date, deaths) %>% 
  plot(type = "l", xlim = c(min(data_exp$date), max(fcst$date)), 
       ylim = c(0, max(fcst$deaths)))

lines(fcst %>% select(date, deaths), lty = 2, col = "red", type = "o")

aux <- bind_rows(data_exp %>% select(date, n_day, deaths),
                 fcst) %>% 
  mutate(new_deaths = c(0, diff(deaths)))

plot(aux %>% select(date, new_deaths), type = "o")

# dy / dt
dy_dt <- c(NA, diff(k_t))*(1:length(k_t)) + k_t + c(NA, diff(beta_0))
dy_dt <- dy_dt * data_exp$deaths
plot(dy_dt, type = "l")
abline(h = 0, lty = 2)
