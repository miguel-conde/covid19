
# LIBRARIES and SOURCES ---------------------------------------------------


library(tidyverse)

# CONSTANTS ---------------------------------------------------------------


WORLD_POP_URL <- "http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv"

# POP_FILE <- "API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv"
POP_FILE <- "API_SP.POP.TOTL_DS2_en_csv_v2_887275.csv"

CONFIRMED_TS_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
DEATHS_TS_URL    <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
RECOVERED_TS_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"


# AUXILIARY FUNCTIONS -----------------------------------------------------

library(tibbletime)

roll_IA_14 <- rollify(function(x) sum(diff(x)), window = 14)

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


# ESPAÑA ------------------------------------------------------------------

library(tidyverse)
library(tabulizer)

ministerio = "https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/documentos/Actualizacion_47_COVID-19.pdf"

area <- locate_areas(ministerio, pages = 1)

area[[1]]

pdf_lista <- extract_tables(
  ministerio,
  output = "data.frame",
  pages = c(1),
  area = list(
    c(337.89431,  90.69972, 684.23597, 510.25186)
  ),
  guess = FALSE,
  encoding = "UTF-8"
)


datos <- data.frame(pdf_lista[1])


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
  filter(active > 100) %>% 
  select(active) 
china_pruned_data <- china_pruned_data %>% 
  mutate(n_day = 1:nrow(china_pruned_data))

italy_pruned_data <- italy_data %>% 
  filter(active > 100) %>% 
  select(active) 
italy_pruned_data <- italy_pruned_data %>% 
  mutate(n_day = 1:nrow(italy_pruned_data))

spain_pruned_data <- spain_data %>% 
  filter(active > 100) %>% 
  select(active) 
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


# DIFFs -------------------------------------------------------------------

spain_data <- spain_data %>% 
  mutate_at(vars(confirmed, deaths, recovered, active),
            list(new = ~ c(NA, diff(.))))

china_data %>% ggplot(aes(x = date)) + 
  geom_line(aes(y = deaths_new, colour = "deaths_new")) + 
  geom_line(aes(y = recovered_new, colour = "recovered_new"))  + 
  geom_line(aes(y = active_new, colour = "active_new"))
  xlab("Fecha") + 
  ylab("Nº")
  