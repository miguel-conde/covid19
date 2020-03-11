library(tidyverse)

CONFIRMED_TS_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
DEATHS_TS_URL    <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
RECOVERED_TS_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

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

get_cntry_region_ttss <- function(cntry_reg,
                                  prov_st = NULL,
                                  raw_data_list) {
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
  
  out <- ts_cofirmed %>% full_join(ts_deaths) %>% full_join(ts_recovered)
  
  if(is.null(prov_st)) {
    out <- out %>% 
      group_by(date) %>% 
      summarise(Lat = mean(Lat),
                Long = mean(Long),
                confirmed = sum(confirmed),
                deaths = sum(deaths),
                recovered = sum(recovered)) %>% 
      ungroup()
  }
  
  out
}

china_data <- get_cntry_region_ttss("Mainland China", raw_data_list = raw_data_list)

spain_data <- get_cntry_region_ttss("Spain", raw_data_list = raw_data_list)

italy_data <- get_cntry_region_ttss("Italy", raw_data_list = raw_data_list)

plot(italy_data %>% select(date, confirmed, deaths, recovered))

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
