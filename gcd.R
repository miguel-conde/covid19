
# LIBRARIES and SOURCES ---------------------------------------------------


library(tidyverse)

# CONSTANTS ---------------------------------------------------------------


WORLD_POP_URL <- "http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv"

POP_FILE <- "API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv"

CONFIRMED_TS_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
DEATHS_TS_URL    <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
RECOVERED_TS_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"


# AUXILIARY FUNCTIONS -----------------------------------------------------

library(tibbletime)

roll_IA_14 <- rollify(function(x) sum(diff(x)), window = 14)

get_cntry_region_ttss <- function(cntry_reg,
                                  raw_data_list,
                                  prov_st = NULL,
                                  pop_data = NULL) {
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
    full_join(ts_recovered) 
  
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
                  pop = mean(x2018)) %>% 
        ungroup()
    } else {
      out <- out %>% 
        group_by(date) %>% 
        summarise(Lat = mean(Lat),
                  Long = mean(Long),
                  confirmed = sum(confirmed),
                  deaths = sum(deaths),
                  recovered = sum(recovered)) %>% 
        ungroup()
    }
    
  } 
  
  if(!is.null(pop_data)) {
    out <- out %>%
      mutate(IA_14 = roll_IA_14(confirmed) / pop * 1e5) %>% 
      select(-pop) %>% 
      drop_na
  }

  out
}

# WORLD POP DATA ----------------------------------------------------------

temp <- tempfile(fileext = ".zip")
download.file(WORLD_POP_URL, temp, mode="wb")
unzip(temp, POP_FILE)
# note that here I modified your original read.table() which did not work
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

china_data <- get_cntry_region_ttss("China", raw_data_list = raw_data_list, pop_data = mydata)

spain_data <- get_cntry_region_ttss("Spain", raw_data_list = raw_data_list, pop_data = mydata)

italy_data <- get_cntry_region_ttss("Italy", raw_data_list = raw_data_list, pop_data = mydata)

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

Conento::descriptivos_n_variables(china_data %>% select(-Lat, -Long))
Conento::descriptivos_n_variables(italy_data %>% select(-Lat, -Long))
Conento::descriptivos_n_variables(spain_data %>% select(-Lat, -Long))


# ESPAÑA ------------------------------------------------------------------

library(tidyverse)
library(tabulizer)

ministerio = "https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/documentos/Actualizacion_41_COVID-19.pdf"

area <- locate_areas(ministerio, pages = 2)

area[[1]]

pdf_lista <- extract_tables(
  ministerio,
  output = "data.frame",
  pages = c(2),
  area = list(
    c(337.89431,  90.69972, 684.23597, 510.25186)
  ),
  guess = FALSE,
  encoding = "UTF-8"
)


datos <- data.frame(pdf_lista[1])


