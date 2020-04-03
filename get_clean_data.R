# LIBRARIES and SOURCES ---------------------------------------------------

library(tibbletime)

source("global.R", encoding = "UTF8")


# FUNCTIONS ---------------------------------------------------------------

tbl_ccaa <- readRDS(RDS_TBL_CCAA)

get_sp_clean_data <- function() {
  
  out <- readRDS(RDS_CASOS_CCAA_LONG) %>% 
    rename(casos = total) %>% 
    full_join(readRDS(RDS_FALLECIDOS_CCAA_LONG) %>% 
                rename(fallecidos = total)) %>% 
    full_join(readRDS(RDS_ALTAS_CCAA_LONG) %>% 
                rename(altas = total))  %>% 
    full_join(readRDS(RDS_HOSPITALIZADOS_CCAA_LONG) %>% 
                rename(hospitalizados = total))  %>% 
    full_join(readRDS(RDS_UCI_CCAA_LONG) %>% 
                rename(uci = total)) %>% 
    mutate(activos = casos - fallecidos - altas) %>% 
    rename(codigo_ine = cod_ine) %>%
    select(-ccaa) %>% 
    full_join(tbl_ccaa %>% select(-pob))
  
  return(out)
}

# WORLD POP DATA ----------------------------------------------------------
wld_pop_data <- readRDS(WLD_POP_RDS)

# COUNTRIES ---------------------------------------------------------------

get_jhu_clean_data <- function() {
  confirmed_ts <- readRDS(CONFIRMED_TS_RDS)
  deaths_ts <- readRDS(DEATHS_TS_RDS)
  recovered_ts <- readRDS(RECOVERED_TS_RDS)
  
  raw_data_list <- list(confirmed_ts = confirmed_ts,
                        deaths_ts = deaths_ts,
                        recovered_ts = recovered_ts)
  
  out <- raw_data_list %>% 
    lapply(function(x) {
      x %>% rename_if(is.character, janitor::make_clean_names)
    })
  
  return(out)
}

get_jhu_clean_data <- function() {
  out <- readRDS(JHU_COUNTRIES_DATA_RDS) %>% 
    select(country, iso3c, date, confirmed, deaths, 
           recovered, population, region) %>% 
           mutate(active = confirmed - deaths - recovered)
  
  return(out)
}
