library(highcharter)
library(tidyverse)
# library(dplyr)
# library(tidyr)
# library(stringr)

# CONSTANTS ---------------------------------------------------------------

DIR_DATA <- here::here("data")
DIR_DATALAKE <- file.path(DIR_DATA, "datalake")

# 1 - ESPAÑA / DATADISTA --------------------------------------------------

URL_ROOT <- "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/"

## CASOS
URL_CASOS_CCAA_WIDE <- paste0(URL_ROOT, "ccaa_covid19_casos.csv")
RDS_CASOS_CCAA_WIDE <- file.path(DIR_DATALAKE, "ccaa_covid19_casos.Rds")

URL_CASOS_CCAA_LONG <- paste0(URL_ROOT, "ccaa_covid19_casos_long.csv")
RDS_CASOS_CCAA_LONG <- file.path(DIR_DATALAKE, "ccaa_covid19_casos_long.Rds")

## FALLECIDOS
URL_FALLECIDOS_CCAA_WIDE <- paste0(URL_ROOT, "ccaa_covid19_fallecidos.csv")
RDS_FALLECIDOS_CCAA_WIDE <- file.path(DIR_DATALAKE, "ccaa_covid19_fallecidos.Rds")

URL_FALLECIDOS_CCAA_LONG <- paste0(URL_ROOT, "ccaa_covid19_fallecidos_long.csv")
RDS_FALLECIDOS_CCAA_LONG <- file.path(DIR_DATALAKE, "ccaa_covid19_fallecidos_long.Rds")

## ALTAS
URL_ALTAS_CCAA_WIDE <- paste0(URL_ROOT, "ccaa_covid19_altas.csv")
RDS_ALTAS_CCAA_WIDE <- file.path(DIR_DATALAKE, "ccaa_covid19_altas.Rds")

URL_ALTAS_CCAA_LONG <- paste0(URL_ROOT, "ccaa_covid19_altas_long.csv")
RDS_ALTAS_CCAA_LONG <- file.path(DIR_DATALAKE, "ccaa_covid19_altas_long.Rds")

## HOSPITALIZADOS
URL_HOSPITALIZADOS_CCAA_WIDE <- paste0(URL_ROOT, "ccaa_covid19_hospitalizados.csv")
RDS_HOSPITALIZADOS_CCAA_WIDE <- file.path(DIR_DATALAKE, "ccaa_covid19_hospitalizados.Rds")

URL_HOSPITALIZADOS_CCAA_LONG <- paste0(URL_ROOT, "ccaa_covid19_hospitalizados_long.csv")
RDS_HOSPITALIZADOS_CCAA_LONG <- file.path(DIR_DATALAKE, "ccaa_covid19_hospitalizados_long.Rds")

## UCI
URL_UCI_CCAA_WIDE <- paste0(URL_ROOT, "ccaa_covid19_uci.csv")
RDS_UCI_CCAA_WIDE <- file.path(DIR_DATALAKE, "ccaa_covid19_uci.Rds")

URL_UCI_CCAA_LONG <- paste0(URL_ROOT, "ccaa_covid19_uci_long.csv")
RDS_UCI_CCAA_LONG <- file.path(DIR_DATALAKE, "ccaa_covid19_uci_long.Rds")

# 2 - DATOS CCAA ----------------------------------------------------------

## TABLA DATOS CCAA
RDS_TBL_CCAA <- file.path(DIR_DATALAKE, "tbl_ccaa.Rds")

# 3 - WORLD POPULATION - WORLD BANK ---------------------------------------

WORLD_POP_URL <- "http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv"

# POP_FILE <- "API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv"
POP_FILE <- "API_SP.POP.TOTL_DS2_en_csv_v2_887275.csv"

WLD_POP_RDS <- file.path(DIR_DATALAKE, "wld_pop.Rds")

CSV_WLD_BANK_COUNTRIES <- file.path(DIR_DATA, "world_bank_countries.csv")
RDS_WLD_BANK_COUNTRIES <- file.path(DIR_DATALAKE, "world_bank_countries.Rds")

# 4 - WORLD DATA -JHU -----------------------------------------------------

JHU_ROOT_URL <- 
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/"
JHU_TS_URL <- paste0(JHU_ROOT_URL, "csse_covid_19_time_series/")

CONFIRMED_TS <- "time_series_covid19_confirmed_global"
CONFIRMED_TS_URL <- paste0(JHU_TS_URL, CONFIRMED_TS, ".csv")
CONFIRMED_TS_RDS <- file.path(DIR_DATALAKE, paste0(CONFIRMED_TS, ".Rds"))

DEATHS_TS <- "time_series_covid19_deaths_global"
DEATHS_TS_URL <- paste0(JHU_TS_URL, DEATHS_TS,".csv")
DEATHS_TS_RDS <- file.path(DIR_DATALAKE, paste0(DEATHS_TS,".Rds"))

RECOVERED_TS <- "time_series_covid19_recovered_global"
RECOVERED_TS_URL <- paste0(JHU_TS_URL, RECOVERED_TS, ".csv")
RECOVERED_TS_RDS <- file.path(DIR_DATALAKE, paste0(RECOVERED_TS, ".Rds"))

JHU_COUNTRIES_TABLE_URL <- paste0(JHU_ROOT_URL, "UID_ISO_FIPS_LookUp_Table.csv")
JHU_COUNTRIES_TABLE_RDS <- file.path(DIR_DATALAKE, "jhu_countries_table.Rds")

# 5 - Ministerio Sanidad --------------------------------------------------

# MINISTERIO SANIDAD
URL_MIN <- "https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/documentos/Actualizacion_XX_COVID-19.pdf" 
CSV_MIN <- "https://covid19.isciii.es/resources/serie_historica_acumulados.csv"





# OTROS -------------------------------------------------------------------

# Defunciones según la Causa de Muerte
# {"Id":23, "Cod_IOE":"30417", "Nombre":"Estadística de Defunciones según la Causa de Muerte", "Codigo":"ECM"}
URL_INE_DEFUNC <- "https://www.ine.es/jaxiT3/Tabla.htm?t=14819&L=0"
# Población CCAA
URL_INE_CCAA_POB <- "https://www.ine.es/jaxiT3/Datos.htm?t=2853#!tabs-tabla"