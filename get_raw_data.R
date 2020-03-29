

# LIBRARIES and SOURCES ---------------------------------------------------
library(readr)
library(tidyverse)

# CONSTANTS ---------------------------------------------------------------

DIR_DATALAKE <- here::here("data", "datalake")

## CASOS
URL_CASOS_CCAA_WIDE <- 
  "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos.csv"
RDS_CASOS_CCAA_WIDE <- file.path(DIR_DATALAKE, "ccaa_covid19_casos.Rds")

URL_CASOS_CCAA_LONG <- 
  "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos_long.csv"
RDS_CASOS_CCAA_LONG <- file.path(DIR_DATALAKE, "ccaa_covid19_casos_long.Rds")

## FALLECIDOS
URL_FALLECIDOS_CCAA_WIDE <- 
  "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos.csv"
RDS_FALLECIDOS_CCAA_WIDE <- file.path(DIR_DATALAKE, "ccaa_covid19_fallecidos.Rds")

URL_FALLECIDOS_CCAA_LONG <- 
  "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos_long.csv"
RDS_FALLECIDOS_CCAA_LONG <- file.path(DIR_DATALAKE, "ccaa_covid19_fallecidos_long.Rds")

## ALTAS
URL_ALTAS_CCAA_WIDE <- 
  "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_altas.csv"
RDS_ALTAS_CCAA_WIDE <- file.path(DIR_DATALAKE, "ccaa_covid19_altas.Rds")

URL_ALTAS_CCAA_LONG <- 
  "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_altas_long.csv"
RDS_ALTAS_CCAA_LONG <- file.path(DIR_DATALAKE, "ccaa_covid19_altas_long.Rds")


# GET DATA ----------------------------------------------------------------

ccaa_casos_wide <- read_csv(URL_CASOS_CCAA_WIDE)
saveRDS(ccaa_casos_wide, RDS_CASOS_CCAA_LONG)

ccaa_casos_long <- read_csv(URL_CASOS_CCAA_WIDE)
saveRDS(ccaa_casos_long, RDS_CASOS_CCAA_LONG)

ccaa_fallecidos_wide <- read_csv(URL_FALLECIDOS_CCAA_WIDE)
saveRDS(ccaa_fallecidos_wide, RDS_FALLECIDOS_CCAA_LONG)

ccaa_fallecidos_long <- read_csv(URL_FALLECIDOS_CCAA_WIDE)
saveRDS(ccaa_fallecidos_long, RDS_FALLECIDOS_CCAA_LONG)

ccaa_altas_wide <- read_csv(URL_ALTAS_CCAA_WIDE)
saveRDS(ccaa_altas_wide, RDS_ALTAS_CCAA_LONG)

ccaa_altas_long <- read_csv(URL_ALTAS_CCAA_WIDE)
saveRDS(ccaa_altas_long, RDS_ALTAS_CCAA_LONG)

tbl_ccaa_ine <- ccaa_altas_long %>% 
  select(cod_ine, ccaa = CCAA) %>% 
  distinct()
