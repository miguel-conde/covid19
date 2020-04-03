

# LIBRARIES and SOURCES ---------------------------------------------------
library(readr)
library(tidycovid19)

source("global.R", encoding = "UTF8")
# source("utils.R", encoding = "UTF8")

# 1 - CONSTANTS -----------------------------------------------------------
# 



# AUXILIARY FUNCTIONS -----------------------------------------------------

get_world_pop_data <- function() {
  
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
  
  return(mydata)
}

# 1.1 - CCAA - CÓDIGO ISO -------------------------------------------------

CCAA_CODIGO_ISO = tribble(~codigo_iso, ~ccaa,
                          "ES", "España",
                          "AN", "Andalucía",
                          "AR", "Aragón",
                          "AS", "Asturias, Principado de",
                          "CN", "Canarias",
                          "CB", "Cantabria",
                          "CM", "Castilla-La Mancha",
                          "CL", "Castilla y León",
                          "CT", "Catalunya (Cataluña)",
                          "EX", "Extremadura",
                          "GA", "Galicia (Galicia)",
                          "IB", "Illes Balears (Islas Baleares)",
                          "RI", "La Rioja",
                          "MD", "Madrid, Comunidad de",
                          "MC", "Murcia, Región de",
                          "NC", "Comunidad Foral de/Nafarroako Foru Komunitateanota",
                          "PV", "País Vasco/Euskadinota",
                          "VC", "Valenciana, Comunidad/Valenciana, Comunitat",
                          "CE", "Ceuta",
                          "ME", "Melilla")



# 1.2 - CCAA - Población --------------------------------------------------

POP_SP = tribble(
  ~codigo_iso, ~ccaa_pop, ~pob,
  "ES", "España", "47026208",
  "AN", "01 Andalucía", "8414240",
  "AR", "02 Aragón", "1319291",
  "AS", "03 Asturias, Principado de", "1022800",
  "IB", "04 Balears, Illes", "1149460",
  "CN", "05 Canarias", "2153389",
  "CB", "06 Cantabria", "581078",
  "CL", "07 Castilla y León", "2399548",
  "CM", "08 Castilla - La Mancha", "2032863",
  "CT", "09 Cataluña", "7675217",
  "VC", "10 Comunitat Valenciana", "5003769",
  "EX", "11 Extremadura", "1067710",
  "GA", "12 Galicia", "2699499",
  "MD", "13 Madrid, Comunidad de", "6663394",
  "MC", "14 Murcia, Región de", "1493898",
  "NC", "15 Navarra, Comunidad Foral de", "654214",
  "PV", "16 País Vasco", "2207776",
  "RI", "17 Rioja, La", "316798",
  "CE", "18 Ceuta", "84777",
  "ME", "19 Melilla", "86487"
) %>% mutate(pob = as.integer(pob)) 


# 1.3 - CCAA - Código INE -------------------------------------------------

CCAA_CODIGO_INE <- 
  tibble::tribble(
    ~codigo_ine,             ~ccaa_ine, ~codigo_iso,
    "01",          "Andalucía", "AN",
    "02",             "Aragón", "AR",
    "03",           "Asturias", "AS",
    "04",           "Baleares", "IB",
    "05",           "Canarias", "CN",
    "06",          "Cantabria", "CB",
    "08", "Castilla-La Mancha", "CM",
    "07",    "Castilla y León", "CL",
    "09",           "Cataluña", "CT",
    "18",              "Ceuta", "CE",
    "10",      "C. Valenciana", "VC",
    "11",        "Extremadura", "EX",
    "12",            "Galicia", "GA",
    "13",             "Madrid", "MD",
    "19",            "Melilla", "ME",
    "14",             "Murcia", "MC",
    "15",            "Navarra", "NC",
    "16",         "País Vasco", "PV",
    "17",           "La Rioja", "RI",
    "00",             "España", "ES"
  )


# 1.4 - CCAA_TBL ----------------------------------------------------------

TBL_CCAA <- CCAA_CODIGO_ISO %>% 
  full_join(CCAA_CODIGO_INE, by = "codigo_iso") %>% 
  full_join(POP_SP, by = "codigo_iso") 

saveRDS(TBL_CCAA, RDS_TBL_CCAA)

# 2 - SPAIN DATA ----------------------------------------------------------

ccaa_casos_wide <- read_csv(URL_CASOS_CCAA_WIDE) %>% janitor::clean_names()
saveRDS(ccaa_casos_wide, RDS_CASOS_CCAA_WIDE)

ccaa_casos_long <- read_csv(URL_CASOS_CCAA_LONG) %>% janitor::clean_names()
saveRDS(ccaa_casos_long, RDS_CASOS_CCAA_LONG)

ccaa_fallecidos_wide <- read_csv(URL_FALLECIDOS_CCAA_WIDE) %>% janitor::clean_names()
saveRDS(ccaa_fallecidos_wide, RDS_FALLECIDOS_CCAA_WIDE)

ccaa_fallecidos_long <- read_csv(URL_FALLECIDOS_CCAA_LONG) %>% janitor::clean_names()
saveRDS(ccaa_fallecidos_long, RDS_FALLECIDOS_CCAA_LONG)

ccaa_altas_wide <- read_csv(URL_ALTAS_CCAA_WIDE) %>% janitor::clean_names()
saveRDS(ccaa_altas_wide, RDS_ALTAS_CCAA_WIDE)

ccaa_altas_long <- read_csv(URL_ALTAS_CCAA_LONG) %>% janitor::clean_names()
saveRDS(ccaa_altas_long, RDS_ALTAS_CCAA_LONG)

ccaa_hospitalizados_wide <- read_csv(URL_HOSPITALIZADOS_CCAA_WIDE) %>% janitor::clean_names()
saveRDS(ccaa_hospitalizados_wide, RDS_HOSPITALIZADOS_CCAA_WIDE)

ccaa_hospitalizados_long <- read_csv(URL_HOSPITALIZADOS_CCAA_LONG) %>% janitor::clean_names()
saveRDS(ccaa_hospitalizados_long, RDS_HOSPITALIZADOS_CCAA_LONG)

ccaa_uci_wide <- read_csv(URL_UCI_CCAA_WIDE) %>% janitor::clean_names()
saveRDS(ccaa_uci_wide, RDS_UCI_CCAA_WIDE)

ccaa_uci_long <- read_csv(URL_UCI_CCAA_LONG) %>% janitor::clean_names()
saveRDS(ccaa_uci_long, RDS_UCI_CCAA_LONG)


# 3 - WORLD POP DATA ------------------------------------------------------

wprld_pop <- get_world_pop_data()
saveRDS(wprld_pop, WLD_POP_RDS)

world_bank_countries <- read_delim(CSV_WLD_BANK_COUNTRIES, 
                                   ";", 
                                   escape_double = FALSE, 
                                   col_types = cols(Code = col_integer()), 
                                   trim_ws = TRUE) %>% 
  janitor::clean_names()

saveRDS(world_bank_countries, RDS_WLD_BANK_COUNTRIES)

# 4 - JHU DATA ------------------------------------------------------------

confirmed_ts <- read_csv(file = CONFIRMED_TS_URL)
saveRDS(confirmed_ts, CONFIRMED_TS_RDS)

deaths_ts    <- read_csv(file = DEATHS_TS_URL)
saveRDS(deaths_ts, DEATHS_TS_RDS)

recovered_ts <- read_csv(file = RECOVERED_TS_URL)
saveRDS(recovered_ts, RECOVERED_TS_RDS)

jhu_countries_table <- read_csv(file = JHU_COUNTRIES_TABLE_URL)
saveRDS(jhu_countries_table, JHU_COUNTRIES_TABLE_RDS)

jhu_countries_data <- download_merged_data(cached = TRUE)
saveRDS(jhu_countries_data, JHU_COUNTRIES_DATA_RDS)
