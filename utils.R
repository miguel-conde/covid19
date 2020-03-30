library(tabulizer)
library(tibbletime)
library(highcharter)
library(INEbaseR)
library(tidyverse)

source("global.R", encoding = "UTF8")


# FUNCTIONS ---------------------------------------------------------------


# Johns Hopkins Data ------------------------------------------------------


roll_IA_14 <- rollify(function(x) sum(diff(x)), window = 14)
roll_mean_14 <- rollify(mean, window = 14)

get_cntry_region_ttss <- function(cntry_reg,
                                  raw_data_list,
                                  prov_st = NULL,
                                  pop_data = NULL,
                                  per_100K = FALSE) {
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
    
    if(per_100K == TRUE) {
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


# Datos Ministerio Sanidad ------------------------------------------------


# CSV ---------------------------------------------------------------------

get_csv_min_sanidad <- function() {
  datos <- read.csv2(CSV_MIN, sep = ",", 
                     stringsAsFactors = FALSE) %>% 
    janitor::clean_names() %>% 
    mutate(fecha = lubridate::dmy(fecha)) %>% 
    mutate_at(vars(-fecha, -ccaa_codigo_iso), ~ as.integer(.))
  
  
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
  
  return(raw_datos_min)
}

calc_vars <- function(in_data, var_mode) {
  
  out <- in_data
  if (var_mode == "units") {
    out <- out %>% 
      mutate_if(is.numeric, list(var = ~  c(NA, diff(.)))) %>% 
      select(fecha, ends_with("var"))
    
  } else {
    if (var_mode == "perc") {
      out <- out %>%
        mutate_if(is.numeric, 
                  list(var_perc = ~ (./dplyr::lag(.) - 1) * 100)) %>% 
        select(fecha, ends_with("var_perc"))
    }
  }
}

datos_min_ccaa_col <- function(raw_datos_min, tgt_var,
                               var_res = c("none", "units", "perc"),
                               per_100K = FALSE,
                               info_ccaa = tbl_ccaa) {
  
  var_mode <- match.arg(var_res)
  
  enquo_tgt_var <- enquo(tgt_var)
  
  datos_min <- raw_datos_min %>% 
    select(fecha, codigo_iso, !!enquo_tgt_var) %>% 
    spread("codigo_iso", quo_name(enquo_tgt_var))
  # datos_min <- datos_min %>% 
  #   mutate(ES = rowSums(datos_min %>% select_if(is.numeric), na.rm = TRUE))
  
  out <- datos_min %>% 
    right_join(tibble(fecha = seq(from = min(datos_min$fecha),
                                  to = max(datos_min$fecha),
                                  by = 1)),
               by = "fecha")
  
  if (per_100K == TRUE) {
    aux <- out %>% select(-fecha)
    aux2 <- info_ccaa$pob
    names(aux2) <- info_ccaa$ccaa_codigo_iso
    aux2 <- aux2[names(aux)] / 100000
    
    aux <- sweep(aux, 2, STATS = aux2, FUN = "/") %>% 
      as.data.frame()
    
    out <- out %>% select(fecha) %>% 
      bind_cols(aux)
    names(out)[-1]  <- paste0(names(out)[-1], "_per_100K")
  }
  
  if (var_mode == "units") {
    out <- out %>%
      mutate_if(is.numeric, list(var = ~  c(NA, diff(.)))) %>%
      select(fecha, ends_with("var"))

  } else {
    if (var_mode == "perc") {
      out <- out %>%
        mutate_if(is.numeric,
                  list(var_perc = ~ (./dplyr::lag(.) - 1) * 100)) %>%
        select(fecha, ends_with("var_perc"))
    }
  }
  
  return(out)
}

datos_min_ccaa <- function(raw_datos_min, cod_ccaa, info_ccaa = tbl_ccaa) {
  
  if(cod_ccaa == "ES") {
    out <- raw_datos_min %>% 
      filter(codigo_iso  == cod_ccaa) %>% 
      group_by(fecha) %>% 
      summarise_if(is.numeric, sum, na.rm = TRUE)
    out <- tibble(codigo_iso = rep("ES", nrow(out)),
                  ccaa = rep("España", nrow(out))) %>% 
      bind_cols(out)
  } else {
    out <- raw_datos_min %>% 
      filter(codigo_iso  == cod_ccaa) %>% 
      right_join(tibble(fecha = seq(from = min(raw_datos_min$fecha),
                                    to = max(raw_datos_min$fecha),
                                    by = 1)),
                 by = "fecha")
  }
  
  ref_pop <- info_ccaa %>% filter(codigo_iso == cod_ccaa) %>% pull(pob)
  out <- out %>% 
    mutate_if(is.numeric, list(per_100K = ~ . / ref_pop * 100000)) %>%
    mutate_if(is.numeric, list(var = ~ c(NA, diff(.)))) %>% 
    mutate_at(vars(ends_with("_var")), 
              list(perc = ~ (./dplyr::lag(.) - 1) * 100)) %>% 
    select(-ends_with("_100K_var_perc"))
  
  # out <- out %>%
  #   mutate_if(is.numeric, list(var = ~  c(NA, diff(.)))) %>%
  #   select(fecha, ends_with("var"))
  # 
  # out <- out %>%
  #   mutate_if(is.numeric,
  #             list(var_perc = ~ (./dplyr::lag(.) - 1) * 100)) %>%
  #   select(fecha, ends_with("var_perc"))
  
  return(out)
}



# PDF ---------------------------------------------------------------------

get_old_sp_table <- function(n, url_min = URL_MIN, info_reports = SP_REPORTS_INFO) {

  ministerio <- URL_MIN %>%  str_replace("XX", as.character(n))
  n_page <- info_reports[info_reports$n_report == n, ]$n_page
  n_table <- info_reports[info_reports$n_report == n, ]$n_table
  table_area <- info_reports[info_reports$n_report == n,
                             c("top", "left", "bottom", "right")]

  out <- extract_tables(ministerio,
                        pages = n_page,
                        area = list(table_area),
                        guess = FALSE,
                        encoding = "UTF-8",
                        output = "data.frame")[[n_table]]

  out

}

get_sp_table <- function(n, url_min = URL_MIN, info_reports = SP_REPORTS_INFO) {

  ministerio <- URL_MIN %>%  str_replace("XX", as.character(n))
  n_page <- info_reports[info_reports$n_report == n, ]$n_page
  n_table <- info_reports[info_reports$n_report == n, ]$n_table

  out <- extract_tables(ministerio,
                        pages = n_page,
                        encoding = "UTF-8",
                        output = "data.frame")[[n_table]]

  out

}

format_sp_old_table <- function(data_table) {
  # Busca Andalucía
  i <- which(data_table[,1] == "Andalucía")

  out <- data_table[i:nrow(data_table), ]

  names(out) <- c("CCAA", "TOTAL Confirmados", "IA (14 d.)")

  out <- out %>% janitor::clean_names()

  out <- out %>%
    mutate(ia_14_d = str_replace(ia_14_d, ",", ".")) %>%
    mutate_at(vars(-ccaa, -ia_14_d), ~ str_remove(., "\\..*")) %>%
    mutate_at(vars(-ccaa), ~ as.numeric(.)) %>%
    mutate(ccaa = ifelse(ccaa %in% c("Total", "TOTAL"),
                         "ESPAÑA",
                         ccaa)) %>%
    mutate(ccaa = ifelse(ccaa == "Castilla La Mancha",
                         "Castilla-La Mancha",
                         ccaa))

  out
}


format_sp_new_table <- function(data_table) {
  # Busca Andalucía
  i <- which(data_table[,1] == "Andalucía")

  out <- data_table[i:nrow(data_table), ]

  names(out) <- c("CCAA", "TOTAL Confirmados", "IA (14 d.)",
                  "UCI", "Fallecidos")

  out <- out %>% janitor::clean_names()

  out <- out %>%
    mutate(ia_14_d = str_replace(ia_14_d, ",", ".")) %>%
    mutate_at(vars(-ccaa, -ia_14_d), ~ str_remove(., "\\.")) %>%
    mutate_at(vars(-ccaa), ~ as.numeric(.)) %>%
    mutate(ccaa = ifelse(ccaa %in% c("Total", "TOTAL"),
                         "ESPAÑA",
                         ccaa)) %>%
    mutate(ccaa = ifelse(ccaa == "Castilla La Mancha",
                         "Castilla-La Mancha",
                         ccaa))

  out
}

format_sp_new2_table <- function(data_table) {
  # Busca Andalucía
  i <- which(data_table[,1] == "Andalucía")

  out <- data_table[i:nrow(data_table), ]

  names(out) <- c("CCAA", "TOTAL Confirmados", "IA (14 d.)",
                  "Hospitalizados", "UCI", "Fallecidos",
                  "Nuevos")

  out <- out %>% janitor::clean_names()

  out <- out %>%
    mutate(ia_14_d = str_replace(ia_14_d, ",", ".")) %>%
    mutate_at(vars(-ccaa, -ia_14_d), ~ str_remove(., "\\.")) %>%
    mutate_at(vars(-ccaa), ~ as.numeric(.)) %>%
    mutate(ccaa = ifelse(ccaa %in% c("Total", "TOTAL"),
                         "ESPAÑA",
                         ccaa)) %>%
    mutate(ccaa = ifelse(ccaa == "Castilla La Mancha",
                         "Castilla-La Mancha",
                         ccaa))

  out
}

format_sp_new3_table <- function(data_table) {
  # Busca Andalucía
  i <- which(data_table[,1] == "Andalucía")

  out <- data_table[i:nrow(data_table), ]

  names(out) <- c("CCAA", "TOTAL Confirmados", "IA (14 d.)",
                  "Hospitalizados", "UCI", "Fallecidos",
                  "Curados", "Nuevos")

  out <- out %>% janitor::clean_names()

  out <- out %>%
    mutate(ia_14_d = str_replace(ia_14_d, ",", ".")) %>%
    mutate_at(vars(-ccaa, -ia_14_d), ~ str_remove(., "\\.")) %>%
    mutate_at(vars(-ccaa), ~ as.numeric(.)) %>%
    mutate(ccaa = ifelse(ccaa %in% c("Total", "TOTAL"),
                         "ESPAÑA",
                         ccaa)) %>%
    mutate(ccaa = ifelse(ccaa == "Castilla La Mancha",
                         "Castilla-La Mancha",
                         ccaa))

  out
}

SP_REPORTS_INFO <- tribble(
  ~n_report, ~date, ~obs, ~n_page, ~n_table, ~top, ~left, ~bottom, ~right,
  ## No tienen info CCAA:
  # 31, as.Date("2020/02/26"), "OLD",   3, 1, NA, NA, NA, NA,
  # 32, as.Date("2020/02/27"), "OLD",   3, 1, NA, NA, NA, NA,
  # 33, as.Date("2020/02/28"), "OLD",   3, 1, NA, NA, NA, NA,
  # 34, as.Date("2020/03/02"), "OLD",   3, 1, NA, NA, NA, NA,
  35, as.Date("2020/03/03"), "OLD",   3, 1, 182.9489, 159.1162, 547.0704, 432.6514,
  36, as.Date("2020/03/04"), "NEW",   2, 1, NA, NA, NA, NA,
  37, as.Date("2020/03/05"), "NEW",   2, 1, NA, NA, NA, NA,
  38, as.Date("2020/03/06"), "NEW",   2, 1, NA, NA, NA, NA,
  39, as.Date("2020/03/09"), "NEW",   2, 1, NA, NA, NA, NA,
  40, as.Date("2020/03/10"), "NEW",   2, 1, NA, NA, NA, NA,
  41, as.Date("2020/03/11"), "NEW",   2, 1, NA, NA, NA, NA,
  42, as.Date("2020/03/12"), "NEW",   2, 1, NA, NA, NA, NA,
  43, as.Date("2020/03/13"), "NEW",   2, 1, NA, NA, NA, NA,
  ## NO ESTÁN en la web del Ministerio:
  # 44, as.Date("2020/03/14"), "OLD",   2, 1, NA, NA, NA, NA,
  # 45, as.Date("2020/03/15"), "OLD",   2, 1, NA, NA, NA, NA,
  46, as.Date("2020/03/16"), "NEW",   2, 1, NA, NA, NA, NA,
  47, as.Date("2020/03/17"), "NEW",   1, 1, NA, NA, NA, NA,
  48, as.Date("2020/03/18"), "NEW",   1, 1, NA, NA, NA, NA,
  49, as.Date("2020/03/19"), "NEW",   1, 1, NA, NA, NA, NA,
  50, as.Date("2020/03/20"), "NEW",   1, 1, NA, NA, NA, NA,
  51, as.Date("2020/03/21"), "NEW_2", 1, 1, NA, NA, NA, NA,
  52, as.Date("2020/03/22"), "NEW_2", 1, 2, NA, NA, NA, NA,
  53, as.Date("2020/03/23"), "NEW_3", 1, 1, NA, NA, NA, NA,
  54, as.Date("2020/03/24"), "NEW_3", 1, 1, NA, NA, NA, NA,
  55, as.Date("2020/03/25"), "NEW_3", 1, 1, NA, NA, NA, NA,
  56, as.Date("2020/03/26"), "NEW_3", 1, 1, NA, NA, NA, NA,
  57, as.Date("2020/03/27"), "NEW_3", 1, 1, NA, NA, NA, NA,
  58, as.Date("2020/03/28"), "NEW_3", 1, 1, NA, NA, NA, NA,
  59, as.Date("2020/03/29"), "NEW_3", 1, 1, NA, NA, NA, NA
)

get_reports_lst <- function(reports_info = SP_REPORTS_INFO) {

  out <- vector(mode = "list", length = nrow(reports_info))

  for(i in 1:nrow(reports_info)) {

    # browser()
    n_report <- reports_info[i, ]$n_report
    n_page <- reports_info[i, ]$n_page
    print(reports_info[i,])

    report_type <- as.character(reports_info[i, "obs"])

    out[[i]] <-
      switch(report_type,
             "OLD" = {
               get_old_sp_table(n_report) %>%
                 format_sp_old_table()
             },
             "NEW" = {
               get_sp_table(n_report) %>%
               format_sp_new_table()
             },
             "NEW_2" = {
               get_sp_table(n_report) %>%
               format_sp_new2_table()
             },
             "NEW_3" = {
               get_sp_table(n_report) %>%
               format_sp_new3_table()
             }
      )
  }

  names(out) <- reports_info$date %>% as.character()
  
  out <- out %>% 
    lapply(change_nom_ccaa)

  out
}

get_OK_nom_ccaa <- function(cod_ISO) {
  if(cod_ISO == "ES") return("España")
  CCAA_CODIGO_ISO %>% 
    filter(ccaa_codigo_iso == cod_ISO) %>% 
    pull(ccaa)
}


change_nom_ccaa <- function(x) {
  
  out <- x %>% 
    mutate(ccaa = ifelse(ccaa == "Andalucía",
                         get_OK_nom_ccaa("AN"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Aragón",
                         get_OK_nom_ccaa("AR"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Asturias",
                         get_OK_nom_ccaa("AS"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Baleares",
                         get_OK_nom_ccaa("IB"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Canarias",
                         get_OK_nom_ccaa("CN"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Cantabria",
                         get_OK_nom_ccaa("CB"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Castilla-La Mancha",
                         get_OK_nom_ccaa("CM"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Castilla y León",
                         get_OK_nom_ccaa("CL"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Cataluña",
                         get_OK_nom_ccaa("CT"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Ceuta",
                         get_OK_nom_ccaa("CE"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "C. Valenciana",
                         get_OK_nom_ccaa("VC"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Extremadura",
                         get_OK_nom_ccaa("EX"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Galicia",
                         get_OK_nom_ccaa("GA"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Madrid",
                         get_OK_nom_ccaa("MD"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Melilla",
                         get_OK_nom_ccaa("ME"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Murcia",
                         get_OK_nom_ccaa("MC"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "Navarra",
                         get_OK_nom_ccaa("NC"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "País Vasco",
                         get_OK_nom_ccaa("PV"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "La Rioja",
                         get_OK_nom_ccaa("RI"),
                         ccaa)) %>% 
    mutate(ccaa = ifelse(ccaa == "ESPAÑA",
                         get_OK_nom_ccaa("ES"),
                         ccaa))
  
  out
  
}
# kk <- get_reports_lst()
# kkk <- kk %>% map_dfr(~ .x, .id = "date") %>%
#   mutate(date = as.Date(date)) %>%
#   as_tibble
#
# kkkk <- kkk %>% select(date, ccaa, fallecidos) %>% spread("ccaa", "fallecidos")
#
# kkkk %>%
#   select(date, "ESPAÑA")

make_raw_datos_min <- function(lst_reports) {
  tbl_reports <- lst_reports %>% bind_rows(.id = "fecha") %>% 
    mutate(ccaa_codigo_iso = CCAA_CODIGO_ISO %>% pull(ccaa_codigo_iso ))
}

# World Pop Data ----------------------------------------------------------

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


# Datos INE ---------------------------------------------------------------

get_ine_data <- function(code, date_start = "2010-01-01") {
  #
  # Esta función se baja de la web del INE la serie temporal trimestral que se
  # le indique
  #
  # PARAMETERS
  #   - code: char, código de la serie deseada
  #   - date_start: fecha de inicio deseada
  #
  # RETURN
  #   - Tibble con <fecha> (ajustada al dia fina de trimestre) y <valor>. Los 
  #     atributos de esre tibble continen detalles de la serie
  #
  
  # ALGUNOS CÓDIGOS ÚTILES:
  #
  # "CNTR4412" = RENTA NACIONAL DISPONIBLE NETA - DATO BASE
  # "CNTR3179" = Total Nacional. Base 2010. Datos ajustados de estacionalidad y 
  #              calendario. Producto interior bruto a precios de mercado. 
  #              Variación anual Índices de volumen encadenados.
  # "CNTR3180" = Total Nacional. Base 2010. Datos ajustados de estacionalidad y 
  #              calendario. Producto interior bruto a precios de mercado. 
  #              Variación trimestral. Índices de volumen encadenados.
  # "CNTR3883" = Total Nacional. Ocupados. Puestos de trabajo equivalentes a 
  #              tiempo completo. Datos ajustados de estacionalidad y 
  #              calendario. Variación anual Total CNAE. Base 2010.
  # "CNTR3896" = Total Nacional. Ocupados. Puestos de trabajo equivalentes a 
  #              tiempo completo. Datos ajustados de estacionalidad y 
  #              calendario. Variación trimestral. Total CNAE. Base 2010.
  # "EPA815"   = Total Nacional. Tasa de paro de la población. Ambos sexos. 
  #              16 y más años.
  
  # aux <- get_data_serie(code, date_start)
  aux <- INEbaseR::get_series(code = code, 
                              resource = "data",
                              date_start = date_start,
                              date_end = "2100-12-31",
                              nlast = 100000)
  
  
  out <- aux$Data %>% 
    as.tibble %>% 
    mutate(Fecha = as.POSIXct(Fecha/1000, origin = "1970-01-01"),
           fecha = as.Date(Fecha + months(3))) %>%
    rename(valor = Valor) %>% 
    select(fecha, valor)  
  
  aux$Data <- NULL
  attributes(out) <- c(attributes(out), aux)
  
  out
}


# PLOTS -------------------------------------------------------------------

hc_min_ccaa_col <- function(in_data, tgt_col, info_ccaa = tbl_ccaa,
                            plot_type = "line", ...) {
  
  quo_tgt_col <- enquo(tgt_col)
  
  plot_data <- datos_min_ccaa_col(in_data, !!quo_tgt_col, ...)
  
  hc_data <- plot_data %>% 
    gather(codigo_iso, fallecidos, -fecha)  %>% 
    mutate(codigo_iso = str_remove(codigo_iso, "_.+$")) %>% 
    left_join(info_ccaa %>% 
                select(codigo_iso, ccaa)) %>% 
    mutate(ccaa = ifelse(is.na(ccaa), "España", ccaa)) %>% 
    select(-codigo_iso)
  
  out <- hchart(hc_data, 
                type = plot_type, 
                hcaes(x = fecha, y = !!quo_tgt_col, group = ccaa))
  
  out
}


hc_min_ccaa <- function(in_data, tgt_ccaa, metricas = NULL) {
  
  plot_data <- datos_min_ccaa(in_data, tgt_ccaa) %>% 
    select(-starts_with("codigo_"), -starts_with("ccaa")) %>% 
    gather(metrica, valor, -fecha) 
  
  if (!is.null(metricas)) {
    plot_data <- plot_data %>% 
      filter(metrica %in% metricas)
  }
  
  out <- hchart(plot_data , 
                "line", hcaes(x = fecha, y = valor, group = metrica))
  
  out
}
