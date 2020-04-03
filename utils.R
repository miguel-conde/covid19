# library(tabulizer)

# library(INEbaseR)
library(tibbletime)
source("global.R", encoding = "UTF8")


# FUNCTIONS ---------------------------------------------------------------


# Johns Hopkins Data ------------------------------------------------------

roll_IA_14 <- rollify(function(x) sum(diff(x)), window = 14)
roll_mean_14 <- rollify(mean, window = 14)

get_cntry_region_ttss <- function(cntry_reg,
                                  clean_data_list,
                                  prov_st = NULL,
                                  pop_data = NULL,
                                  per_100K = FALSE) {
  # browser()
  ts_cofirmed <- clean_data_list[["confirmed_ts"]] %>%
    filter(country_region == cntry_reg)
  ts_deaths <- clean_data_list[["deaths_ts"]] %>%
    filter(country_region == cntry_reg)
  ts_recovered <- clean_data_list[["recovered_ts"]] %>%
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


# Datadista data ----------------------------------------------------------

chg_orig_t <- function(in_data, orig_t) {
  
  out <- lapply(in_data %>% select(-fecha),
                    function(x) {
                      out <- tibble(x = x[x > orig_t]) %>% 
                        drop_na()
                      if(nrow(out) > 0) {
                        out %>% 
                          mutate(n_day = 1:nrow(out))
                      }
                       
                    })
  
  out <- out[sapply(out,function(x) !is.null(x)) %>% unlist]  
  
  if(length(out) == 0) return(NULL)
  
  aux_names <- names(out)
  
  # names(out) <- names(in_data %>% select(-fecha))
  
  out <- out %>% reduce(full_join, by = "n_day") %>% 
    select(-n_day) 
  # names(out) <- names(in_data %>% select(-fecha))
  names(out) <- aux_names
  
  out <- tibble(n_day = 1:nrow(out)) %>% 
    bind_cols(out)
  
  out
}

jhu_chg_orig_t <- function(in_data, orig_t) {
  
  out <- lapply(in_data %>% select(-date),
                function(x) {
                  out <- tibble(x = x[x > orig_t]) %>%
                    drop_na()
                  if(nrow(out) > 0) {
                    out %>%
                      mutate(n_day = 1:nrow(out))
                  }

                })
  # n <- in_data %>% select(-date) %>% names()
  # out <- lapply(n,
  #               function(x) {
  #                 out <- in_data[in_data[, x] > orig_t, ] %>% 
  #                   select(date, x) %>% 
  #                   drop_na()
  #                 if(nrow(out) > 0) {
  #                   out %>% 
  #                     mutate(n_day = 1:nrow(out))
  #                 }
  #                 
  #               })
  
  out <- out[sapply(out,function(x) !is.null(x)) %>% unlist]  
  
  if(length(out) == 0) return(NULL)
  
  aux_names <- names(out)
  
  # names(out) <- names(in_data %>% select(-fecha))
  
  out <- out %>% reduce(full_join, by = "n_day") %>% 
    select(-n_day) 
  # names(out) <- names(in_data %>% select(-fecha))
  names(out) <- aux_names
  
  out <- tibble(n_day = 1:nrow(out)) %>% 
    bind_cols(out)
  
  out
}


datos_min_ccaa_col <- function(clean_datos_min, tgt_var,
                               var_res = c("none", "units", "perc"),
                               per_100K = FALSE,
                               info_ccaa = tbl_ccaa,
                               orig_t = NULL) {

  var_mode <- match.arg(var_res)

  enquo_tgt_var <- enquo(tgt_var)

  datos_min <- clean_datos_min %>%
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
    names(aux2) <- info_ccaa$codigo_iso
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
  
  if (!is.null(orig_t)) {
    # aux_names <- names(out)[-1]
    out <- chg_orig_t(out, orig_t)
    # names(out)[-1] <- aux_names
  }

  return(out)
}

jhu_ctry_data_col <- function(jhu_clean_data, tgt_var,
                               var_res = c("none", "units", "perc"),
                               per_100K = FALSE,
                               orig_t = NULL) {
  
  var_mode <- match.arg(var_res)
  
  enquo_tgt_var <- enquo(tgt_var)
  
  jhu_data <- jhu_clean_data %>%
    select(date, iso3c, !!enquo_tgt_var) %>%
    spread("iso3c", quo_name(enquo_tgt_var))
  
  out <- jhu_data %>%
    right_join(tibble(date = seq(from = min(jhu_data$date),
                                  to = max(jhu_data$date),
                                  by = 1)),
               by = "date")
  
  if (per_100K == TRUE) {
    aux <- out %>% select(-date)
    # aux2 <- info_ccaa$pob
    # names(aux2) <- info_ccaa$iso3c
    # aux2 <- aux2[names(aux)] / 100000
    
    aux2 <- jhu_clean_data %>% 
      select(iso3c, population) %>% 
      distinct() 
    aux2_names <- aux2 %>% pull(iso3c)
    aux2 <- aux2 %>% pull(population)
    names(aux2) <- aux2_names
    aux2 <- aux2[names(aux)] / 100000
    
    aux <- sweep(aux, 2, STATS = aux2, FUN = "/") %>%
      as.data.frame()
    
    out <- out %>% select(date) %>%
      bind_cols(aux)
    names(out)[-1]  <- paste0(names(out)[-1], "_per_100K")
  }
  
  if (var_mode == "units") {
    out <- out %>%
      mutate_if(is.numeric, list(var = ~  c(NA, diff(.)))) %>%
      select(date, ends_with("var"))
    
  } else {
    if (var_mode == "perc") {
      out <- out %>%
        mutate_if(is.numeric,
                  list(var_perc = ~ (./dplyr::lag(.) - 1) * 100)) %>%
        select(date, ends_with("var_perc"))
    }
  }
  
  if (!is.null(orig_t)) {
    # aux_names <- names(out)[-1]
    out <- jhu_chg_orig_t(out, orig_t)
    # names(out)[-1] <- aux_names
  }
  
  return(out)
}


datos_min_ccaa <- function(clean_datos_min, cod_ccaa, info_ccaa = tbl_ccaa) {

  if(cod_ccaa == "ES") {
    out <- clean_datos_min %>%
      filter(codigo_iso  == cod_ccaa) %>%
      group_by(fecha) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE)
    out <- tibble(codigo_iso = rep("ES", nrow(out)),
                  ccaa = rep("España", nrow(out))) %>%
      bind_cols(out)
  } else {
    out <- clean_datos_min %>%
      filter(codigo_iso  == cod_ccaa) %>%
      right_join(tibble(fecha = seq(from = min(clean_datos_min$fecha),
                                    to = max(clean_datos_min$fecha),
                                    by = 1)),
                 by = "fecha")
  }

  aux <- out %>% select_if(is.numeric) %>% names()
  ref_pop <- info_ccaa %>% filter(codigo_iso == cod_ccaa) %>% pull(pob)
  out <- out %>%
    mutate_if(is.numeric, list(per_100K = ~ . / ref_pop * 100000)) %>%
    mutate_if(is.numeric, list(var = ~ c(NA, diff(.)))) %>%
    mutate_at(vars((aux)),
              list(var_perc = ~ (./dplyr::lag(.) - 1) * 100)) %>%
    select(-ends_with("_100K_var_perc"))

  
  return(out)
}


jhu_ctry_data <- function(jhu_clean_data, cod_ctry) {
  
  out <- jhu_clean_data %>%
    filter(iso3c  == cod_ctry) %>%
    right_join(tibble(date = seq(from = min(jhu_clean_data$date),
                                 to = max(jhu_clean_data$date),
                                 by = 1)),
               by = "date")
  
  aux <- out %>% select_if(is.numeric) %>% names()
  
  out <- out %>%
    mutate_if(is.numeric, list(per_100K = ~ . / population * 100000)) %>%
    mutate_if(is.numeric, list(var = ~ c(NA, diff(.)))) %>%
    mutate_at(vars((aux)),
              list(var_perc = ~ (./dplyr::lag(.) - 1) * 100)) %>%
    select(-ends_with("_100K_var_perc"))
  
  
  return(out)
}



# PLOTS -------------------------------------------------------------------

hc_min_ccaa_col <- function(in_data, tgt_col, info_ccaa = tbl_ccaa,
                            # plot_type = "line", 
                            ...) {
  
  quo_tgt_col <- enquo(tgt_col)
  
  plot_data <- datos_min_ccaa_col(in_data, !!quo_tgt_col, ...)
  
  if(is.null(plot_data)) return(NULL)
  
  hc_data <- plot_data %>% 
    # gather(codigo_iso, !!quo_tgt_col, -fecha)  %>% 
    gather(codigo_iso, valor, -1)  %>% 
    mutate(codigo_iso = str_remove(codigo_iso, "_.+$")) %>% 
    left_join(info_ccaa %>% 
                select(codigo_iso, ccaa)) %>% 
    mutate(ccaa = ifelse(is.na(ccaa), "España", ccaa)) %>% 
    select(-codigo_iso)
  
  if(names(hc_data)[1] == "fecha") {
    out <- hchart(hc_data, 
                  type = "line", 
                  hcaes(x = fecha, 
                        # y = quo_name(quo_tgt_col), 
                        y = valor, 
                        group = ccaa)) %>%
      hc_chart(zoomType = "xy") %>% 
      hc_title(text = quo_name(quo_tgt_col))
  } else {
    out <- hchart(hc_data, 
                  type = "line", 
                  hcaes(x = n_day, 
                        # y = quo_name(quo_tgt_col), 
                        y = valor, 
                        group = ccaa)) %>%
      hc_chart(zoomType = "xy") %>% 
      hc_title(text = quo_name(quo_tgt_col))
  }
  
  
  out
}

hc_jhu_ctry_col <- function(in_data, tgt_col,
                            # plot_type = "line", 
                            ...) {
  
  quo_tgt_col <- enquo(tgt_col)
  
  plot_data <- jhu_ctry_data_col(in_data, !!quo_tgt_col, ...)
  
  if(is.null(plot_data)) return(NULL)
  
  info_ctries <-  in_data %>% select(country, iso3c) %>% distinct()
  
  hc_data <- plot_data %>% 
    # gather(codigo_iso, !!quo_tgt_col, -fecha)  %>% 
    gather(iso3c, value, -1)  %>% 
    mutate(iso3c = str_remove(iso3c, "_.+$")) %>% 
    left_join(info_ctries %>% 
                select(iso3c, country), by = "iso3c") %>% 
    # mutate(country = ifelse(is.na(country), "España", country)) %>% 
    select(-iso3c)
  
  if(names(hc_data)[1] == "date") {
    out <- hchart(hc_data, 
                  type = "line", 
                  hcaes(x = date, 
                        y = value, 
                        group = country)) %>%
      hc_chart(zoomType = "xy") %>% 
      hc_title(text = quo_name(quo_tgt_col))
  } else {
    out <- hchart(hc_data, 
                  type = "line", 
                  hcaes(x = n_day, 
                        # y = quo_name(quo_tgt_col), 
                        y = value, 
                        group = country)) %>%
      hc_chart(zoomType = "xy") %>% 
      hc_title(text = quo_name(quo_tgt_col))
  }
  
  
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
                "line", 
                hcaes(x = fecha, y = valor, group = metrica)) %>%
    hc_chart(zoomType = "xy") %>% 
    hc_title(text = quo_name(tgt_ccaa))
  
  out
}

hc_jhu_ctry <- function(in_data, cod_ctry, metrics = NULL) {
  
  plot_data <- jhu_ctry_data(in_data, cod_ctry) %>% 
    select(-country, -iso3c, -population, -region) %>% 
    gather(metric, value, -date) 
  
  if (!is.null(metrics)) {
    plot_data <- plot_data %>% 
      filter(metric %in% metrics)
  }
  
  out <- hchart(plot_data , 
                "line", 
                hcaes(x = date, y = value, group = metric)) %>%
    hc_chart(zoomType = "xy") %>% 
    hc_title(text = quo_name(cod_ctry))
  
  out
}
