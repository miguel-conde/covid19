# library(tabulizer)

# library(INEbaseR)

source("global.R", encoding = "UTF8")


# FUNCTIONS ---------------------------------------------------------------


# Johns Hopkins Data ------------------------------------------------------





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






# PLOTS -------------------------------------------------------------------

hc_min_ccaa_col <- function(in_data, tgt_col, info_ccaa = tbl_ccaa,
                            # plot_type = "line", 
                            ...) {
  
  quo_tgt_col <- enquo(tgt_col)
  
  # print(paste("looking at", quo_name(quo_tgt_col)))
  
  plot_data <- datos_min_ccaa_col(in_data, !!quo_tgt_col, ...)
  # plot_data <- datos_min_ccaa_col(in_data, tgt_col, ...)
  
  hc_data <- plot_data %>% 
    gather(codigo_iso, !!quo_tgt_col, -fecha)  %>% 
    mutate(codigo_iso = str_remove(codigo_iso, "_.+$")) %>% 
    left_join(info_ccaa %>% 
                select(codigo_iso, ccaa)) %>% 
    mutate(ccaa = ifelse(is.na(ccaa), "España", ccaa)) %>% 
    select(-codigo_iso)
  
  out <- hchart(hc_data, 
                type = "line", 
                hcaes(x = fecha, 
                      y = quo_name(quo_tgt_col), 
                      group = ccaa)) %>%
    hc_chart(zoomType = "xy") # %>% 
  # hc_title(text = quo_name(quo_tgt_col))
  
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
