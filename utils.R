library(tabulizer)

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
  # 44, as.Date("2020/03/14"), "OLD",   2, 1, NA, NA, NA, NA,
  # 45, as.Date("2020/03/15"), "OLD",   2, 1, NA, NA, NA, NA,
  46, as.Date("2020/03/16"), "NEW",   2, 1, NA, NA, NA, NA,
  47, as.Date("2020/03/17"), "NEW",   1, 1, NA, NA, NA, NA,
  48, as.Date("2020/03/18"), "NEW",   1, 1, NA, NA, NA, NA,
  49, as.Date("2020/03/19"), "NEW",   1, 1, NA, NA, NA, NA,
  50, as.Date("2020/03/20"), "NEW",   1, 1, NA, NA, NA, NA,
  51, as.Date("2020/03/21"), "NEW_2", 1, 1, NA, NA, NA, NA,
  52, as.Date("2020/03/22"), "NEW_2", 1, 2, NA, NA, NA, NA,
  53, as.Date("2020/03/23"), "NEW_2", 1, 1, NA, NA, NA, NA,
  54, as.Date("2020/03/24"), "NEW_2", 1, 1, NA, NA, NA, NA,
  55, as.Date("2020/03/25"), "NEW_3", 1, 1, NA, NA, NA, NA
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
