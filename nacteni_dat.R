# hodinova data
data_path <- "~\\bo_data_analysis\\data\\data_procesovane\\spojena_data_upravena.rds"

data <- readRDS(data_path)


# denni data
data_denni_path <- "~\\bo_data_analysis\\data\\data_procesovane\\data_denni.rds"

data_denni <- readRDS(data_denni_path)





# vytvoreni dennich dat

# data_denni <- data %>%
#   group_by(datum) %>%
#   summarise(
#     datum = first(datum),
#     valid_speed_count = sum(valid_speed_count, na.rm = TRUE),
#     den_v_tydnu = first(den_v_tydnu),
#     data_pm100 = mean(data_pm100, na.rm = TRUE),
#     data_no2 = mean(data_no2, na.rm = TRUE),
#     data_hum1 = mean(data_hum1, na.rm = TRUE),
#     data_pressure = mean(data_pressure, na.rm = TRUE),
#     data_windSpeed = mean(data_windSpeed, na.rm = TRUE),
#     data_windImpact = mean(data_windImpact, na.rm = TRUE),
#     data_volumeMm = mean(data_volumeMm, na.rm = TRUE),
#     month = first(month),
#     letni_prazdniny = first(letni_prazdniny),
#     topna_sezona = first(topna_sezona),
#     statni_svatky = first(statni_svatky),
#     prumerna_teplota_dne = first(prumerna_teplota_dne),
#     velke_svatky = first(velke_svatky)
#   ) %>%
#   ungroup()
# 
# if (is.numeric(data_denni$datum)) {
#   data_denni <- data_denni %>%
#     mutate(datum = as.Date(datum, origin = "1970-01-01"))
# }
# 
# data_denni <- data_denni %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))
# 
# 
# saveRDS(data_denni, file = "~\\bo_data_analysis\\data\\data_procesovane\\data_denni.rds")
