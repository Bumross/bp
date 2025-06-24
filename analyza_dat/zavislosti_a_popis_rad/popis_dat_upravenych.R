# nutno mit nactena data z hlavni slozky nacteni_dat.R

continuous_vars <- c(
  "valid_speed_count",
  "data_pm100",
  "data_no2",
  "data_temp1",
  "data_hum1",
  "data_pressure",
  "data_windSpeed",
  "data_windImpact",
  "data_volumeMm",
  "prumerna_teplota_dne"
)


summary(data[ , continuous_vars])

sapply(data[ , continuous_vars], is.numeric)

# Počet platných hodnot (po agregaci bývá více NA)
colSums(!is.na(data[ , continuous_vars]))

# Průměrná denní rychlost aut (pokud tě zajímá intenzita provozu)
mean(data$valid_speed_count, na.rm = TRUE)

# Korelace mezi valid_speed_count a data_pm100 / data_no2
cor(data$valid_speed_count, data$data_pm100, use = "complete.obs")
cor(data$valid_speed_count, data$data_no2, use = "complete.obs")




nad_25_cervenec <- data %>%
  filter(month(cas) == 7) %>%
  select(cas, data_pm100)

# Výpis počtu
nrow(nad_25_cervenec)

# Výpis všech dat s hodnotami
print(nad_25_cervenec, n=300)
