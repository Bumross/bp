data_daily <- data %>%
  mutate(date = as_date(cas)) %>%
  group_by(date) %>%
  summarise(
    no2 = mean(data_no2, na.rm = TRUE),
    valid_speed_count = sum(valid_speed_count, na.rm = TRUE),
    data_temp1 = mean(data_temp1, na.rm = TRUE),
    data_hum1 = mean(data_hum1, na.rm = TRUE),
    data_pressure = mean(data_pressure, na.rm = TRUE),
    data_volumeMm = sum(data_volumeMm, na.rm = TRUE),
    data_windSpeed = mean(data_windSpeed, na.rm = TRUE),
    topna_sezona = max(topna_sezona)
  ) %>%
  ungroup() %>%
  mutate(
    ln_no2 = log(no2 + 0.1), # malý offset pro nulové hodnoty
    t = as.numeric(difftime(date, min(date), units = "days")),
    t2 = t^2,
    lag_auta_1d = lag(valid_speed_count),
    log_auta = log(valid_speed_count + 1),
    den_v_tydnu = wday(date, week_start = 1)
  ) %>%
  drop_na()

# ROZDĚLENÍ NA SEZÓNY
data_topna <- data_daily %>% filter(topna_sezona == 1)
data_netopna <- data_daily %>% filter(topna_sezona == 0)

# LINEÁRNÍ MODELY
lm_topna <- lm(ln_no2 ~ log_auta +
                 data_temp1 + data_hum1 + data_pressure +
                 data_volumeMm + data_windSpeed,
               data = data_topna)

lm_netopna <- lm(ln_no2 ~ log_auta +
                   data_temp1 + data_hum1 +
                   data_volumeMm + data_windSpeed,
                 data = data_netopna)

summary(lm_topna)
summary(lm_netopna)



###-----------GAM-----------------
gam_topna <- gam(
  ln_no2 ~ s(log_auta) +
    s(data_temp1) + s(data_hum1) + s(data_pressure) +
    s(data_volumeMm) + s(data_windSpeed),
  data = data_topna
)

gam_netopna <- gam(
  ln_no2 ~ s(log_auta) +
    s(data_temp1) + s(data_hum1) +
    s(data_volumeMm) + s(data_windSpeed),
  data = data_netopna
)

summary(gam_topna)
summary(gam_netopna)



##############################################
data_ts <- data_daily %>%
  as_tsibble(index = date)

model_topna_tslm <- data_ts %>%
  filter(topna_sezona == 1) %>%
  model(
    tslm = TSLM(ln_no2 ~ trend()  +
                  log_auta + data_temp1 + data_hum1 + data_pressure +
                  data_volumeMm + data_windSpeed)
  )

model_netopna_tslm <- data_ts %>%
  filter(topna_sezona == 0) %>%
  model(
    tslm = TSLM(ln_no2 ~ trend() +
                  log_auta + data_temp1 + data_hum1 +
                  data_volumeMm + data_windSpeed)
  )

report(model_topna_tslm)
report(model_netopna_tslm)


