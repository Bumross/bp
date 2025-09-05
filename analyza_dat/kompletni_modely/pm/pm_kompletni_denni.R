data_daily <- data %>%
  mutate(date = as_date(cas)) %>%
  group_by(date) %>%
  summarise(
    pm100 = mean(data_pm100, na.rm = TRUE),
    valid_speed_count = sum(valid_speed_count, na.rm = TRUE),
    data_temp1 = mean(data_temp1, na.rm = TRUE),
    data_hum1 = mean(data_hum1, na.rm = TRUE),
    data_pressure = mean(data_pressure, na.rm = TRUE),
    data_volumeMm = sum(data_volumeMm, na.rm = TRUE),
    data_windSpeed = mean(data_windSpeed, na.rm = TRUE),
    topna_sezona = max(topna_sezona)  # pro den platí pokud v jakékoli hodině byl topná_sezona==1
  ) %>%
  ungroup() %>%
  mutate(
    ln_pm100 = log(pm100),
    t = as.numeric(difftime(date, min(date), units = "days")),
    t2 = t^2,
    lag_auta_1d = lag(valid_speed_count),
    log_auta = log(valid_speed_count + 1),  # +1 aby šlo log(0)
    den_v_tydnu = wday(date, week_start = 1)
  ) %>%
  drop_na()



data_topna <- filter(data_daily, topna_sezona == 1)
data_netopna <- filter(data_daily, topna_sezona == 0)

## LINEÁRNÍ MODELY
model_topna <- lm(ln_pm100 ~ log_auta +
                    data_temp1 + data_hum1 + data_volumeMm + data_windSpeed,
                  data = data_topna)

model_netopna <- lm(ln_pm100 ~ log_auta +
                      data_temp1  + data_pressure + data_windSpeed ,
                    data = data_netopna)

summary(model_topna)
summary(model_netopna)






###################
gam_topna <- gam(
ln_pm100 ~ s(log_auta) +
  s(data_temp1) + s(data_hum1) +
  s(data_volumeMm) + s(data_windSpeed),
data = data_topna
)

# GAM netopna
gam_netopna <- gam(
  ln_pm100 ~ s(log_auta) +
    s(data_temp1) +
    s(data_pressure) + s(data_windSpeed),
  data = data_netopna
)

summary(gam_topna)
summary(gam_netopna)

checkresiduals(gam_topna)
checkresiduals(gam_netopna)

# predikce
data_topna$pred_pm100_gam <- exp(predict(gam_topna, newdata = data_topna))
data_netopna$pred_pm100_gam <- exp(predict(gam_netopna, newdata = data_netopna))

# graf
g1 <- ggplot(data_topna, aes(x = date)) +
  geom_line(aes(y = pm100), color = "black", alpha = 0.4) +
  geom_line(aes(y = pred_pm100_gam), color = "blue", alpha = 0.7) +
  labs(title = "Topná sezóna – GAM model", y = "PM100")

g2 <- ggplot(data_netopna, aes(x = date)) +
  geom_line(aes(y = pm100), color = "black", alpha = 0.4) +
  geom_line(aes(y = pred_pm100_gam), color = "darkgreen", alpha = 0.7) +
  labs(title = "Netopná sezóna – GAM model", y = "PM100")

ggarrange(g1, g2, ncol = 1)


#########################################
data_ts <- data_daily %>%
  select(date, ln_pm100, log_auta,
         data_temp1, data_hum1, data_pressure, data_volumeMm, data_windSpeed,
         topna_sezona, t, t2) %>%
  as_tsibble(index = date)

data_topna <- filter(data_ts, topna_sezona == 1)
data_netopna <- filter(data_ts, topna_sezona == 0)

# TSLM topna
model_topna_tslm <- data_topna %>%
  model(
    tslm = TSLM(ln_pm100 ~ trend() +
                  log_auta +
                  data_temp1 + data_hum1 + data_volumeMm + data_windSpeed)
  )

# TSLM netopna
model_netopna_tslm <- data_netopna %>%
  model(
    tslm = TSLM(ln_pm100 ~ trend()  +
                  log_auta +
                  data_temp1 + data_pressure + data_windSpeed)
  )

report(model_topna_tslm)
report(model_netopna_tslm)

# fitted
fitted_topna <- fitted(model_topna_tslm) %>% as_tibble()
fitted_netopna <- fitted(model_netopna_tslm) %>% as_tibble()

data_topna <- data_topna %>%
  mutate(pred_pm100_tslm = exp(fitted_topna$.fitted))

data_netopna <- data_netopna %>%
  mutate(pred_pm100_tslm = exp(fitted_netopna$.fitted))

# graf
g1 <- ggplot(data_topna, aes(x = date)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100_tslm), color = "blue", alpha = 0.7) +
  labs(title = "Topná sezóna – TSLM model", y = "PM100")

g2 <- ggplot(data_netopna, aes(x = date)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100_tslm), color = "darkgreen", alpha = 0.7) +
  labs(title = "Netopná sezóna – TSLM model", y = "PM100")

g1 / g2
