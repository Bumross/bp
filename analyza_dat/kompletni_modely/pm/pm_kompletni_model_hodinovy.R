library(tidyverse)
library(lubridate)
library(broom)
library(ggplot2)
library(ggpubr)
library(mgcv)

# zkusil jsem postupne pridavat promenne:




data <- data %>%
  mutate(
    ln_pm100 = log(data_pm100),
    t = as.numeric(difftime(cas, min(cas), units = "hours")),
    t2 = t^2,
    lag_auta_1h = lag(valid_speed_count),
    log_auta = log(valid_speed_count),
    log_auta_lag = lag(log(valid_speed_count + 1)) # +1 aby nešlo log(0)
  )

model_vars <- c(
  "ln_pm100", "t", "t2", "valid_speed_count", "lag_auta_1h",
  "data_temp1", "data_hum1", "data_pressure", "data_volumeMm", "data_windSpeed",
  "log_auta_lag", "cas", "hodina", "log_auta"
)

data_topna <- data %>%
  filter(topna_sezona == 1) %>%
  select(all_of(model_vars)) %>%
  drop_na()

data_netopna <- data %>%
  filter(topna_sezona == 0) %>%
  select(all_of(model_vars)) %>%
  drop_na()







# LINEÁRNÍ MODELY
model_topna <- lm(ln_pm100 ~ valid_speed_count +
                    data_temp1 + data_hum1 + data_pressure + data_volumeMm + data_windSpeed + as.factor(hodina),
                  data = data_topna)

model_netopna <- lm(ln_pm100 ~ lag_auta_1h +
                      data_temp1 + data_hum1 + data_volumeMm + data_windSpeed + as.factor(hodina),
                    data = data_netopna)

summary(model_topna)
summary(model_netopna)

checkresiduals(model_topna)
checkresiduals(model_netopna)

# ARIMA na rezidua
res_topna <- ts(residuals(model_topna), frequency = 24)
res_netopna <- ts(residuals(model_netopna), frequency = 24)





data_topna$pred_pm100 <- exp(predict(model_topna, newdata = data_topna))
data_topna$real_pm100 <- exp(data_topna$ln_pm100)

data_netopna$pred_pm100 <- exp(predict(model_netopna, newdata = data_netopna))
data_netopna$real_pm100 <- exp(data_netopna$ln_pm100)

# GRAFY
g1 <- ggplot(data_topna, aes(x = cas)) +
  geom_line(aes(y = real_pm100), color = "black", alpha = 0.3) +
  geom_line(aes(y = pred_pm100), color = "blue", alpha = 0.7) +
  labs(title = "Topná sezóna – Fit vs. realita (PM100)", x = "Datum", y = "PM100") +
  scale_x_datetime(date_labels = "%d/%m", date_breaks = "14 days") +
  theme_minimal()

g2 <- ggplot(data_netopna, aes(x = cas)) +
  geom_line(aes(y = real_pm100), color = "black", alpha = 0.3) +
  geom_line(aes(y = pred_pm100), color = "blue", alpha = 0.7) +
  labs(title = "Netopná sezóna – Fit vs. realita (PM100)", x = "Datum", y = "PM100") +
  scale_x_datetime(date_labels = "%d/%m", date_breaks = "14 days") +
  theme_minimal()

ggarrange(g1, g2, ncol = 1)







# GAM MODELY
gam_topna <- gam(
  ln_pm100 ~ s(lag_auta_1h) +
    s(data_temp1) + s(data_hum1) + s(data_pressure) +
    s(data_volumeMm) + s(data_windSpeed) + as.factor(hodina),
  data = data_topna
)
# auta nic v topne sezone


gam_netopna <- gam(
  ln_pm100 ~ s(valid_speed_count) +
    s(data_temp1) + s(data_hum1) +
    s(data_volumeMm) + s(data_windSpeed) + as.factor(hodina),
  data = data_netopna
)

summary(gam_topna)
summary(gam_netopna)

checkresiduals(gam_topna)
checkresiduals(gam_netopna)

# predikce GAM
data_topna$pred_pm100_gam <- exp(predict(gam_topna, newdata = data_topna))
data_netopna$pred_pm100_gam <- exp(predict(gam_netopna, newdata = data_netopna))

g1 <- ggplot(data_topna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100_gam), color = "blue", alpha = 0.7) +
  labs(title = "Topná sezóna – GAM model (PM100)", y = "PM100", x = NULL)

g2 <- ggplot(data_netopna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100_gam), color = "darkgreen", alpha = 0.7) +
  labs(title = "Netopná sezóna – GAM model (PM100)", y = "PM100", x = NULL)

ggarrange(g1, g2, ncol = 1)





#######################################
# TSLM MODELY
data_ts <- data %>%
  select(cas, ln_pm100, valid_speed_count,
         data_temp1, data_hum1, data_pressure, data_volumeMm, data_windSpeed,
         topna_sezona, t, t2, log_auta, lag_auta_1h) %>%
  as_tsibble(index = cas)

data_topna <- filter(data_ts, topna_sezona == 1)
data_netopna <- filter(data_ts, topna_sezona == 0)

model_topna_all <- data_topna %>%
  model(
    tslm = TSLM(ln_pm100 ~ trend() + season("day") +
                  log_auta +
                  data_temp1 + data_hum1 + data_pressure +
                  data_volumeMm + data_windSpeed)
  )

model_netopna_all <- data_netopna %>%
  model(
    tslm = TSLM(ln_pm100 ~ t + t2 + season("day") +
                  lag_auta_1h +
                  data_temp1 + data_hum1 + data_pressure +
                  data_volumeMm + data_windSpeed)
  )

report(model_topna_all)
report(model_netopna_all)

fitted_topna_all <- fitted(model_topna_all) %>% as_tibble()
fitted_netopna_all <- fitted(model_netopna_all) %>% as_tibble()

data_topna <- data_topna %>%
  mutate(pred_pm100_tslm = exp(fitted_topna_all$.fitted))

data_netopna <- data_netopna %>%
  mutate(pred_pm100_tslm = exp(fitted_netopna_all$.fitted))

g1 <- ggplot(data_topna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100_tslm), color = "blue", alpha = 0.7) +
  labs(title = "Topná sezóna – TSLM model (fit, PM100)", y = "PM100", x = NULL)

g2 <- ggplot(data_netopna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100_tslm), color = "darkgreen", alpha = 0.7) +
  labs(title = "Netopná sezóna – TSLM model (fit, PM100)", y = "PM100", x = NULL)

g1 / g2

combined_data <- bind_rows(data_topna, data_netopna)

ggplot(combined_data, aes(x = cas)) +
  geom_line(aes(y = exp(ln_pm100), color = "Realita"), alpha = 0.3) +
  geom_line(aes(y = pred_pm100_tslm, color = "Predikce"), alpha = 0.7) +
  scale_color_manual(values = c("Realita" = "black", "Predikce" = "blue")) +
  labs(
    title = "Fit vs. realita koncentrace PM10 v čase",
    x = "Datum", y = "PM10 [µg/m³]", color = ""
  ) +
  scale_x_datetime(date_labels = "%d/%m", date_breaks = "1 month") +
  theme_minimal()
