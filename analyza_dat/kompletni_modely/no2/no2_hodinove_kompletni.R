library(tidyverse)
library(lubridate)
library(broom)
library(ggplot2)
library(ggpubr)
library(mgcv)
library(fable)
library(tsibble)
library(fabletools)




##### příprava dat
data <- data %>%
  mutate(
    ln_no2 = log(data_no2),
    t = as.numeric(difftime(cas, min(cas), units = "hours")),
    t2 = t^2,
    lag_auta_1h = lag(valid_speed_count),
    log_auta_lag = lag(log_auta)
  )

model_vars <- c(
  "ln_no2", "t", "t2", "valid_speed_count", "lag_auta_1h",
  "data_temp1", "data_hum1", "data_pressure", "data_volumeMm", "data_windSpeed",
  "log_auta", "cas", "log_auta_lag",
  "hodina"
)




data_topna <- data %>%
  filter(topna_sezona == 1) %>%
  select(all_of(model_vars)) %>%
  drop_na()

data_netopna <- data %>%
  filter(topna_sezona == 0) %>%
  select(all_of(model_vars)) %>%
  drop_na()

# lineární modely
model_topna <- lm(ln_no2 ~ log_auta +
                    data_temp1 + data_hum1 + data_pressure + data_volumeMm + data_windSpeed + as.factor(hodina),
                  data = data_topna)

model_netopna <- lm(ln_no2 ~ lag_auta_1h +
                      data_temp1 + data_hum1  + data_volumeMm + data_windSpeed + as.factor(hodina),
                    data = data_netopna)
# cas na netopny nehraje vubec smysl, davam ho pryc (t a t2)
# stejne tak v netopne sezone dam pryc pressure
# pridal jsem hodinu jako faktor - vyrazne snizila v checkresiduals autokorelaci
# a snizila i sezonni korelaci

# SHRNUTÍ
summary(model_topna)
summary(model_netopna)

# DIAGNOSTIKA
par(mfrow = c(2, 4))
plot(model_topna, main = "Topná sezóna – diagnostika")
plot(model_netopna, main = "Netopná sezóna – diagnostika")
par(mfrow = c(1, 1))




# kontrola reziduí
checkresiduals(model_topna)
checkresiduals(model_netopna)




# ARIMA na rezidua
res_topna <- ts(residuals(model_topna), frequency = 24)
res_netopna <- ts(residuals(model_netopna), frequency = 24)

arima_topna <- auto.arima(res_topna, stepwise = TRUE, approximation = TRUE)
arima_netopna <- auto.arima(res_netopna, stepwise = TRUE, approximation = TRUE)


summary(arima_topna)
summary(arima_netopna)


checkresiduals(arima_topna)
checkresiduals(arima_netopna)





# PREDIKCE
data_topna$pred_no2 <- exp(predict(model_topna, newdata = data_topna))
data_topna$real_no2 <- exp(data_topna$ln_no2)

data_netopna$pred_no2 <- exp(predict(model_netopna, newdata = data_netopna))
data_netopna$real_no2 <- exp(data_netopna$ln_no2)

# GRAFY
g1 <- ggplot(data_topna, aes(x = cas)) +
  geom_line(aes(y = real_no2), color = "black", alpha = 0.3) +
  geom_line(aes(y = pred_no2), color = "blue", alpha = 0.7) +
  labs(title = "Topná sezóna – Fit vs. realita", x = "Datum", y = "NO2") +
  scale_x_datetime(date_labels = "%d/%m", date_breaks = "14 days") + theme_minimal()

g2 <- ggplot(data_netopna, aes(x = cas)) +
  geom_line(aes(y = real_no2), color = "black", alpha = 0.3) +
  geom_line(aes(y = pred_no2), color = "blue", alpha = 0.7) +
  labs(title = "Netopná sezóna – Fit vs. realita", x = "Datum", y = "NO2") +
  scale_x_datetime(date_labels = "%d/%m", date_breaks = "14 days") + theme_minimal()

ggarrange(g1, g2, ncol = 1)



combined_data <- bind_rows(data_topna, data_netopna)

ggplot(combined_data, aes(x = cas)) +
  geom_line(aes(y = real_no2, color = "Realita"), alpha = 0.3) +
  geom_line(aes(y = pred_no2, color = "Predikce"), alpha = 0.7) +
  scale_color_manual(values = c("Realita" = "black", "Predikce" = "blue")) +
  labs(
    title = "Fit vs. realita koncentrace NO2 v čase",
    x = "Datum", y = "NO2 [µg/m³]", color = ""
  ) +
  scale_x_datetime(date_labels = "%d/%m", date_breaks = "1 month") +
  theme_minimal()

############################################################################
# GAM modely
data <- data %>%
  mutate(
    ln_no2 = log(data_no2),
    t = as.numeric(difftime(cas, min(cas), units = "hours")),
    t2 = t^2,
    lag_auta_1h = lag(valid_speed_count)
  )

model_vars <- c(
  "ln_no2", "t", "t2", "valid_speed_count", "lag_auta_1h", "log_auta",
  "data_temp1", "data_hum1", "data_pressure", "data_volumeMm", "data_windSpeed"
)



data_topna <- data %>%
  filter(topna_sezona == 1) %>%
  select(all_of(model_vars), cas) %>%
  drop_na()

data_netopna <- data %>%
  filter(topna_sezona == 0) %>%
  select(all_of(model_vars), cas) %>%
  drop_na()



# GAM
# topná sezóna
gam_topna <- gam(
  ln_no2 ~ s(log_auta) +
    s(data_temp1) +
    s(data_hum1) +
    s(data_pressure) +
    s(data_volumeMm) +
    s(data_windSpeed) +
    as.factor(hodina),
  data = data_topna
)

# netopná sezóna
gam_netopna <- gam(
  ln_no2 ~ 
    s(lag_auta_1h) +
    s(data_temp1) +
    s(data_hum1) +
    s(data_volumeMm) +
    s(data_windSpeed) +
    as.factor(hodina),
  data = data_netopna
)

summary(gam_topna)
summary(gam_netopna)

checkresiduals(gam_topna)
checkresiduals(gam_netopna)



# ARIMA na rezidua
res_topna_gam <- ts(residuals(gam_topna), frequency = 24)
res_netopna_gam <- ts(residuals(gam_netopna), frequency = 24)

arima_topna_gam <- auto.arima(res_topna, stepwise = TRUE, approximation = TRUE)
arima_netopna_gam <- auto.arima(res_netopna, stepwise = TRUE, approximation = TRUE)


summary(arima_topna)
summary(arima_netopna)


checkresiduals(arima_topna)
checkresiduals(arima_netopna)





# predikce
data_topna$pred_no2_gam <- exp(predict(gam_topna, newdata = data_topna))
data_netopna$pred_no2_gam <- exp(predict(gam_netopna, newdata = data_netopna))

g1 <- ggplot(data_topna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_no2)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_no2_gam), color = "blue", alpha = 0.7) +
  labs(title = "Topná sezóna – GAM model", y = "NO2", x = NULL)

g2 <- ggplot(data_netopna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_no2)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_no2_gam), color = "darkgreen", alpha = 0.7) +
  labs(title = "Netopná sezóna – GAM model", y = "NO2", x = NULL)

ggarrange(g1, g2, ncol = 1)



########################
# test vs train
data <- data %>%
  mutate(
    ln_no2 = log(data_no2),
    time_index = as.numeric(difftime(cas, min(cas), units = "hours")),
    time_index2 = time_index^2,
    lag_auta_1h = lag(valid_speed_count)
  ) %>%
  filter(!is.na(ln_no2), !is.na(lag_auta_1h))

model_vars <- c(
  "ln_no2", "time_index", "time_index2", "valid_speed_count", "lag_auta_1h",
  "data_temp1", "data_hum1", "data_pressure", "data_volumeMm", "data_windSpeed"
)

data_topna <- data %>%
  filter(topna_sezona == 1) %>%
  select(all_of(model_vars), cas)

data_netopna <- data %>%
  filter(topna_sezona == 0) %>%
  select(all_of(model_vars), cas)

train_topna <- data_topna %>% slice(1:floor(0.7 * n()))
test_topna <- data_topna %>% slice((floor(0.7 * n()) + 1):n())

train_netopna <- data_netopna %>% slice(1:floor(0.7 * n()))
test_netopna <- data_netopna %>% slice((floor(0.7 * n()) + 1):n())

train_topna <- train_topna %>% drop_na()
train_netopna <- train_netopna %>% drop_na()

gam_topna <- gam(ln_no2 ~ s(valid_speed_count) + s(lag_auta_1h) +
                   s(data_temp1) + s(data_hum1) + s(data_pressure) +
                   s(data_volumeMm) + s(data_windSpeed),
                 data = train_topna)

gam_netopna <- gam(ln_no2 ~ s(time_index) + s(valid_speed_count) +
                     s(data_temp1) + s(data_hum1) + s(data_pressure) +
                     s(data_volumeMm) + s(data_windSpeed),
                   data = train_netopna)


summary(gam_topna)
summary(gam_netopna)

checkresiduals(gam_topna)
checkresiduals(gam_netopna)

test_topna$pred_no2_gam <- exp(predict(gam_topna, newdata = test_topna))
test_netopna$pred_no2_gam <- exp(predict(gam_netopna, newdata = test_netopna))

g1 <- ggplot(test_topna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_no2)), color = "black", alpha = 0.6) +
  geom_line(aes(y = pred_no2_gam), color = "blue", alpha = 0.8) +
  labs(title = "Topná sezóna – GAM model (predikce na testovacích datech)", y = "NO2", x = NULL) +
  theme_minimal()

g2 <- ggplot(test_netopna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_no2)), color = "black", alpha = 0.6) +
  geom_line(aes(y = pred_no2_gam), color = "darkgreen", alpha = 0.8) +
  labs(title = "Netopná sezóna – GAM model (predikce na testovacích datech)", y = "NO2", x = NULL) +
  theme_minimal()

ggarrange(g1, g2, ncol = 1)

##############################################################################
# TSLM
data <- data %>%
  filter(!is.na(cas)) %>%
  arrange(cas) %>%
  distinct(cas, .keep_all = TRUE) %>%
  mutate(
    ln_no2 = log(data_no2),
    t = as.numeric(difftime(cas, min(cas), units = "hours")),
    t2 = t^2
  )

data_ts <- data %>%
  select(cas, ln_no2, valid_speed_count,
         data_temp1, data_hum1, data_pressure, data_volumeMm, data_windSpeed,
         topna_sezona, t, t2) %>%
  as_tsibble(index = cas)

data_topna <- filter(data_ts, topna_sezona == 1)
data_netopna <- filter(data_ts, topna_sezona == 0)

model_topna_all <- data_topna %>%
  model(
    tslm = TSLM(ln_no2 ~ trend() + season("day") +
                  valid_speed_count +
                  data_temp1 + data_hum1 + data_pressure +
                  data_volumeMm + data_windSpeed)
  )

model_netopna_all <- data_netopna %>%
  model(
    tslm = TSLM(ln_no2 ~ t + t2 + season("day") +
                  valid_speed_count +
                  data_temp1 + data_hum1 + data_pressure +
                  data_volumeMm + data_windSpeed)
  )

report(model_topna_all)
report(model_netopna_all)

fitted_topna_all <- fitted(model_topna_all) %>% as_tibble()
fitted_netopna_all <- fitted(model_netopna_all) %>% as_tibble()

data_topna <- data_topna %>%
  mutate(pred_no2_tslm = exp(fitted_topna_all$.fitted))

data_netopna <- data_netopna %>%
  mutate(pred_no2_tslm = exp(fitted_netopna_all$.fitted))

g1 <- ggplot(data_topna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_no2)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_no2_tslm), color = "blue", alpha = 0.7) +
  labs(title = "Topná sezóna – TSLM model (fit)", y = "NO2", x = NULL)

g2 <- ggplot(data_netopna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_no2)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_no2_tslm), color = "darkgreen", alpha = 0.7) +
  labs(title = "Netopná sezóna – TSLM model (fit)", y = "NO2", x = NULL)

g1 / g2

# TEST VS TRAIN
data <- data %>%
  filter(!is.na(cas)) %>%
  arrange(cas) %>%
  distinct(cas, .keep_all = TRUE) %>%
  mutate(
    ln_no2 = log(data_no2)
  )

data_ts <- data %>%
  select(cas, ln_no2, valid_speed_count,
         data_temp1, data_hum1, data_pressure, data_volumeMm, data_windSpeed,
         topna_sezona) %>%
  as_tsibble(index = cas) %>%
  mutate(
    t = as.numeric(difftime(cas, min(cas), units = "hours")),
    t2 = t^2
  )

data_topna <- filter(data_ts, topna_sezona == 1)
data_netopna <- filter(data_ts, topna_sezona == 0)

split_ts_data <- function(data_season) {
  n <- nrow(data_season)
  split_point <- floor(0.7 * n)
  
  train_df <- data_season %>% slice(1:split_point)
  test_df <- data_season %>% slice((split_point + 1):n)
  
  list(
    train = as_tsibble(train_df, index = cas),
    test = as_tsibble(test_df, index = cas)
  )
}

topna_split <- split_ts_data(data_topna)
netopna_split <- split_ts_data(data_netopna)

model_topna <- topna_split$train %>%
  model(
    tslm = TSLM(ln_no2 ~ trend() + season("day") +
                  valid_speed_count +
                  data_temp1 + data_hum1 + data_pressure +
                  data_volumeMm + data_windSpeed)
  )

model_netopna <- netopna_split$train %>%
  model(
    tslm = TSLM(ln_no2 ~ t + t2 + season("day") +
                  valid_speed_count +
                  data_temp1 + data_hum1 + data_pressure +
                  data_volumeMm + data_windSpeed)
  )

forecast_topna <- forecast(model_topna, new_data = topna_split$test)
forecast_netopna <- forecast(model_netopna, new_data = netopna_split$test)

data_topna_forecasted <- topna_split$test %>%
  bind_cols(pred_no2_tslm = exp(forecast_topna$.mean))

data_netopna_forecasted <- netopna_split$test %>%
  bind_cols(pred_no2_tslm = exp(forecast_netopna$.mean))

g1 <- ggplot(data_topna_forecasted, aes(x = cas)) +
  geom_line(aes(y = exp(ln_no2)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_no2_tslm), color = "blue", alpha = 0.7) +
  labs(title = "Topná sezóna – TSLM model (predikce na testovacích datech)", y = "NO2", x = NULL)

g2 <- ggplot(data_netopna_forecasted, aes(x = cas)) +
  geom_line(aes(y = exp(ln_no2)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_no2_tslm), color = "darkgreen", alpha = 0.7) +
  labs(title = "Netopná sezóna – TSLM model (predikce na testovacích datech)", y = "NO2", x = NULL)

g1 / g2





















data_topna_export <- data_topna %>%
  select(cas, ln_no2) %>%
  mutate(
    fitted_no2 = exp(as.numeric(fitted_topna_all$.fitted)),
    sezona = 1,
    data_no2 = exp(ln_no2)
  )

# přidat predikce do netopné sezóny
data_netopna_export <- data_netopna %>%
  select(cas, ln_no2) %>%
  mutate(
    fitted_no2 = exp(as.numeric(fitted_netopna_all$.fitted)),
    sezona = 0,
    data_no2 = exp(ln_no2)
  )

# spojit
data_spojene <- bind_rows(data_topna_export, data_netopna_export)

# uložit do csv
write.csv(data_spojene, "fitted_all.csv", row.names = FALSE)
