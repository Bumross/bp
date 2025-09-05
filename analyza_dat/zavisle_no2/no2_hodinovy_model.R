

library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(mgcv)
library(ggplot2)
library(patchwork)

data_model <- data %>%
  filter(!is.na(cas), !is.na(data_no2), is.finite(data_no2)) %>%
  mutate(
    hodina = hour(cas),
    den_v_tydnu = factor(wday(cas, label = TRUE, week_start = 1)),
    den_v_roce = yday(cas),
    sin_day = sin(2 * pi * hodina / 24),
    cos_day = cos(2 * pi * hodina / 24),
    sin_year = sin(2 * pi * den_v_roce / 365),
    cos_year = cos(2 * pi * den_v_roce / 365),
    log_no2 = log(data_no2)
  )


# Převod na tsibble
data_ts <- data_model %>%
  arrange(cas) %>%
  as_tsibble(index = cas)


ccf_temp <- ccf(data_ts$log_no2, data_ts$data_temp1, lag.max = 100, na.action = na.pass,
                main = "CCF – NO₂ vs Teplota")

ccf_auta <- ccf(data_ts$log_no2, data_ts$valid_speed_count, lag.max = 100, na.action = na.pass,
                main = "CCF – NO₂ vs Počet aut")



# === TSLM MODEL ===
model_tslm <- data_ts %>%
  model(
    tslm = TSLM(log_no2 ~ trend() + season("day") +
                  data_temp1 + valid_speed_count)
  )

report(model_tslm)
resid <- residuals(model_tslm)

resid_vec <- resid %>% as_tibble() %>% pull(.resid)
checkresiduals(ts(resid_vec, frequency = 24))   # denní sezónnost

ts_resid <- ts(resid_vec, frequency = 24)
arima <- auto.arima(ts_resid, approximation = TRUE, stepwise = TRUE)
summary(arima)




auta <- auta_hodinova
teplota <- teplota_hodinova

start <- as.POSIXct("2025-01-16 01:00:00", tz="CET")
end   <- as.POSIXct("2025-01-29 23:00:00", tz="CET")

auta$cas <- ifelse(
  nchar(auta$cas) == 10,                  # yyyy-mm-dd
  paste0(auta$cas, " 00:00:00"),
  auta$cas
)
auta$cas <- as.POSIXct(auta$cas, tz="CET")

teplota$cas <- ifelse(
  nchar(teplota$cas) == 10,
  paste0(teplota$cas, " 00:00:00"),
  teplota$cas
)
teplota$cas <- as.POSIXct(teplota$cas, tz="CET")

auta_pred <- subset(auta, cas >= start & cas <= end)
teplota_pred <- subset(teplota, cas >= start & cas <= end)


future_data <- auta_pred %>%
  select(cas, valid_speed_count = pred_14dni) %>%
  inner_join(
    teplota_pred %>% select(cas, data_temp1 = pred_30),
    by = "cas"
  ) %>%
  arrange(cas)

future_data <- future_data %>%
  mutate(
    hodina = hour(cas),
    den_v_tydnu = factor(wday(cas, label = TRUE, week_start = 1)),
    den_v_roce = yday(cas),
    sin_day = sin(2 * pi * hodina / 24),
    cos_day = cos(2 * pi * hodina / 24),
    sin_year = sin(2 * pi * den_v_roce / 365),
    cos_year = cos(2 * pi * den_v_roce / 365)
  ) %>%
  as_tsibble(index = cas)

# PŘEDPOVĚĎ
fc <- forecast(model_tslm, new_data = future_data)

# pro přepočet zpět z log(NO₂) na NO₂
fc_df <- fc %>%
  hilo(level = 95) %>%
  as_tibble() %>%
  mutate(
    lower = exp(`95%`$lower),
    upper = exp(`95%`$upper),
    mean = exp(.mean)
  )


start_pred <- as.POSIXct("2025-01-16 01:00:00", tz="CET")

historical <- data_ts %>%
  filter(cas >= start_pred - lubridate::days(14),
         cas < start_pred) %>%
  as_tibble() %>%
  mutate(
    type = "Skutečnost",
    value = exp(log_no2)
  ) %>%
  select(cas, value, type)

fc_df <- fc %>%
  hilo(level = 95) %>%
  as_tibble() %>%
  mutate(
    lower = exp(`95%`$lower),
    upper = exp(`95%`$upper),
    value = exp(.mean),
    type = "Predikce"
  ) %>%
  select(cas, value, lower, upper, type)

combined <- bind_rows(
  historical %>% mutate(lower = NA, upper = NA),
  fc_df
)

# GRAF
ggplot(combined, aes(x = cas)) +
  geom_ribbon(
    data = combined %>% filter(type == "Predikce"),
    aes(ymin = lower, ymax = upper),
    fill = "lightblue", alpha = 0.5
  ) +
  geom_line(aes(y = value, color = type)) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce" = "blue")) +
  labs(
    x = "Čas",
    y = "NO2",
    color = ""
  ) +
  theme_minimal() +
  scale_x_datetime(date_labels = "%d/%m/%Y")









# Fitted values TSLM
fitted_tslm <- fitted(model_tslm) %>%
  as_tibble() %>%
  select(cas, .fitted) %>%
  rename(fit_no2_tslm = .fitted) %>%
  mutate(fit_no2_tslm = exp(fit_no2_tslm))

data_ts <- data_ts %>%
  left_join(fitted_tslm, by = "cas")

# === GAM MODEL ===
data_df <- as_tibble(data_ts) %>%
  mutate(den_v_tydnu = factor(den_v_tydnu)) %>%
  drop_na(log_no2, sin_day, cos_day, sin_year, cos_year,
          data_temp1, valid_speed_count)

model_gam <- gam(log_no2 ~ 
                   s(sin_day, bs = "cc") +
                   s(cos_day, bs = "cc") +
                   s(sin_year, bs = "cc") +
                   s(cos_year, bs = "cc") +
                   s(den_v_tydnu, bs = "re") +
                   s(data_temp1) +
                   s(valid_speed_count),
                 data = data_df,
                 method = "REML")

summary(model_gam)

data_df$fit_no2_gam <- exp(fitted(model_gam))

g1 <- ggplot(data_ts, aes(x = cas)) +
  geom_line(aes(y = exp(log_no2)), color = "black", alpha = 0.5, linewidth = 0.4) +
  geom_line(aes(y = fit_no2_tslm), color = "blue", alpha = 0.6, linewidth = 0.4) +
  labs(title = "NO2 – Časový vývoj (TSLM)", y = "NO₂", x = NULL) +
  scale_x_datetime(date_labels = "%d/%m/%Y") +
  theme_minimal()

# GAM: časový průběh s formátováním osy X
g2 <- ggplot(data_df, aes(x = cas)) +
  geom_line(aes(y = exp(log_no2)), color = "black", alpha = 0.5, linewidth = 0.4) +
  geom_line(aes(y = fit_no2_gam), color = "darkgreen", alpha = 0.6, linewidth = 0.4) +
  labs(title = "NO2 – Časový vývoj (GAM)", y = "NO₂", x = NULL) +
  scale_x_datetime(date_labels = "%d/%m/%Y") +
  theme_minimal()

g1 / g2



summary(model_gam)
report(model_tslm)
















# Transformace
data_model <- data %>%
  filter(!is.na(cas), !is.na(data_no2), is.finite(data_no2)) %>%
  mutate(
    hodina = hour(cas),
    den_v_tydnu = factor(wday(cas, label = TRUE, week_start = 1)),
    den_v_roce = yday(cas),
    sin_day = sin(2 * pi * hodina / 24),
    cos_day = cos(2 * pi * hodina / 24),
    sin_year = sin(2 * pi * den_v_roce / 365),
    cos_year = cos(2 * pi * den_v_roce / 365),
    log_no2 = log(data_no2)
  )


# Uspořádání a převod na tsibble
data_ts <- data_model %>%
  arrange(cas) %>%
  as_tsibble(index = cas)

# Rozdělení 70/30
n <- nrow(data_ts)
split_point <- floor(0.7 * n)
train_ts <- data_ts %>% slice(1:split_point)
test_ts <- data_ts %>% slice((split_point + 1):n)



model_tslm <- train_ts %>%
  model(
    tslm = TSLM(log_no2 ~ trend() + season("day") +
                  data_temp1 + valid_speed_count)
  )

report(model_tslm)

# Predikce
forecast_tslm <- forecast(model_tslm, new_data = test_ts)
test_tslm <- test_ts %>%
  mutate(pred_no2_tslm = exp(forecast_tslm$.mean))





train_df <- as_tibble(train_ts) %>%
  mutate(den_v_tydnu = factor(den_v_tydnu))
test_df <- as_tibble(test_ts) %>%
  mutate(den_v_tydnu = factor(den_v_tydnu))

train_df <- as_tibble(train_ts) %>%
  mutate(den_v_tydnu = factor(den_v_tydnu)) %>%
  drop_na(log_no2, sin_day, cos_day, sin_year, cos_year,
          data_temp1, valid_speed_count)

model_gam <- gam(log_no2 ~ 
                   s(sin_day, bs = "cc") +
                   s(cos_day, bs = "cc") +
                   s(sin_year, bs = "cc") +
                   s(cos_year, bs = "cc") +
                   s(den_v_tydnu, bs = "re") +
                   s(data_temp1) +
                   s(valid_speed_count),
                 data = train_df,
                 method = "REML")

summary(model_gam)




model_base <- gam(log_no2 ~ s(data_temp1) + s(valid_speed_count), 
                  data = train_df, method = "REML")

anova(model_base, model_gam, test = "F")



gam.check(model_gam)


library(gratia)
draw(model_gam)


# Predikce
test_df$pred_no2_gam <- exp(predict(model_gam, newdata = test_df))

g1 <- ggplot(test_tslm, aes(x = cas)) +
  geom_line(aes(y = exp(log_no2)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_no2_tslm), color = "blue", alpha = 0.4) +
  labs(title = "NO2 – TSLM model (30 % test)", y = "NO₂", x = NULL)+
  scale_x_datetime(date_labels = "%d/%m/%Y") +
  theme_minimal()


g2 <- ggplot(test_df, aes(x = cas)) +
  geom_line(aes(y = exp(log_no2)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_no2_gam), color = "darkgreen", alpha = 0.4) +
  labs(title = "NO2 – GAM model (30 % test)", y = "NO₂", x = NULL)+
  scale_x_datetime(date_labels = "%d/%m/%Y") +
  theme_minimal()

g1 / g2


#############
# gam zachytil opravdu hodne, libi se mi to






###########################################################################
# zpozdene auta o 2, teplota o 1

# === Transformace dat ===
data_model <- data %>%
  filter(!is.na(cas), !is.na(data_no2), is.finite(data_no2)) %>%
  arrange(cas) %>%
  mutate(
    hodina = hour(cas),
    den_v_tydnu = wday(cas, label = TRUE, week_start = 1),
    den_v_roce = yday(cas),
    sin_day = sin(2 * pi * hodina / 24),
    cos_day = cos(2 * pi * hodina / 24),
    sin_year = sin(2 * pi * den_v_roce / 365),
    cos_year = cos(2 * pi * den_v_roce / 365),
    log_no2 = log(data_no2),
    valid_speed_count_lag2 = lag(valid_speed_count, 2),
    data_temp1_lag1 = lag(data_temp1, 1)
  )

data_ts <- data_model %>%
  as_tsibble(index = cas)

# === TSLM MODEL ===
model_tslm <- data_ts %>%
  model(
    tslm = TSLM(log_no2 ~ trend() + season("day") +
                  data_temp1_lag1 + valid_speed_count_lag2)
  )

#lagy aut nejsou dobre
report(model_tslm)

fitted_tslm <- fitted(model_tslm) %>%
  as_tibble() %>%
  select(cas, .fitted) %>%
  rename(fit_no2_tslm = .fitted) %>%
  mutate(fit_no2_tslm = exp(fit_no2_tslm))

data_ts <- data_ts %>%
  left_join(fitted_tslm, by = "cas")

# === GAM MODEL ===
data_df <- as_tibble(data_ts) %>%
  mutate(den_v_tydnu = factor(den_v_tydnu)) %>%
  drop_na(log_no2, sin_day, cos_day, sin_year, cos_year,
          data_temp1_lag1, valid_speed_count_lag2)

model_gam <- gam(log_no2 ~ 
                   s(sin_day, bs = "cc") +
                   s(cos_day, bs = "cc") +
                   s(sin_year, bs = "cc") +
                   s(cos_year, bs = "cc") +
                   s(den_v_tydnu, bs = "re") +
                   s(data_temp1_lag1) +
                   s(valid_speed_count_lag2),
                 data = data_df,
                 method = "REML")

data_df$fit_no2_gam <- exp(fitted(model_gam))

summary(model_gam)

# === GRAFY ===
g1 <- ggplot(data_ts, aes(x = cas)) +
  geom_line(aes(y = exp(log_no2)), color = "black", alpha = 0.5, linewidth = 0.4) +
  geom_line(aes(y = fit_no2_tslm), color = "blue", alpha = 0.6, linewidth = 0.4) +
  labs(title = "NO₂ – Časový vývoj (TSLM)", y = "NO₂", x = NULL) +
  scale_x_datetime(date_labels = "%d/%m/%Y") +
  theme_minimal()

g2 <- ggplot(data_df, aes(x = cas)) +
  geom_line(aes(y = exp(log_no2)), color = "black", alpha = 0.5, linewidth = 0.4) +
  geom_line(aes(y = fit_no2_gam), color = "darkgreen", alpha = 0.6, linewidth = 0.4) +
  labs(title = "NO₂ – Časový vývoj (GAM)", y = "NO₂", x = NULL) +
  scale_x_datetime(date_labels = "%d/%m/%Y") +
  theme_minimal()

g1 / g2