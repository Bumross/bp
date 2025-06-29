library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(dplyr)
library(lubridate)
library(tidyr)


data_ts_denni <- data_denni %>%
  select(datum, prumerna_teplota_dne) %>%
  mutate(
    mesic = factor(month(datum), levels = 1:12)
  ) %>%
  as_tsibble(index = datum) %>%
  fill_gaps()



model_temp_denni <- data_ts_denni %>%
  model(
    temp_model = ARIMA(
      prumerna_teplota_dne ~ 
        fourier(period = 365, K = 3)
        # + mesic
    )
  )
# 200
# mesic byl vyrazen, akorat delal hrube prechody mezi mesici,
# fourier se sam postaral o dobre zachyceni rocni periody

report(model_temp_denni)
glance(model_temp_denni)


resid_temp_denni <- residuals(model_temp_denni)
resid_temp_denni %>%
  ACF() %>%
  autoplot()

resid_temp_denni %>%
  features(.resid, ljung_box, lag = 7)

resid_temp_denni %>%
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram reziduí", x = "Reziduum", y = "Počet")

resid_temp_denni %>%
  ggplot(aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ-plot reziduí")








fitted_data <- augment(model_temp_denni)

fitovana_denni_teplota <- fitted_data %>%
  mutate(
    mesic = month(datum, label = TRUE, abbr = FALSE)
  ) %>%
  select(datum, .fitted, mesic)

fitted_data <- fitted_data %>%
  as_tibble() %>%
  mutate(datum = as.POSIXct(datum))

ggplot(fitted_data, aes(x = datum)) +
  geom_line(aes(y = prumerna_teplota_dne, color = "Skutečné hodnoty"), linewidth = 0.8) +
  geom_line(aes(y = .fitted, color = "Fitované hodnoty"), linewidth = 0.8) +
  labs(
    title = "Fitované vs. skutečné hodnoty teploty",
    x = "Datum",
    y = "Teplota",
    color = "Typ hodnot"
  ) +
  scale_color_manual(values = c("Skutečné hodnoty" = "black", "Fitované hodnoty" = "blue")) +
  scale_x_datetime(date_labels = "%d.%m.%y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





















#################################################
# predikce
# Parametry
period_year <- 365
K_year <- 3

# Funkce na fourierovy členy
make_fourier_terms <- function(t, K, period, prefix) {
  purrr::map_dfc(1:K, function(k) {
    tibble(
      !!paste0(prefix, "_C", k) := cos(2 * pi * k * t / period),
      !!paste0(prefix, "_S", k) := sin(2 * pi * k * t / period)
    )
  })
}

# Příprava dat
data_ts_denni <- data_denni %>%
  mutate(
    t = row_number(),
    mesic = factor(month(datum), levels = 1:12)
  ) %>%
  bind_cols(
    make_fourier_terms(1:nrow(data_denni), K_year, period_year, "fyear")
  ) %>%
  as_tsibble(index = datum) %>% fill_gaps()


model_temp_forecastable <- data_ts_denni %>%
  model(
    temp_forecast = ARIMA(
      prumerna_teplota_dne ~ 
        fyear_C1 + fyear_S1 + fyear_C2 + fyear_S2 +  fyear_C3 + fyear_S3 +
        #mesic +
        pdq(2, 0, 0)
    )
  )


future_data_denni <- tibble(
  datum = seq(
    from = max(data_ts_denni$datum) + 1,
    by = "1 day",
    length.out = 400  # místo 30
  )
) %>%
  mutate(
    t = (nrow(data_ts_denni) + 1):(nrow(data_ts_denni) + 400),
    mesic = factor(month(datum), levels = 1:12)
  ) %>%
  bind_cols(
    make_fourier_terms(1:400, K_year, period_year, "fyear")
  ) %>%
  as_tsibble(index = datum)



forecast_temp <- forecast(model_temp_forecastable, new_data = future_data_denni)


resid_sd <- model_temp_forecastable %>% 
  residuals() %>% 
  as_tibble() %>% 
  pull(.resid) %>% 
  sd(na.rm = TRUE)

forecast_temp <- forecast_temp %>%
  mutate(
    pred = .mean,
    lower = .mean - 1.96 * resid_sd,
    upper = .mean + 1.96 * resid_sd
  )


historicka_data <- data_ts_denni %>%
  as_tibble() %>%
  select(datum, y = prumerna_teplota_dne) %>%
  mutate(typ = "Skutečnost")

predikce_data <- forecast_temp %>%
  as_tibble() %>%
  select(datum, y = pred, lower, upper) %>%
  mutate(typ = "Predikce")

predikce_dopredu <- predikce_data

combined_data <- bind_rows(historicka_data, predikce_data)

ggplot(combined_data, aes(x = datum)) +
  geom_ribbon(
    data = filter(combined_data, typ == "Predikce"),
    aes(ymin = lower, ymax = upper),
    fill = "lightblue", alpha = 0.9
  ) +
  geom_line(aes(y = y, color = typ), linewidth = 0.7) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce" = "blue")) +
  labs(
    title = "Predikce průměrné denní teploty na 400 dnů",
    x = "Datum",
    y = "Teplota (°C)",
    color = "Typ dat"
  ) +
  scale_x_date(date_labels = "%d.%m.%y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



autoplot(forecast_temp, data_ts_denni) +
  labs(
    title = "Predikce průměrné denní teploty na 400 dní dopředu",
    x = "Datum",
    y = "Teplota",
    level = ""
  ) +
  scale_x_date(date_labels = "%d.%m.%y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







################################################################################
# test vs train

n <- nrow(data_ts_denni)
train_n <- floor(0.7 * n)

data_train <- data_ts_denni[1:train_n, ]
data_test  <- data_ts_denni[(train_n + 1):n, ]

model_temp_train <- data_train %>%
  model(
    temp_forecast = ARIMA(
      prumerna_teplota_dne ~ 
        fyear_C1 + fyear_S1 + fyear_C2 + fyear_S2 + fyear_C3 + fyear_S3 +
        pdq(2, 0, 0)
    )
  )


report(model_temp_train)
glance(model_temp_train)

forecast_temp_test <- forecast(model_temp_train, new_data = data_test)




# vykresleni hezke 
resid_sd <- model_temp_train %>% 
  residuals() %>% 
  as_tibble() %>% 
  pull(.resid) %>% 
  sd(na.rm = TRUE)

forecast_temp_test <- forecast_temp_test %>%
  mutate(
    pred = .mean,
    lower = .mean - 1.96 * resid_sd,
    upper = .mean + 1.96 * resid_sd
  )

historicka_data <- data_ts_denni %>%
  as_tibble() %>%
  select(datum, y = prumerna_teplota_dne) %>%
  mutate(typ = "Skutečnost")

predikce_data <- forecast_temp_test %>%
  as_tibble() %>%
  select(datum, y = pred, lower, upper) %>%
  mutate(typ = "Predikce")

combined_data <- bind_rows(historicka_data, predikce_data)

ggplot(combined_data, aes(x = datum)) +
  geom_ribbon(
    data = filter(combined_data, typ == "Predikce"),
    aes(ymin = lower, ymax = upper),
    fill = "lightblue", alpha = 0.9
  ) +
  geom_line(aes(y = y, color = typ), linewidth = 0.7) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce" = "blue")) +
  labs(
    title = "Predikce průměrné denní teploty na testovacím intervalu",
    x = "Datum",
    y = "Teplota (°C)",
    color = "Typ dat"
  ) +
  scale_x_date(date_labels = "%d.%m.%y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






##########################################################################################
###############
# spojeni dat
pred_400 <- predikce_dopredu %>%
  as_tibble() %>%
  select(datum, y) %>%
  rename(pred_400 = y)

pred_test <- predikce_data %>%
  as_tibble() %>%
  select(datum, y, lower, upper)

fitted <- fitted_data %>%
  select(datum, .fitted)

data_clean <- data_denni %>%
  select(datum, prumerna_teplota_dne)

# 2. Spojíme všechno dohromady
vse_spojeno <- data_clean %>%
  full_join(fitted, by = "datum") %>%
  full_join(pred_test, by = "datum")




vse_spojeno <- bind_rows(vse_spojeno, pred_400)

write.csv(vse_spojeno, "teplota_denni.csv")



fc <- forecast(model_train, new_data = data_test)
accuracy(fc, data_test)
