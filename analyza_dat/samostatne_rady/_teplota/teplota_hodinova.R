library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(dplyr)
library(lubridate)
library(tidyr)


data_ts_temp <- data %>%
  select(cas, data_temp1) %>%
  mutate(
    hodina = hour(cas),
    den_v_roce = yday(cas),
    den_v_tydnu = wday(cas, label = TRUE), # pondělí = 1
    mesic = factor(month(cas), levels = 1:12),
    datum = as_date(cas)
  ) %>%
  as_tsibble(index = cas) %>%
  fill_gaps()



####
data_ts_temp <- data_ts_temp %>%
  mutate(
    lag_temp_1 = lag(data_temp1, 1),
    lag_temp_24 = lag(data_temp1, 24)
  )



model_temp <- data_ts_temp %>%
  model(
    temp_fourier = ARIMA(
      data_temp1 ~ 
        fourier(24, K = 3) +      # denní periodicita
        fourier(24*365, K = 1) +     # roční periodicita
        factor(month(cas), levels = 1:12)
        + pdq(3, 0, 0) +
        PDQ(1, 0, 0, 24)
    )
  )

report(model_temp)
glance(model_temp)

# ARIMA(5,0,0)
fitted_data <- augment(model_temp)

fitovana_hodinova_teplota <- fitted_data %>%
  mutate(
    mesic = month(cas, label = TRUE, abbr = FALSE)
  ) %>%
  select(cas, .fitted, mesic)


ggplot(fitted_data, aes(x = cas)) +
  geom_line(aes(y = data_temp1, color = "Skutečné hodnoty"), linewidth = 0.8) +
  geom_line(aes(y = .fitted, color = "Fitované hodnoty"), linewidth = 0.8, linetype = "dashed") +
  labs(
    title = "Fitované vs. skutečné hodnoty teploty",
    x = "Datum a čas",
    y = "Teplota",
    color = "Typ hodnot"
  ) +
  scale_color_manual(values = c("Skutečné hodnoty" = "black", "Fitované hodnoty" = "blue")) +
  scale_x_datetime(date_labels = "%d.%m.%y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



resid_temp <- residuals(model_temp)
resid_temp %>% ACF() %>% autoplot()

resid_temp %>%
  features(.resid, ljung_box, lag = 24, dof = 12) 


resid_temp %>% autoplot()
resid_temp %>% ggplot(aes(x = .resid)) + geom_histogram(bins = 30)
resid_temp %>% ggplot(aes(sample = .resid)) + stat_qq() + stat_qq_line()



###############################################################################
# predikce
#


future_data <- tibble(
  cas = seq(
    from = max(data_ts_temp$cas) + hours(1),
    by = "1 hour",
    length.out = 24 * 30
  )
) %>%
  mutate(
    t = row_number(),                             # časový index
    mesic = factor(month(cas)),
    den_v_roce = yday(cas),
    hodina = hour(cas)
  )

period_day <- 24
period_year <- 24 * 365

K_day <- 3
K_year <- 1

make_fourier_terms <- function(t, K, period, prefix) {
  terms <- purrr::map_dfc(1:K, function(k) {
    tibble(
      !!paste0(prefix, "_C", k) := cos(2 * pi * k * t / period),
      !!paste0(prefix, "_S", k) := sin(2 * pi * k * t / period)
    )
  })
  return(terms)
}

future_data <- future_data %>%
  bind_cols(
    make_fourier_terms(future_data$t, K_day, period_day, "fday"),
    make_fourier_terms(future_data$t, K_year, period_year, "fyear")
  )

future_data_ts <- future_data %>%
  as_tsibble(index = cas)







model_temp_forecastable <- data_ts_temp %>%
  mutate(t = row_number()) %>%  # přidat časový index
  bind_cols(
    make_fourier_terms(1:nrow(data_ts_temp), K_day, period_day, "fday"),
    make_fourier_terms(1:nrow(data_ts_temp), K_year, period_year, "fyear")
  ) %>%
  model(
    temp_forecast = ARIMA(
      data_temp1 ~ 
        fday_C1 + fday_S1 + fday_C2 + fday_S2 + fday_C3 + fday_S3 +
        fyear_C1 + fyear_S1 +
        mesic +
        pdq(3, 0, 0) + PDQ(1, 0, 0, 24)
    )
  )


future_data_ts <- future_data_ts %>%
  mutate(
    mesic = factor(month(cas), levels = 1:12)
  )


forecast_temp <- forecast(model_temp_forecastable, new_data = future_data_ts)


resid_sd <- model_temp_forecastable %>% 
  residuals() %>% 
  as_tibble() %>% 
  pull(.resid) %>% 
  sd(na.rm = TRUE)

# prirazeni predikce, lower, upper
forecast_temp <- forecast_temp %>%
  mutate(
    pred = .mean,
    lower = .mean - 1.96 * resid_sd,
    upper = .mean + 1.96 * resid_sd
  )



combined_data <- forecast_temp %>%
  as_tibble() %>%
  rename(datum = cas) %>%
  mutate(
    typ = "Predikce",
    y = pred,
    lower = lower,
    upper = upper
  ) %>%
  select(datum, y, lower, upper, typ) %>%
  bind_rows(
    data_test %>%
      rename(datum = cas, y = data_temp1) %>%
      mutate(typ = "Skutečnost") %>%
      select(datum, y, typ)
  )

# Vykreslení grafu
ggplot(combined_data, aes(x = datum)) +
  geom_ribbon(
    data = filter(combined_data, typ == "Predikce"),
    aes(ymin = lower, ymax = upper),
    fill = "lightblue", alpha = 0.9
  ) +
  geom_line(aes(y = y, color = typ), linewidth = 0.7) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce" = "blue")) +
  labs(
    title = "Predikce průměrné hodinové teploty",
    x = "Datum",
    y = "Teplota (°C)",
    color = "Typ dat"
  ) +
  scale_x_datetime(date_labels = "%d.%m.%y", date_breaks = "14 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


autoplot(forecast_temp, data_ts_temp) +
  labs(
    title = "Predikce teploty na 30 dní dopředu",
    x = "Datum a čas",
    y = "Teplota"
  ) +
  scale_x_datetime(date_labels = "%d.%m. %H:%M") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()


predpoved_na_30_dnu_dopredu_hodinova <- forecast_temp %>%
  mutate(
    mesic = month(cas, label = TRUE, abbr = FALSE)
  ) %>%
  select(cas, .mean, mesic)






model_temp_forecastable %>% select(temp_forecast) %>% tidy() %>% pull(term)


glimpse(future_data_ts)
levels(future_data_ts$mesic)
levels(data_ts_temp$mesic)
names(future_data_ts)
names(model_temp_forecastable$fit$models$temp_forecast$coefficients)




################################################
# testovaci x trenovaci data
data_ts_temp_clean <- data_ts_temp %>%
  select(cas, data_temp1) %>%
  mutate(mesic = factor(month(cas))) %>%
  as_tsibble(index = cas)




n <- nrow(data_ts_temp_clean)
train_n <- floor(0.7 * n)
data_train <- data_ts_temp_clean[1:train_n, ]
data_test  <- data_ts_temp_clean[(train_n + 1):n, ]

# trenovaci model
model_train <- data_train %>%
  model(temp_model = ARIMA(
    data_temp1 ~ 
      fourier(24, K = 3) +
      fourier(24 * 365, K = 1) +
      factor(month(cas), levels = 1:12) +
      pdq(3, 0, 0) + PDQ(1, 0, 0, 24)
  ))



report(model_train)
glance(model_train)

future_data <- tibble(
  cas = seq(from = max(data_train$cas) + hours(1), by = "1 hour", length.out = nrow(data_test)),
  data_temp1 = NA_real_
) %>%
  as_tsibble(index = cas)

forecast_test <- forecast(model_train, new_data = future_data)



resid_sd <- model_train %>% 
  residuals() %>% 
  as_tibble() %>% 
  pull(.resid) %>% 
  sd(na.rm = TRUE)

# prirazeni predikce, lower, upper
forecast_test <- forecast_test %>%
  mutate(
    pred = .mean,
    lower = .mean - 1.96 * resid_sd,
    upper = .mean + 1.96 * resid_sd
  )



combined_data <- forecast_test %>%
  as_tibble() %>%
  rename(datum = cas) %>%
  mutate(
    typ = "Predikce",
    y = pred,
    lower = lower,
    upper = upper
  ) %>%
  select(datum, y, lower, upper, typ) %>%
  bind_rows(
    data_test %>%
      rename(datum = cas, y = data_temp1) %>%
      mutate(typ = "Skutečnost") %>%
      select(datum, y, typ)
  )

# Vykreslení grafu
ggplot(combined_data, aes(x = datum)) +
  geom_ribbon(
    data = filter(combined_data, typ == "Predikce"),
    aes(ymin = lower, ymax = upper),
    fill = "lightblue", alpha = 0.9
  ) +
  geom_line(aes(y = y, color = typ), linewidth = 0.7) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce" = "blue")) +
  labs(
    title = "Predikce průměrné hodinové teploty vs. skutečnost",
    x = "Datum",
    y = "Teplota (°C)",
    color = "Typ dat"
  ) +
  scale_x_datetime(date_labels = "%d.%m.%y", date_breaks = "7 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


teplota_testovaci_predikce <- forecast_test





















#################xx
# spojeni dat
pred_30 <- predpoved_na_30_dnu_dopredu_hodinova %>%
  as_tibble() %>%
  select(cas, mesic, .mean) %>%
  rename(pred_30 = .mean, mesic = mesic)

pred_test <- teplota_testovaci_predikce %>%
  as_tibble() %>%
  select(cas, pred, lower, upper)

fitted <- fitovana_hodinova_teplota %>%
  select(cas, .fitted)

data_clean <- data %>%
  select(cas, data_temp1, mesic)

# 2. Spojíme všechno dohromady
vse_spojeno <- data_clean %>%
  full_join(fitted, by = "cas") %>%
  full_join(pred_test, by = "cas")


mesiace_nazvy <- month.name
mesiace_zkratky <- month.abb


vse_spojeno <- vse_spojeno %>%
  mutate(mesic = match(as.character(mesic), month.abb))

pred_30 <- pred_30 %>%
  mutate(mesic = as.integer(as.character(mesic)))

vse_spojeno <- bind_rows(vse_spojeno, pred_30)

write.csv(vse_spojeno, "teplota_hodinova.csv")
