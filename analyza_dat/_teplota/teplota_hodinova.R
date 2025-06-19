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


data_ts_temp <- data_ts_temp %>%
  mutate(data_temp1 = tsibble::fill_gaps(data_temp1)) %>%
  mutate(data_temp1 = na.interpolation(data_temp1))




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
        mesic +
        lag_temp_1 +
        lag_temp_24 +
        pdq(5, 0, 0) +
        PDQ(1, 0, 0, 24)
    )
  )

report(model_temp)
glance(model_temp)

# ARIMA(5,0,0)




resid_temp <- residuals(model_temp)
resid_temp %>% ACF() %>% autoplot()

resid_temp %>%
  features(.resid, ljung_box, lag = 24, dof = 12) 


resid_temp %>% autoplot()
resid_temp %>% ggplot(aes(x = .resid)) + geom_histogram(bins = 30)
resid_temp %>% ggplot(aes(sample = .resid)) + stat_qq() + stat_qq_line()



###############################################################################
# predikce
# musim vytvorit model bez lagu, jinak to nejde asi predikovat :(


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

# Počet harmonických členů
K_day <- 3
K_year <- 1

# Funkce pro vytvoření fourierových členů
make_fourier_terms <- function(t, K, period, prefix) {
  terms <- purrr::map_dfc(1:K, function(k) {
    tibble(
      !!paste0(prefix, "_C", k) := cos(2 * pi * k * t / period),
      !!paste0(prefix, "_S", k) := sin(2 * pi * k * t / period)
    )
  })
  return(terms)
}

# Aplikuj na future_data
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
        pdq(5, 0, 0) + PDQ(1, 0, 0, 24)
    )
  )


future_data_ts <- future_data_ts %>%
  mutate(
    mesic = factor(month(cas), levels = 1:12)
  )


forecast_temp <- forecast(model_temp_forecastable, new_data = future_data_ts)



autoplot(forecast_temp, data_ts_temp) +
  labs(
    title = "Predikce teploty na 30 dní dopředu",
    x = "Datum a čas",
    y = "Teplota"
  ) +
  scale_x_datetime(date_labels = "%d.%m. %H:%M") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








model_temp_forecastable %>% select(temp_forecast) %>% tidy() %>% pull(term)


glimpse(future_data_ts)
levels(future_data_ts$mesic)
levels(data_ts_temp$mesic)
names(future_data_ts)
names(model_temp_forecastable$fit$models$temp_forecast$coefficients)




################################################
# testovaci x trenovaci data
n <- nrow(data_ts_temp)
train_n <- floor(0.7 * n)
data_train <- data_ts_temp[1:train_n, ]
data_test <- data_ts_temp[(train_n + 1):n, ]

# Model jen na trénovací
model_train <- data_train %>%
  model(temp_model = ARIMA(
    data_temp1 ~ 
      fourier(24, K = 3) +
      fourier(24*365, K = 1) +
      as.factor(mesic) +
      lag_temp_1 + lag_temp_24 +
      pdq(5, 0, 0) + PDQ(1, 0, 0, 24)
  ))

# Forecast
forecast_test <- forecast(model_train, new_data = data_test)

# Graf
autoplot(forecast_test, data_ts_temp) +
  labs(title = "Predikce na testovací sadě (30 %)", y = "Teplota")




