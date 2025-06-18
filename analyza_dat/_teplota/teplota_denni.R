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

data_ts_denni <- data_ts_denni %>%
  mutate(prumerna_teplota_dne = tsibble::fill_gaps(prumerna_teplota_dne)) %>%
  mutate(prumerna_teplota_dne = na.interpolation(prumerna_teplota_dne))


model_temp_denni <- data_ts_denni %>%
  model(
    temp_model = ARIMA(
      prumerna_teplota_dne ~ 
        fourier(period = 365, K = 3)
        # + mesic
    )
  )
# 200

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

autoplot(forecast_temp, data_ts_denni) +
  labs(
    title = "Predikce průměrné denní teploty na 400 dní dopředu",
    x = "Datum",
    y = "Teplota",
    level = ""
  ) +
  scale_x_date(date_labels = "%d.%m.%y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
