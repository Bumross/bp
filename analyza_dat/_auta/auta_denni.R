library(tsibble)
library(fable)
library(feasts)
library(fabletools)
library(tidyverse)
library(lubridate)






################################################################################
################################################################################
################################################################################
################################################################################
# AUTA DENNI


auta <- data_denni$valid_speed_count
den <- data_denni$den_v_tydnu
svatek <- data_denni$velke_svatky
prazd <- data_denni$letni_prazdniny



# tsibble
data_ts_denni <- data_denni %>%
  mutate(datum = as_date(datum)) %>%
  as_tsibble(index = datum) %>%
  fill_gaps()

data_ts_denni <- data_ts_denni %>%
  mutate(
    den_v_tydnu = factor(den_v_tydnu, levels = 1:7),
    velke_svatky = factor(velke_svatky, levels = c(0, 1)),
    letni_prazdniny = factor(letni_prazdniny, levels = c(0, 1)),
    mesice = factor(month(datum), levels = 1:12)
  )

# posledni den je jen polovicni a hazi spatny data, proto vynechavam 16.1.2025



# diferencovani
data_ts_denni %>%
  features(valid_speed_count, unitroot_kpss)

# linearni model
model_lm1 <- data_ts_denni %>%
  model(
    lm1 = TSLM(valid_speed_count ~ den_v_tydnu + velke_svatky + letni_prazdniny)
  )

report(model_lm1)









##
model_auto_sarima <- data_ts_denni %>%
  model(
    ARIMA(valid_speed_count)
  )
report(model_auto_sarima)





# arima

model_denni <- data_ts_denni %>%
  model(
    sarima7 = ARIMA(
      valid_speed_count ~ 
        den_v_tydnu + velke_svatky + letni_prazdniny
    )
  )
# 1 0 1
# AIC 7565
# BIC 7612

report(model_denni)


model_denni_2 <- data_ts_denni %>%
  model(
    sarima7 = ARIMA(
      valid_speed_count ~ 
        velke_svatky + letni_prazdniny + 
        fourier(7, K = 2)
    )
  )
# 1 0 1
# fourier je horsi nez faktory dnu v tydnu
report(model_denni_2)



model_sarima_explicit <- data_ts_denni %>%
  model(
    ARIMA(valid_speed_count ~ den_v_tydnu + velke_svatky  + letni_prazdniny + pdq(1,0,0) + PDQ(1,0,0, 7)
    ))

report(model_sarima_explicit)
# 1 0 0,  1 0 0
# explicitni sarima to nezlepsi, rezidua nejsou korelovano, jsou uz dobre popsany zakladem a regresory


################################################################################
################################################################################
# nejlepsi model

# mesice jsem zkousel, pak ale rozlisoval moc mezi mesicema a vznikaly tam velke odchylky
model_denni <- data_ts_denni %>%
  model(
    sarima7 = ARIMA(
      valid_speed_count ~ 
        den_v_tydnu + velke_svatky + letni_prazdniny + pdq(1, 0, 1)
    )
  )
# 1 0 1
# AIC 7565
# BIC 7612

report(model_denni)





model_denni %>%
  gg_tsresiduals()


augment(model_denni) %>%
  features(.resid, ljung_box, lag = 7)
# zadny zbytky - p = 0.83

augment(model_denni) %>%
  features(.resid, ljung_box, lag = 30)
# 0.09 = p

glance(model_denni)


augment(model_denni) %>%
  ggplot(aes(x = datum)) +
  geom_line(aes(y = valid_speed_count, color = "Skutečnost")) +
  geom_line(aes(y = .fitted, color = "Model")) +
  labs(
    title = "Model vs. skutečnost (auta)",
    y = "počet projetých aut za den",
    color = ""
  ) +
  scale_color_manual(values = c("Skutečnost" = "gray", "Model" = "blue")) +
  scale_x_date(date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################################
################################################################################





resid_model <- augment(model_denni) %>%
  as_tsibble(index = datum) %>%
  model(
    ARIMA(.resid)
  )

report(resid_model)
# na zaklade tohoto tam pridam jeste sezonni slozku



# ne, puvodni model, nevylepseny, je lepsi :-)
model_vylepseny <- data_ts_denni %>%
  model(
    ARIMA(
      valid_speed_count ~ den_v_tydnu + velke_svatky + letni_prazdniny +
        pdq(1, 0, 1) + PDQ(0, 0, 1, 7)
    )
  )

report(model_vylepseny)
glance(model_vylepseny)
augment(model_vylepseny) %>% features(.resid, ljung_box, lag = 7)





###############################################################################
###############################################################################


# predikce

new_data <- tibble(
  datum = seq(
    from = max(data_ts_denni$datum) + 1,
    by = "1 day",
    length.out = 30
  )
) %>%
  mutate(
    den_v_tydnu = wday(datum, week_start = 1),
    den_v_tydnu = factor(den_v_tydnu, levels = 1:7),
    velke_svatky = factor(0, levels = c(0, 1)),
    letni_prazdniny = factor(0, levels = c(0, 1))
  ) %>%
  as_tsibble(index = datum)

forecast_denni <- forecast(model_denni, new_data = new_data)

autoplot(forecast_denni, data_ts_denni) +
  labs(
    title = "Predikce počtu projetých aut za den",
    x = "Datum",
    y = "Počet projetých aut"
  ) +
  scale_x_date(date_labels = "%m/%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




###########################################
# testovaci x trenovaci sada

n_total <- nrow(data_ts_denni)
n_train <- round(n_total * 0.7)

data_train <- data_ts_denni %>% slice(1:n_train)
data_test <- data_ts_denni %>% slice((n_train + 1):n_total)

model_train <- data_train %>%
  model(
    ARIMA(valid_speed_count ~ den_v_tydnu + velke_svatky + letni_prazdniny + pdq(1, 0, 1))
  )

forecast_test <- forecast(model_train, new_data = data_test)

results <- left_join(
  forecast_test,
  data_test %>% select(datum, valid_speed_count),
  by = "datum"
)

ggplot(results, aes(x = datum)) +
  geom_line(aes(y = valid_speed_count.y, color = "Skutečnost")) +
  geom_line(aes(y = .mean, color = "Predikce")) +
  labs(
    title = "Predikce vs. skutečnost",
    x = "Datum",
    y = "Počet projetých aut",
    color = ""
  ) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce" = "blue")) +
  scale_x_date(date_labels = "%d.%m.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
