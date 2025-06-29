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


data_ts_denni <- data_ts_denni %>%
  slice_head(n = nrow(.) - 1)
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
    title = "Model vs. skutečnost",
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
    title = "Predikce počtu projetých vozidel za den",
    x = "Datum",
    y = "Počet projetých vozidel"
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
  geom_line(aes(y = valid_speed_count.y, color = "Skutečnost"), linewidth = 0.7) +
  geom_line(aes(y = .mean, color = "Predikce"), linewidth = 0.7) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce" = "blue")) +
  labs(
    title = "Predikce počtu projetých vozidel vs. skutečnost",
    x = "Datum",
    y = "Počet projetých aut",
    color = "Typ dat"
  ) +
  scale_x_date(date_labels = "%d.%m.%y", date_breaks = "7 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(results, aes(x = cas)) +
  geom_line(aes(y = valid_speed_count.y, color = "Skutečnost"), linewidth = 0.7) +
  geom_line(aes(y = .mean, color = "Predikce"), linewidth = 0.7) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce" = "blue")) +
  labs(
    title = "Predikce počtu projetých vozidel vs. skutečnost",
    x = "Datum",
    y = "Počet projetých vozidel",
    color = "Typ dat"
  ) +
  scale_x_datetime(date_labels = "%d.%m.%y", date_breaks = "7 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




################################################################################
################################################################################
# vypocet rucni vlivu svatku

# prumerne hodnoty podle dne v tydnu:
prumery_dnu <- data_denni %>%
  filter(velke_svatky == FALSE, !is.na(valid_speed_count)) %>%
  group_by(den_v_tydnu) %>%
  summarise(prumer_dne = mean(valid_speed_count), .groups = "drop")

svatecni_dny <- data_denni %>%
  filter(velke_svatky == TRUE) %>%
  select(datum, den_v_tydnu, valid_speed_count)

svatek_vs_prumer <- svatecni_dny %>%
  left_join(prumery_dnu, by = "den_v_tydnu") %>%
  mutate(odchylka = valid_speed_count - prumer_dne)

prumerna_odchylka_svatek <- mean(svatek_vs_prumer$odchylka, na.rm = TRUE)
print(prumerna_odchylka_svatek)

# vyslo celkem velke zaporne cislo, to je divne
# -14499.49

# kontrola, jake jsou odchylky od svatku
svatek_vs_prumer %>%
  select(datum, den_v_tydnu, valid_speed_count, prumer_dne, odchylka)


######
vliv_svatku <- -14499.49

n_total <- nrow(data_ts_denni)
n_train <- round(n_total * 0.7)

data_train <- data_ts_denni %>% slice(1:n_train)
data_test <- data_ts_denni %>% slice((n_train + 1):n_total)

model_train <- data_train %>%
  model(
    ARIMA(valid_speed_count ~ den_v_tydnu + letni_prazdniny 
          + pdq(0,0,2)
          + PDQ(2,0,1,7))
  )

report(model_train) # 002, 201

forecast_test <- model_train %>%
  forecast(new_data = data_test)

forecast_df <- forecast_test %>%
  as_tibble() 


# vysledky predikce
results <- forecast_df %>%
  mutate(
    velke_svatky = as.numeric(as.character(velke_svatky))  # přetypování z faktoru
  ) %>%
  mutate(
    predikce_puvodni = .mean,
    predikce_upravena = if_else(velke_svatky == 1, .mean + vliv_svatku, .mean)
  )

results <- results %>%
  left_join(
    data_test %>%
      as_tibble() %>%
      select(datum, skutecnost = valid_speed_count),
    by = "datum"
  )

ggplot(results, aes(x = datum)) +
  geom_line(aes(y = skutecnost, color = "Skutečnost")) +
  geom_line(aes(y = predikce_upravena, color = "Predikce (upravená)")) +
  labs(
    title = "Predikce vs. skutečnost (s ruční korekcí za svátky)",
    x = "Datum",
    y = "Počet projetých aut",
    color = ""
  ) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce (upravená)" = "blue")) +
  scale_x_date(date_labels = "%d.%m.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




fc <- forecast(model_train, new_data = data_test)
accuracy(fc, data_test)




####
fitted_train <- model_train %>%
  fitted()

# Spojení se skutečností
results_fit <- fitted_train %>%
  as_tibble() %>%
  left_join(
    data_train %>%
      as_tibble() %>%
      select(datum, skutecnost = valid_speed_count),
    by = "datum"
  )

# Vykreslení
ggplot(results_fit, aes(x = datum)) +
  geom_line(aes(y = skutecnost, color = "Skutečnost")) +
  geom_line(aes(y = .fitted, color = "Fitované hodnoty")) +
  labs(
    title = "Fit modelu na trénovacích datech",
    x = "Datum",
    y = "Počet projetých aut",
    color = ""
  ) +
  scale_color_manual(values = c("Skutečnost" = "black", "Fitované hodnoty" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
