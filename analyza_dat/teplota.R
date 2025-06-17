### Vymyslim dle dynlm od masterchefa doplneni matice regresoru do arimy

den <- as.numeric(date(data$cas) - as.Date("2024-01-01"))
den <- ifelse(den > 365, den - 366, den)
den2 <- den^2


# interakce hodiny a mesice (ten crazy graf)
hod <- as.factor(hour(data$cas))
mesic <- as.factor(month(data$cas))

# design matrix
reg <- model.matrix(~ den + den2 + hod * mesic)[, -1]



temp2 <- ts(data$data_temp1, frequency = 24)

nrow(reg) == length(temp2)
# radky souhlasi


# nespoustet, az moc regresoru, nedava to..
ar.t1 <- auto.arima(temp2, xreg = reg, d = 1, D = 0,
                    max.p = 3, max.q = 3, max.order = 5,
                    stepwise = TRUE, approximation = TRUE)
summary(ar.t1)


##############################################################################
##############################################################################
##############################################################################

# použití tsibble fable atd podle Hyndmana
library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(lubridate)
library(zoo)
library(slider)
library(ggplot2)


# Doplňující proměnné
data_ts_temp <- data %>%
  select(cas, data_temp1, hodina, month, tretina_dne) %>%
  as_tsibble(index = cas)

data_ts_temp <- data_ts_temp %>%
  arrange(cas)

data_ts_temp <- data_ts_temp %>% 
  fill_gaps()


# klouzavy prumer
data_ts_temp <- data_ts_temp %>%
  mutate(
    temp_rm_99 = slide_dbl(data_temp1, mean, .before = 49, .after = 49, .complete = TRUE),
    temp_rm_169 = slide_dbl(data_temp1, mean, .before = 84, .after = 84, .complete = TRUE),
    temp_rm_301 = slide_dbl(data_temp1, mean, .before = 150, .after = 150, .complete = TRUE)
  )


data_long <- data_ts_temp %>%
  select(cas, data_temp1, temp_rm_99, temp_rm_169, temp_rm_301) %>%
  pivot_longer(
    cols = c(data_temp1, temp_rm_99, temp_rm_169, temp_rm_301),
    names_to = "typ",
    values_to = "teplota"
  )

# Vykreslení
ggplot(data_long, aes(x = cas, y = teplota, color = typ)) +
  geom_line(alpha = 2) +
  labs(
    title = "Teplota a její klouzavé průměry",
    x = "Datum",
    y = "Teplota [°C]",
    color = "Typ série"
  ) +
  scale_color_manual(
    values = c(
      "data_temp1" = "black",
      "temp_rm_99" = "red",
      "temp_rm_169" = "blue",
      "temp_rm_301" = "green"
    ),
    labels = c(
      "data_temp1" = "Původní teplota",
      "temp_rm_99" = "Klouzavý průměr (99)",
      "temp_rm_169" = "Klouzavý průměr (169)",
      "temp_rm_301" = "Klouzavý průměr (301)"
    )
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################################################



# trendova slozka
model_lm1 <- data_ts_temp %>%
  model(
    lm_trend = TSLM(data_temp1 ~ poly(cas, 2))
  )

report(model_lm1)
augment(model_lm1) %>% autoplot(.resid)


model_lm2 <- data_ts_temp %>%
  model(
    TSLM(data_temp1 ~ poly(cas, 2) + interaction(hodina, mesic))
  )

report(model_lm2)
augment(model_lm2) %>% autoplot(.resid)
# to se hezky zmensilo :-)

augment(model_lm2) %>%
  ACF(.resid) %>%
  autoplot()

augment(model_lm2) %>%
  PACF(.resid) %>%
  autoplot()


##################
data_ts_temp <- data_ts_temp %>%
  mutate(lag1 = lag(data_temp1, 1))

model_lm3 <- data_ts_temp %>%
  model(
    TSLM(data_temp1 ~ poly(cas, 2) + interaction(hodina, mesic) + lag1)
  )

report(model_lm3)
augment(model_lm3) %>% autoplot(.resid)

augment(model_lm3) %>%
  ACF(.resid) %>%
  autoplot()

augment(model_lm3) %>%
  PACF(.resid) %>%
  autoplot()

########################################################################
data_ts_temp <- data_ts_temp %>%
  mutate(
    lag_1 = lag(data_temp1, 1),
    lag_24 = lag(data_temp1, 24)
  )

model_lm4<- data_ts_temp %>%
  model(
    TSLM(data_temp1 ~ poly(cas, 2) + interaction(hodina, mesic) + lag_1 + lag_24)
  )

report(model_lm4)
augment(model_lm4) %>% autoplot(.resid)

augment(model_lm4) %>%
  ACF(.resid) %>%
  autoplot()

augment(model_lm4) %>%
  PACF(.resid) %>%
  autoplot()

augment(model_lm4) %>% features(.resid, ljung_box)

augment(model_lm4) %>%
  bind_cols(cas = data_ts_temp$cas) %>%
  ggplot() +
  geom_line(aes(x = cas, y = data_temp1), color = "black") +
  geom_line(aes(x = cas, y = .fitted), color = "blue") +
  labs(title = "Model vs. Skutečnost", y = "Teplota", x = "Čas")


########################################################################


data_ts_temp <- data_ts_temp %>%
  mutate(
    lag_1 = lag(data_temp1, 1),
    lag_2 = lag(data_temp1, 2),
    lag_24 = lag(data_temp1, 24),
    lag_25 = lag(data_temp1, 25)
  )

model_lm5 <- data_ts_temp %>%
  model(
    TSLM(data_temp1 ~ poly(cas, 2) + interaction(hodina, mesic) + lag_1 + lag_2 + lag_24 + lag_25)
  )


report(model_lm5)
augment(model_lm5) %>% autoplot(.resid)

augment(model_lm5) %>%
  ACF(.resid) %>%
  autoplot()

augment(model_lm5) %>%
  PACF(.resid) %>%
  autoplot()

augment(model_lm5) %>% features(.resid, ljung_box)

# MNE SE TO LIBIIII <3 - to uz nekam smeruje - obklopeni lagama haha



################################################################################
################################################################################
################################################################################
# Fourierovy cleny v arima modelu
data_ts_temp <- data_ts_temp %>%
  mutate(
    den_v_roce = yday(cas),
    den_v_roce2 = den_v_roce^2,
    hodina = hour(cas),
    lag_1 = lag(data_temp1, 1),
    lag_2 = lag(data_temp1, 2),
    lag_24 = lag(data_temp1, 24),
    lag_25 = lag(data_temp1, 25)
  )

# arima + fourier k = 3, zkusime to zasadit do toho predchoziho modelu
model_arima_temp <- data_ts_temp %>%
  model(
    arima_temp = ARIMA(data_temp1 ~ 
                         fourier(period = "1 day", K = 3) + # denní periodicita
                         den_v_roce + den_v_roce2 +         # dlouhodobý trend
                         lag_1 + lag_2 + lag_24 + lag_25     # autoregrese
    )
  )

model_arima_temp %>% glance()
augment(model_arima_temp) %>% features(.resid, ljung_box)


augment(model_arima_temp) %>% autoplot(.resid)

augment(model_arima_temp) %>%
  ACF(.resid) %>%
  autoplot()

augment(model_arima_temp) %>%
  PACF(.resid) %>%
  autoplot()


model_arima_temp %>% tidy()



## zrovna na ten temp a tamp^2 mi to vraci NaN :( zkousim najit pricinu

data_ts_temp %>%
  select(data_temp1, den_v_roce, den_v_roce2, lag_1, lag_2, lag_24, lag_25) %>%
  cor(use = "complete.obs")

# a je to tim, ze tyto dve jsou korelovane jak 

###########################################
# oukej, to bylo tim, ze jsem nepouzil to poly() a potrebuji dat nektere lagy pryc
model_arima_temp <- data_ts_temp %>%
  model(
    arima_temp = ARIMA(data_temp1 ~ 
                         fourier(period = "1 day", K = 3) +
                         poly(den_v_roce, 2) + 
                         lag_1 + lag_24
    )
  )

model_arima_temp %>% glance()
augment(model_arima_temp) %>% features(.resid, ljung_box)


augment(model_arima_temp) %>% autoplot(.resid)

augment(model_arima_temp) %>%
  ACF(.resid) %>%
  autoplot()

augment(model_arima_temp) %>%
  PACF(.resid) %>%
  autoplot()


model_arima_temp %>% tidy()
#naprosty banger
# 5,0,0 vyhodnotil (ale cleny ar4 a ar5 > 0.5), takze spise 3,0,0
# UPLNA KRASA
# tento model asi pouziju 5,0,0 - ma trochu vetsi p-hodnotu ljungboxe
model_finalni_temo <- data_ts_temp %>%
  model(
    arima_temp = ARIMA(data_temp1 ~ fourier(period = "1 day", K = 3)
                       [, c("C1_24", "S1_24", "S2_24", "S3_24")] +
                         poly(den_v_roce, 2) +
                         lag_1 + lag_24 )
  )

model_finalni_temo %>% glance()

# a nebo 3,0,0, ktery je skoro totozny, jen proste ljungboxe ma o 0,04 mensi..
# celkovy model:

data_ts_temp <- data %>%
  select(cas, data_temp1, hodina, month) %>%
  as_tsibble(index = cas)

data_ts_temp <- data_ts_temp %>%
  arrange(cas)

data_ts_temp <- data_ts_temp %>% 
  fill_gaps()


data_ts_temp <- data_ts_temp %>%
  mutate(
    den_v_roce = yday(cas),
    den_v_roce2 = den_v_roce^2,
    hodina = hour(cas),
    lag_1 = lag(data_temp1, 1),
    lag_24 = lag(data_temp1, 24)
  )



model_arima_300 <- data_ts_temp %>%
  model(
    arima_temp = ARIMA(
      data_temp1 ~ fourier(period = "1 day", K = 3) +
        poly(den_v_roce, 2) + month +
        lag_1 + lag_24 +
        pdq(3, 1, 0) # pevné nastavení jádra
    )
  )

model_arima_300 %>% glance()

model_arima_300 %>% tidy()
augment(model_arima_300) %>% features(.resid, ljung_box)


augment(model_arima_300) %>% autoplot(.resid)

augment(model_arima_300) %>%
  ACF(.resid) %>%
  autoplot()

augment(model_arima_300) %>%
  PACF(.resid) %>%
  autoplot()


###
# fitovani

fitted_data <- augment(model_arima_300) %>%
  select(cas, data_temp1, .fitted) %>%
  pivot_longer(cols = c(data_temp1, .fitted), names_to = "typ", values_to = "teplota") %>%
  mutate(typ = factor(typ, levels = c("data_temp1", ".fitted")))  # pořadí vrstvy v grafu

ggplot(fitted_data, aes(x = cas, y = teplota, color = typ)) +
  geom_line() +
  labs(title = "Skutečná vs. nafitovaná teplota",
       x = "Čas", y = "Teplota [°C]",
       color = "Typ") +
  scale_color_manual(
    values = c("data_temp1" = "lightblue", ".fitted" = "red"),
    labels = c("data_temp1" = "Skutečná teplota", ".fitted" = "Nafitovaná teplota")
  ) +
  theme_minimal()

# to se mi nelibi :(


################
# forecast
delka_predpovedi = 168

future_data <- tibble(cas = seq(max(data_ts_temp$cas) + hours(1), by = "1 hour", length.out = delka_predpovedi))

# výpočet potřebných proměnných
future_data <- future_data %>%
  mutate(
    den_v_roce = yday(cas),
    den_v_roce2 = den_v_roce^2,
    lag_1 = rep(tail(data_ts_temp$data_temp1, 1), n()),
    lag_24 = rep(tail(data_ts_temp$data_temp1, 24)[1], n())
  )

# doplnění Fourierových členů
X_fourier <- fourier(ts(data_ts_temp$data_temp1, frequency = 24), K = 3, h = delka_predpovedi)
future_data <- bind_cols(future_data, X_fourier)

future_data <- future_data %>%
  as_tsibble(index = cas)

forecast_arima <- forecast(model_arima_300, new_data = future_data)

autoplot(forecast_arima, data_ts_temp)

# TOHLE SE MI LIBI
# az na to, ze jsem zapomnel na mesice :-)






###############################################################################
###############################################################################
###############################################################################
# pridavam mesic a odebiram fourier

# model jen s mesicem 3,0,0 - 2,0,0





data_ts_temp <- data_ts_temp %>%
  mutate(
    hodina = factor(hour(cas)),
    month = factor(month(cas), levels = 1:12),
    den_v_roce = yday(cas),
    den_v_roce2 = den_v_roce^2,
    lag_1 = lag(data_temp1, 1),
    lag_24 = lag(data_temp1, 24)
  )


data_ts_temp <- data_ts_temp %>%
  arrange(cas)

data_ts_temp <- data_ts_temp %>% 
  fill_gaps()



model_arima_mesic <- data_ts_temp %>%
  model(
    arima_temp = ARIMA(
      data_temp1 ~ 
        poly(den_v_roce, 2) +             
        interaction(hodina, month) +                             
        lag_1 + lag_24     
      +
        pdq(3, 1, 0) + PDQ(2,0,0)
      )
  )


model_arima_mesic %>% glance()
print(model_arima_mesic %>% tidy(), n=26)
augment(model_arima_mesic) %>% features(.resid, ljung_box)


# Rezidua
augment(model_arima_mesic) %>% autoplot(.resid)
augment(model_arima_mesic) %>% ACF(.resid) %>% autoplot()
augment(model_arima_mesic) %>% PACF(.resid) %>% autoplot()








fitted_data <- augment(model_arima_mesic) %>%
  select(cas, data_temp1, .fitted) %>%
  pivot_longer(cols = c(data_temp1, .fitted), names_to = "typ", values_to = "teplota") %>%
  mutate(typ = factor(typ, levels = c("data_temp1", ".fitted")))  # pořadí vrstvy v grafu

ggplot(fitted_data, aes(x = cas, y = teplota, color = typ)) +
  geom_line() +
  labs(title = "Skutečná vs. nafitovaná teplota",
       x = "Čas", y = "Teplota [°C]",
       color = "Typ") +
  scale_color_manual(
    values = c("data_temp1" = "lightblue", ".fitted" = "red"),
    labels = c("data_temp1" = "Skutečná teplota", ".fitted" = "Nafitovaná teplota")
  ) +
  theme_minimal()







###############################################################################x

# Nová budoucí data
delka_predpovedi <- 168
future_data <- tibble(cas = seq(max(data_ts_temp$cas) + hours(1), by = "1 hour", length.out = delka_predpovedi)) %>%
  mutate(
    den_v_roce = yday(cas),
    den_v_roce2 = den_v_roce^2,
    month = factor(month(cas), levels = 1:12),
    lag_1 = rep(tail(data_ts_temp$data_temp1, 1), delka_predpovedi),
    lag_24 = rep(tail(data_ts_temp$data_temp1, 24)[1], delka_predpovedi)
  )

# 4. Dummy pozorování pro chybějící úrovně faktoru 'month'
dummy_months <- tibble(
  cas = as.POSIXct(sprintf("1999-01-%02d", 1:12), tz = "UTC"),
  den_v_roce = 1,
  den_v_roce2 = 1,
  month = factor(1:12, levels = 1:12),
  lag_1 = 0,
  lag_24 = 0
)

# Spojení a konverze na tsibble
future_data_full <- bind_rows(future_data, dummy_months) %>%
  arrange(cas) %>%
  as_tsibble(index = cas) %>%
  fill_gaps()

# 5. Forecast
forecast_arima <- forecast(model_arima_mesic, new_data = future_data_full)

# 6. Filtrování jen reálné predikce (vynechání dummy řádků)
forecast_final <- forecast_arima %>%
  filter(cas >= min(future_data$cas))

# 7. Vykreslení výsledku
autoplot(forecast_final, data = data_ts_temp) +
  labs(
    title = "Predikce teploty pomocí ARIMA modelu s faktorem měsíc",
    x = "Čas", y = "Teplota [°C]"
  ) +
  theme_minimal()