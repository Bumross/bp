library(tsibble)
library(fable)
library(feasts)
library(fabletools)
library(tidyverse)
library(lubridate)


#######################################
### model na autech                    
#######################################

### HODINOVÁ DATA

# vytvoření objektu tsibble (časové řady) pro auta
data_ts <- data %>%
  select(cas, valid_speed_count) %>%
  mutate(cas = as_datetime(cas)) %>%
  as_tsibble(index = cas)


data_ts <- data_ts %>%
  mutate(
    den_v_tydnu = factor(data$den_v_tydnu),
    velke_svatky = data$velke_svatky,
    prazdniny = data$letni_prazdniny,
    hodina = data$hodina
  )


# modelu se nelibilo, ze nejdou za sebou casove stopy
#(ale navadi mu null hodnoty)
data_ts <- data_ts %>%
  fill_gaps()


# test diferenciace
data_ts %>%
  features(valid_speed_count, unitroot_kpss)
# p-hodnota = 0.1 > 0.05 (ale ne zas tak o moc),
#on si to kdyztak prechrousta sam
# -> test na diferenciaci d=1 (zamítnutí H0 -> d=0)


###############################################################################
# 1. pokus

# tvorim arima s fourierem 24 a 168
# hodnota K je neco jako citlivost na nepravidelne vlny
#(cim vyssi, tim jemnejsi)
model_auta <- data_ts %>%
  model(
    arima_final = ARIMA(
      valid_speed_count ~ 
        fourier(24, K = 2) + 
        fourier(168, K = 2)
    )
  )


#LM w/ ARIMA(0,1,4)(0,0,2)[24] errors
# i tak si to vybralo první diferenci klasickou
# sezónní diference = 0
# takze si to vybralo stejne, jako jste vybrala vy :-)

model_auta %>%
  gg_tsresiduals()

# acf a pacf
augment(model_auta) %>%
  ACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "ACF reziduí modelu aut")

augment(model_auta) %>%
  PACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "PACF reziduí modelu aut")

res <- residuals(model_auta)$arima_final
res <- augment(model_auta)$.resid

var(res, na.rm = TRUE)
var(diff(res, 1), na.rm = TRUE)          # klasická diference
var(diff(res, 24), na.rm = TRUE)         # denní sezónní diference
var(diff(diff(res, 1), 1), na.rm = TRUE) # 2. klasická diference
var(diff(res, 7*24), na.rm = TRUE)       # týdenní sezónní diference

augment(model_auta) %>%
  features(.resid, ljung_box, lag = 24)

augment(model_auta) %>%
  features(.resid, ljung_box, lag = 168)

glance(model_auta)

augment(model_auta) %>%
  ggplot(aes(x = cas)) +
  geom_line(aes(y = valid_speed_count), color = "gray") +
  geom_line(aes(y = .fitted), color = "blue") +
  labs(title = "Model vs. skutečnost (auta)")

###############################################################################
# 2. pokus
# zvysim složitost fourier slozek
# ps: pridal jsem novou promennou: velke svatky
# (velikonoce (pouze pondeli),
#rijnovy svatek, vanoce, novy rok)


model_auta2 <- data_ts %>%
  model(
    arima_final = ARIMA(
      valid_speed_count ~ 
        fourier(24, K = 4) + 
        fourier(168, K = 3)
    )
  )

model_auta2 %>%
  gg_tsresiduals()

report(model_auta2)
# k=4 u 24 hodin vypada dobre
# k=3 u 168 hodin uz tak dobry neni

# acf a pacf
augment(model_auta2) %>%
  ACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "ACF reziduí modelu aut")

augment(model_auta2) %>%
  PACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "PACF reziduí modelu aut")

res <- residuals(model_auta2)$arima_final
res2 <- augment(model_auta2)$.resid

var(res2, na.rm = TRUE)
var(diff(res2, 1), na.rm = TRUE)          # klasická diference
var(diff(res2, 24), na.rm = TRUE)         # denní sezónní diference
var(diff(diff(res2, 1), 1), na.rm = TRUE) # 2. klasická diference
var(diff(res2, 7*24), na.rm = TRUE)       # týdenní sezónní diference

augment(model_auta2) %>%
  features(.resid, ljung_box, lag = 24)

augment(model_auta2) %>%
  features(.resid, ljung_box, lag = 168)

glance(model_auta2)

augment(model_auta2) %>%
  ggplot(aes(x = cas)) +
  geom_line(aes(y = valid_speed_count), color = "gray") +
  geom_line(aes(y = .fitted), color = "blue") +
  labs(title = "Model vs. skutečnost (auta)")

###############################################################################
# 3. pokus
# ponechani starych K
# pridani dalsich regresoru
# den v tydnu, velke svatky


model_auta3 <- data_ts %>%
  model(
    arima_final = ARIMA(
      valid_speed_count ~ 
        fourier(24, K = 2) +
        fourier(168, K = 2) +
        factor(den_v_tydnu) +
        velke_svatky
    )
  )

model_auta3 %>%
  gg_tsresiduals()

report(model_auta3)
# sigma^2 estimated as 58314:  log likelihood=-62469.83
# AIC=124981.7   AICc=124981.8   BIC=125131.2




# acf a pacf
augment(model_auta3) %>%
  ACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "ACF reziduí modelu aut")

augment(model_auta3) %>%
  PACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "PACF reziduí modelu aut")

res <- residuals(model_auta3)$arima_final
res3 <- augment(model_auta3)$.resid

var(res3, na.rm = TRUE)
var(diff(res3, 1), na.rm = TRUE)          # klasická diference
var(diff(res3, 24), na.rm = TRUE)         # denní sezónní diference
var(diff(diff(res3, 1), 1), na.rm = TRUE) # 2. klasická diference
var(diff(res3, 7*24), na.rm = TRUE)       # týdenní sezónní diference

augment(model_auta3) %>%
  features(.resid, ljung_box, lag = 24)

augment(model_auta3) %>%
  features(.resid, ljung_box, lag = 168)

glance(model_auta3)

augment(model_auta3) %>%
  ggplot(aes(x = cas)) +
  geom_line(aes(y = valid_speed_count), color = "gray") +
  geom_line(aes(y = .fitted), color = "blue") +
  labs(title = "Model vs. skutečnost (auta)")

###############################################################################
# zkousim jenom ty svatky zatraceny

model_auta5 <- data_ts %>%
  model(
    arima_final = ARIMA(
      valid_speed_count ~ 
        velke_svatky
    )
  )

report(model_auta5)
# to nevypada ale tak moc hrozne, zkusim to tam dat
# upravil jsem delsi obdobi u Vanoc)
###############################################################################
# zkousim prazdniny


model_auta6 <- data_ts %>%
  model(
    arima_final = ARIMA(
      valid_speed_count ~ 
        prazdniny
    )
  )

report(model_auta6)
# prazdniny velke nic


###############################################################################
# 4. pokus
# pridat den v tydnu a hodinu
# zvysuju k=3 a snizuju k=1 u 168

model_auta4 <- data_ts %>%
  model(
    arima_final = ARIMA(
      valid_speed_count ~ 
        fourier(24, K = 3) +
        fourier(168, K = 1) +
        velke_svatky + den_v_tydnu + hodina
    )
  )

model_auta4 %>%
  gg_tsresiduals()


report(model_auta4)


# acf a pacf
augment(model_auta4) %>%
  ACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "ACF reziduí modelu aut")

augment(model_auta4) %>%
  PACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "PACF reziduí modelu aut")

res <- residuals(model_auta4)$arima_final
res4 <- augment(model_auta4)$.resid

var(res3, na.rm = TRUE)
var(diff(res3, 1), na.rm = TRUE)          # klasická diference
var(diff(res3, 24), na.rm = TRUE)         # denní sezónní diference
var(diff(diff(res3, 1), 1), na.rm = TRUE) # 2. klasická diference
var(diff(res3, 7*24), na.rm = TRUE)       # týdenní sezónní diference

augment(model_auta4) %>%
  features(.resid, ljung_box, lag = 24)

augment(model_auta4) %>%
  features(.resid, ljung_box, lag = 168)

glance(model_auta4)

augment(model_auta4) %>%
  ggplot(aes(x = cas)) +
  geom_line(aes(y = valid_speed_count), color = "gray") +
  geom_line(aes(y = .fitted), color = "blue") +
  labs(title = "Model vs. skutečnost (auta)")

###############################################################################
# pokus 7

# bez hodiny, bez velkych svatku (v kombinovanem modelu neni vyznamny)

model_auta7 <- data_ts %>%
  model(
    arima_final = ARIMA(
      valid_speed_count ~ 
        fourier(24, K = 3) +
        fourier(168, K = 1) +
        den_v_tydnu + velke_svatky
    )
  )
# 111 101

model_auta7 %>%
  gg_tsresiduals()


report(model_auta7)


# acf a pacf
augment(model_auta7) %>%
  ACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "ACF reziduí modelu aut")

augment(model_auta7) %>%
  PACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "PACF reziduí modelu aut")

res <- residuals(model_auta7)$arima_final
res7 <- augment(model_auta7)$.resid

var(res3, na.rm = TRUE)
var(diff(res3, 1), na.rm = TRUE)          # klasická diference
var(diff(res3, 24), na.rm = TRUE)         # denní sezónní diference
var(diff(diff(res3, 1), 1), na.rm = TRUE) # 2. klasická diference
var(diff(res3, 7*24), na.rm = TRUE)       # týdenní sezónní diference

augment(model_auta7) %>%
  features(.resid, ljung_box, lag = 24)

augment(model_auta7) %>%
  features(.resid, ljung_box, lag = 168)

glance(model_auta7)

augment(model_auta7) %>%
  ggplot(aes(x = cas)) +
  geom_line(aes(y = valid_speed_count), color = "gray") +
  geom_line(aes(y = .fitted), color = "blue") +
  labs(title = "Model vs. skutečnost (auta)")

###############################################################################
###############################################################################
# pokus 8

# bbez velkych svatku, K=4

model_auta8 <- data_ts %>%
  model(
    arima_final = ARIMA(
      valid_speed_count ~ 
        fourier(24, K = 4) +
        fourier(168, K = 1) +
        den_v_tydnu
    )
  )
# 

model_auta8 %>%
  gg_tsresiduals()


report(model_auta8)


# acf a pacf
augment(model_auta8) %>%
  ACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "ACF reziduí modelu aut")

augment(model_auta8) %>%
  PACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "PACF reziduí modelu aut")

res <- residuals(model_auta8)$arima_final
res8 <- augment(model_auta8)$.resid

var(res3, na.rm = TRUE)
var(diff(res3, 1), na.rm = TRUE)          # klasická diference
var(diff(res3, 24), na.rm = TRUE)         # denní sezónní diference
var(diff(diff(res3, 1), 1), na.rm = TRUE) # 2. klasická diference
var(diff(res3, 7*24), na.rm = TRUE)       # týdenní sezónní diference

augment(model_auta8) %>%
  features(.resid, ljung_box, lag = 24)

augment(model_auta8) %>%
  features(.resid, ljung_box, lag = 168)

glance(model_auta8)

augment(model_auta8) %>%
  ggplot(aes(x = cas)) +
  geom_line(aes(y = valid_speed_count), color = "gray") +
  geom_line(aes(y = .fitted), color = "blue") +
  labs(title = "Model vs. skutečnost (auta)")


###############################################################################
model_auta9 <- data_ts %>%
  model(
    arima_final = ARIMA(
      valid_speed_count ~ 
        fourier(24, K = 4) +
        fourier(168, K = 1) +
        velke_svatky
    )
  )
# 

model_auta9 %>%
  gg_tsresiduals()


report(model_auta9)


# acf a pacf
augment(model_auta9) %>%
  ACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "ACF reziduí modelu aut")

augment(model_auta9) %>%
  PACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "PACF reziduí modelu aut")

res <- residuals(model_auta9)$arima_final
res8 <- augment(model_auta9)$.resid

var(res3, na.rm = TRUE)
var(diff(res3, 1), na.rm = TRUE)          # klasická diference
var(diff(res3, 24), na.rm = TRUE)         # denní sezónní diference
var(diff(diff(res3, 1), 1), na.rm = TRUE) # 2. klasická diference
var(diff(res3, 7*24), na.rm = TRUE)       # týdenní sezónní diference

augment(model_auta9) %>%
  features(.resid, ljung_box, lag = 24)

augment(model_auta9) %>%
  features(.resid, ljung_box, lag = 168)

glance(model_auta9)

augment(model_auta9) %>%
  ggplot(aes(x = cas)) +
  geom_line(aes(y = valid_speed_count), color = "gray") +
  geom_line(aes(y = .fitted), color = "blue") +
  labs(title = "Model vs. skutečnost (auta)")






################################################################################
################################################################################
################################################################################
################################################################################
# AUTA DENNI


auta <- data_denni$valid_speed_count
den <- data_denni$den_v_tydnu
svatek <- data_denni$velke_svatky
prazd <- data_denni$letni_prazdniny

lm1 <- lm(auta ~ as.factor(hod) + as.factor(den) + as.factor(svatek) + as.factor(prazd))
Anova(lm1)

oldpar <- par(mfrow = c(2,2))
plot(lm1)
par(oldpar)

res <- ts(residuals(lm1))
plot(res)
