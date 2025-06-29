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
# 0 1 4,   2 0 0

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

var(res4, na.rm = TRUE)
var(diff(res4, 1), na.rm = TRUE)          # klasická diference
var(diff(res4, 24), na.rm = TRUE)         # denní sezónní diference
var(diff(diff(res4, 1), 1), na.rm = TRUE) # 2. klasická diference
var(diff(res4, 7*24), na.rm = TRUE)       # týdenní sezónní diference

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


# AIC 123 097
# BIC 124 261

###############################################################################
# pokus 7


model_auta7 <- data_ts %>%
  model(
    arima_final = ARIMA(
      valid_speed_count ~ 
        fourier(24, K = 3) +
        fourier(168, K = 1) +
        den_v_tydnu + velke_svatky
    )
  )
# 1 1 1 , 1 0 0

model_auta7 %>%
  gg_tsresiduals()


report(model_auta7)
# AIC 124 383
# BIC 124 627

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
        den_v_tydnu + velke_svatky
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
# 0 1 1    0 0 1

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

var(res8, na.rm = TRUE)
var(diff(res8, 1), na.rm = TRUE)          # klasická diference
var(diff(res8, 24), na.rm = TRUE)         # denní sezónní diference
var(diff(diff(res8, 1), 1), na.rm = TRUE) # 2. klasická diference
var(diff(res8, 7*24), na.rm = TRUE)       # týdenní sezónní diference

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


# AIC 124 521
# BIC 124 628
################################################################################
################################################################################
################################################################################
################################################################################

data_ts_hodinove <- data %>%
  mutate(
    cas = cas,                               
    hodina = hour(cas),
    den_v_tydnu = wday(cas, week_start = 1),          
    den_v_tydnu = factor(den_v_tydnu, levels = 1:7),
    mesice = factor(month(cas)),
    den = date(cas),
    letni_prazdniny = factor(letni_prazdniny, levels = c(0, 1)),
    velke_svatky = factor(velke_svatky, levels = c(0, 1))
 
  ) %>%
  as_tsibble(index = cas) %>%
  fill_gaps()        




model_hodinovy <- data_ts_hodinove %>%
  model(
    ARIMA(
      valid_speed_count ~ 
        velke_svatky + letni_prazdniny +
        fourier(24, K = 3) + fourier(168, K = 2) + pdq(2,0,3) + PDQ(1,0,0,24)
    )
  )
# 203 100

model_hodinovy %>%
  gg_tsresiduals()

report(model_hodinovy)

model_hodinovy %>% augment() %>% features(.resid, ljung_box, lag = 24, dof = 5)


# acf a pacf
augment(model_hodinovy) %>%
  ACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "ACF reziduí modelu aut")

augment(model_hodinovy) %>%
  PACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "PACF reziduí modelu aut")

res <- residuals(model_hodinovy)$arima_final
res8 <- augment(model_hodinovy)$.resid

var(res8, na.rm = TRUE)
var(diff(res8, 1), na.rm = TRUE)          # klasická diference
var(diff(res8, 24), na.rm = TRUE)         # denní sezónní diference
var(diff(diff(res8, 1), 1), na.rm = TRUE) # 2. klasická diference
var(diff(res8, 7*24), na.rm = TRUE)       # týdenní sezónní diference

augment(model_hodinovy) %>%
  features(.resid, ljung_box, lag = 24)

augment(model_hodinovy) %>%
  features(.resid, ljung_box, lag = 168)

glance(model_hodinovy)

augment(model_hodinovy) %>%
  ggplot(aes(x = cas)) +
  geom_line(aes(y = valid_speed_count), color = "gray") +
  geom_line(aes(y = .fitted), color = "blue") +
  labs(title = "Model vs. skutečnost (auta)")



# model vypada hodne dobre, fourierovy rady hodne pomohly s rychlostmi vypoctu
########################x
# predikce

# musim nejdriv vytvorit dalsi hodnoty, ktere se daji predikovat dopredu
# cas, svatky, letni_prazdniny
max_time <- max(data_ts_hodinove$cas)

new_data <- tibble(
  cas = seq(from = max_time + hours(1), by = "1 hour", length.out = 336)
) %>%
  mutate(
    hodina = hour(cas),
    den_v_tydnu = wday(cas, week_start = 1),
    den_v_tydnu = factor(den_v_tydnu, levels = 1:7),
    mesice = factor(month(cas)),
    den = date(cas),
    velke_svatky = factor(0, levels = c(0, 1)),          # předpokládáme že nejsou
    letni_prazdniny = factor(0, levels = c(0, 1))        # předpokládáme že nejsou
  ) %>%
  as_tsibble(index = cas)


forecast_hodinovy <- forecast(model_hodinovy, new_data = new_data)

autoplot(forecast_hodinovy, data_ts_hodinove) +
  labs(
    title = "Predikce počtu projetých aut na 14 dní",
    x = "Datum a čas",
    y = "Počet projetých aut za hodinu"
  ) +
  scale_x_datetime(date_labels = "%d.%m. %H:%M") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# chci to vykreslit hezcejc
#takze beru posledni mesic + predikovane hodnoty

data_last_month <- data_ts_hodinove %>%
  filter(cas >= max(cas) - days(14))

autoplot(forecast_hodinovy, data_last_month) +
  labs(
    title = "Predikce počtu projetých aut na 14 dní",
    x = "Datum a čas",
    y = "Počet projetých aut za hodinu"
  ) +
  scale_x_datetime(date_labels = "%d.%m.", date_breaks = "3 days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###########
forecast_hodinovy <- forecast(
  model_hodinovy,
  new_data = new_data,
  level = c(80, 95)
)


resid_sd <- model_hodinovy %>% 
  residuals() %>% 
  as_tibble() %>% 
  pull(.resid) %>% 
  sd(na.rm = TRUE)

# Dodej 95% a 80% intervaly za použití normální distribuce
forecast_hodinovy <- forecast_hodinovy %>%
  mutate(
    .mean = pmax(.mean, 0),
    .lower_80 = pmax(.mean - 1.28 * resid_sd, 0),
    .upper_80 = .mean + 1.28 * resid_sd,
    .lower_95 = pmax(.mean - 1.96 * resid_sd, 0),
    .upper_95 = .mean + 1.96 * resid_sd
  )


# Data za posledních 14 dní
data_last_month <- data_ts_hodinove %>%
  filter(cas >= max(cas) - days(14))

# Oříznutí predikovaných hodnot pod nulou
forecast_hodinovy_fixed <- forecast_hodinovy %>%
  mutate(
    pred = pmax(.mean, 0),
    lower = pmax(as.numeric(.lower_95), 0),
    upper = pmax(as.numeric(.upper_95), 0)
  )

# Vykreslení
data_last_month_plot <- data_last_month %>%
  select(cas, valid_speed_count) %>%
  mutate(
    typ = "Skutečnost",
    y = valid_speed_count
  )

# Připrav predikci
forecast_plot <- forecast_hodinovy_fixed %>%
  as_tibble() %>%
  select(cas, pred, lower, upper) %>%
  mutate(
    typ = "Predikce",
    y = pred
  )

# Spoj
combined_data <- bind_rows(data_last_month_plot, forecast_plot)

# Vykresli
ggplot(combined_data, aes(x = cas)) +
  geom_ribbon(
    data = forecast_plot,
    aes(ymin = lower, ymax = upper),
    fill = "lightblue", alpha = 0.4
  ) +
  geom_line(aes(y = y, color = typ), linewidth = 0.7) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce" = "blue")) +
  labs(
    title = "Predikce počtu projetých vozidel na 14 dní",
    x = "Datum a čas",
    y = "Počet projetých aut za hodinu",
    color = "Typ dat"
  ) +
  scale_x_datetime(date_labels = "%d.%m.", date_breaks = "3 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




##############################################################################
# testovaci x trenovaci predikce

n_total <- nrow(data_ts_hodinove)
n_train <- round(n_total * 0.7)

data_train <- data_ts_hodinove %>% slice(1:n_train)
data_test <- data_ts_hodinove %>% slice((n_train + 1):n_total)

model_train <- data_train %>%
  model(
    ARIMA(
      valid_speed_count ~ 
        velke_svatky + letni_prazdniny +
        fourier(24, K = 3) + fourier(168, K = 2) + pdq(2,0,3) + PDQ(1,0,0)
    )
  )
# 203 100
forecast_test <- forecast(model_train, new_data = data_test) %>%
  mutate(.mean = pmax(.mean, 0))

results <- left_join(
  forecast_test,
  data_test %>% select(cas, valid_speed_count),
  by = "cas"
)

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


accuracy(forecast_test, test_data)
fc <- forecast(model_train, new_data = data_test)
accuracy(fc, data_test)




###############################################################################
##################################################################################
# dat jen zavislost na dnech v tydnu + svatky

model_auta_bez_f <- data_ts_hodinove %>%
  model(
    ARIMA(
      valid_speed_count ~ 
        velke_svatky + letni_prazdniny + den_v_tydnu
    )
  )
#102, 010

model_auta_bez_f %>%
  gg_tsresiduals()


report(model_auta_bez_f)


# acf a pacf
augment(model_hodinovy) %>%
  ACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "ACF reziduí hodinového modelu aut") + theme_minimal()

augment(model_auta_bez_f) %>%
  PACF(.resid, lag_max = 100) %>%
  autoplot() +
  labs(title = "PACF reziduí modelu aut")

res <- residuals(model_auta_bez_f)$arima_final
res_f <- augment(model_auta_bez_f)$.resid

var(res8, na.rm = TRUE)
var(diff(res8, 1), na.rm = TRUE)          # klasická diference
var(diff(res8, 24), na.rm = TRUE)         # denní sezónní diference
var(diff(diff(res8, 1), 1), na.rm = TRUE) # 2. klasická diference
var(diff(res8, 7*24), na.rm = TRUE)       # týdenní sezónní diference

augment(model_hodinovy) %>%
  features(.resid, ljung_box, lag = 24)

augment(model_hodinovy) %>%
  features(.resid, ljung_box, lag = 168)

glance(model_auta_bez_f)

augment(model_auta_bez_f) %>%
  ggplot(aes(x = cas)) +
  geom_line(aes(y = valid_speed_count), color = "gray") +
  geom_line(aes(y = .fitted), color = "blue") +
  labs(title = "Model vs. skutečnost (auta)")


# neni dobry :( denni sezonnost neni vubec chycena


###########
augment(model_hodinovy) %>% 
  features(.innov, features = list(
    LjungBox = ~ljung_box(.),
    ShapiroW = ~shapiro_test(.),
    Mean = ~mean(.),
    Var = ~var(.)
  ))

residua <- augment(model_hodinovy)$.innov
shapiro.test(residua)

rezidua <- augment(model_hodinovy) %>% pull(.resid)

library(nortest)
ad.test(residua)

qqnorm(residua, main = "QQ–plot reziduí hodinového modelu")
qqline(residua, col = "red", lwd = 2)

augment(model_hodinovy) %>% 
  autoplot(.innov) + ggtitle("Rezidua hodinového modelu") + theme_minimal()


p1 <- ggplot(data.frame(residua), aes(x = residua)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "gray", color = "black") +
  geom_density(color = "steelblue", size = 1) +
  labs(title = "Histogram reziduí", x = "Reziduum", y = "Hustota") +
  theme_minimal()

# 2. QQ-plot
p2 <- ggplot(data.frame(residua), aes(sample = residua)) +
  stat_qq() +
  stat_qq_line(color = "steelblue") +
  labs(title = "QQ–plot reziduí") +
  theme_minimal()

# 3. ACF plot – převedeme na dataframe
acf_data <- forecast::Acf(residua, plot = FALSE)
acf_df <- data.frame(lag = acf_data$lag, acf = acf_data$acf)

acf_df_filtered <- acf_df %>% filter(lag != 0)


p3 <- ggplot(acf_df_filtered, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_hline(
    yintercept = c(0, qnorm(0.975)/sqrt(length(residua)), -qnorm(0.975)/sqrt(length(residua))),
    linetype = "dashed", color = "steelblue"
  ) +
  labs(
    title = "Autokorelační funkce (ACF)",
    x = "Zpoždění",
    y = "ACF"
  ) +
  theme_minimal()

# Spojení do jednoho výstupu
(p1 | p2) / p3
