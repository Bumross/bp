hist(data_denni$data_no2, breaks = 30, main = "Histogram NO2", xlab = "NO2")

boxplot(data_no2 ~ mesic, data = data_denni, main = "NO2 podle měsíce")
boxplot(data_no2 ~ topeni, data = data_denni, main = "NO2 podle topné sezóny")
plot(data_no2 ~ den, data = data_denni, main = "NO2 v čase")
# extremni hodnoty v lednu - proc, nevim

data_leto_no2 <- data_denni %>% filter(mesic > 2 & mesic < 11)

model_leto_no2 <- lm(data_no2 ~ den + I(den^2), data = data_leto_no2)

summary(model_leto_no2)




####
# hledani sezonnosti:
data_denni <- data_denni %>%
  mutate(den_v_tydnu = wday(datum, label = TRUE, week_start = 1)) # Po–Ne

boxplot(data_no2 ~ den_v_tydnu, data = data_denni,
        main = "NO₂ podle dne v týdnu", ylab = "NO₂ [μg/m³]")

aggregate(data_no2 ~ mesic, data = data_denni, FUN = mean)

no2_ts <- ts(data_denni$data_no2, frequency = 365)

spec.pgram(no2_ts, main = "Periodogram NO₂", log = "no")


no2_ts <- ts(data_denni$data_no2, frequency = 7)

stl_no2 <- stl(no2_ts, s.window = "periodic")
plot(stl_no2)


########
model_sez <- lm(data_no2 ~ den + I(den^2) + factor(den_v_tydnu) + factor(topeni), data = data_denni)
summary(model_sez)

# den a den2 je dobrej, den_v_tydnu je k nicemu a topeni taky - neni vazan na topnou sezonu


##
K <- 2
period <- 365

data_denni <- data_denni %>%
  mutate(
    cos_y1 = cos(2 * pi * 1 * den / period),
    sin_y1 = sin(2 * pi * 1 * den / period),
    cos_y2 = cos(2 * pi * 2 * den / period),
    sin_y2 = sin(2 * pi * 2 * den / period)
  )

model_base <- lm(data_no2 ~ den + I(den^2) +
                   cos_y1 + sin_y1 + cos_y2 + sin_y2,
                 data = data_denni)

acf(residuals(model_base))
# spatny proste, fourierovy rady v tom nehraji zadnou rolu



###
model_gls <- gls(data_no2 ~ den + I(den^2),
                 correlation = corAR1(form = ~ den),
                 data = data_denni)

summary(model_gls)

###
data_denni$fitted <- fitted(model_gls)

# Forecast například na dalších 30 dní:
future_data <- data_denni %>%
  slice_tail(n = 30)  # nebo připrav data s novými dny

# Pozor: gls neumí predikci jako `forecast()`, ale můžeš použít `predict()`:
pred <- predict(model_gls, newdata = future_data)

ggplot(data_denni, aes(x = datum)) +
  geom_line(aes(y = data_no2), color = "black", size = 1, alpha = 0.7) +
  geom_line(aes(y = fitted), color = "blue") +
  labs(title = "Observed vs Fitted NO₂ values",
       y = "NO₂", x = "Datum") +
  theme_minimal()





# 2. Vytvoření 30denní predikce (přes predict)
max_den <- max(data_denni$den)
future_den <- (max_den + 1):(max_den + 100)
future_datum <- seq(max(data_denni$datum) + 1, by = "day", length.out = 100)
future_data <- data.frame(
  den = future_den,
  datum = future_datum
)

future_data$predicted <- predict(model_gls, newdata = future_data)

n <- nrow(data_denni)
split_point <- n - 30
train_data <- data_denni[1:split_point, ]
test_data <- data_denni[(split_point + 1):n, ]

# 4. Znovuvytvoření modelu pouze na trénovacích datech
model_val <- gls(data_no2 ~ den + I(den^2),
                 correlation = corAR1(form = ~ den),
                 data = train_data)

test_data$predicted <- predict(model_val, newdata = test_data)

# 5. Vykreslení
ggplot() +
  geom_line(data = data_denni, aes(x = datum, y = data_no2), color = "black", size = 1, alpha = 0.7) +
  geom_line(data = data_denni, aes(x = datum, y = fitted), color = "blue", linetype = "dashed") +
  geom_line(data = future_data, aes(x = datum, y = predicted), color = "red") +
  geom_line(data = test_data, aes(x = datum, y = predicted), color = "green", linetype = "dashed") +
  geom_vline(xintercept = data_denni$datum[split_point], color = "darkred") +
  geom_vline(xintercept = max(data_denni$datum), linetype = "dashed", color = "darkgreen") +
  labs(title = "NO₂ – fitted hodnoty, predikce a porovnání",
       x = "Datum", y = "NO₂ [μg/m³]",
       caption = "Černá: pozorovaná, modrá: fitted, zelená: validace, červená: predikce do budoucna") +
  theme_minimal()



period <- 365
data_denni <- data_denni %>%
  mutate(
    cos_y1 = cos(2 * pi * 1 * den / period),
    sin_y1 = sin(2 * pi * 1 * den / period),
    cos_y2 = cos(2 * pi * 2 * den / period),
    sin_y2 = sin(2 * pi * 2 * den / period)
  )

model_harm <- gls(data_no2 ~ den + I(den^2) + cos_y1 + sin_y1 + cos_y2 + sin_y2,
                  correlation = corAR1(form = ~ den),
                  data = data_denni)


period <- 365
dalsich_dni <- 30

# Poslední den v datech
last_den <- max(data_denni$den)
last_date <- max(data_denni$datum)

# Budoucí data
future_data <- tibble(
  den = (last_den + 1):(last_den + dalsich_dni),
  datum = seq.Date(from = last_date + 1, by = "1 day", length.out = dalsich_dni)
) %>%
  mutate(
    cos_y1 = cos(2 * pi * 1 * den / period),
    sin_y1 = sin(2 * pi * 1 * den / period),
    cos_y2 = cos(2 * pi * 2 * den / period),
    sin_y2 = sin(2 * pi * 2 * den / period)
  )

future_data$pred <- predict(model_harm, newdata = future_data)

data_denni$fitted <- predict(model_harm)

plot_data <- bind_rows(
  data_denni %>% select(datum, data_no2, fitted) %>% mutate(typ = "pozorováno"),
  future_data %>% select(datum, pred) %>% rename(fitted = pred) %>%
    mutate(data_no2 = NA, typ = "predikce")
)


ggplot(plot_data, aes(x = datum)) +
  geom_line(aes(y = data_no2), color = "black") +
  geom_line(aes(y = fitted, linetype = typ, color = typ)) +
  scale_color_manual(values = c("pozorováno" = "blue", "predikce" = "red")) +
  scale_linetype_manual(values = c("pozorováno" = "solid", "predikce" = "dashed")) +
  labs(title = "NO₂ – fitted hodnoty a predikce",
       y = "NO₂ [µg/m³]",
       x = "Datum") +
  theme_minimal()










################################################################################
data_denni <- data_denni %>%
  mutate(
    mesic = month(datum),
    den = as.numeric(datum - min(datum)) + 1,
    log_no2 = log(data_no2),
    is_winter = mesic %in% c(10 ,11, 12, 1, 2, 3, 4)
  )

# 2. Zimní model na logaritmovaných hodnotách
data_zima <- data_denni %>% filter(is_winter)
model_log_zima <- lm(log_no2 ~ den_center + I(-1 * den_center^2), data = data_zima)

#model_log_zima <- lm(log_no2 ~ den + I(den^2), data = data_zima)
summary(model_log_zima)  # kontrola

# 3. Průměrná log-hodnota pro léto
leto_log_mean <- mean(data_denni$log_no2[!data_denni$is_winter], na.rm = TRUE)

# 4. Predikce log-hodnot
data_denni <- data_denni %>%
  mutate(
    pred_log = if_else(
      is_winter,
      predict(model_log_zima, newdata = .),
      leto_log_mean
    ),
    pred = exp(pred_log)  # zpětná transformace
  )

# 5. Vizualizace
ggplot(data_denni, aes(x = datum)) +
  geom_line(aes(y = data_no2), color = "black", size = 0.6) +
  geom_line(aes(y = pred), color = "darkred", size = 1) +
  labs(title = "NO2 – model se zimní parabolou a logaritmickou transformací",
       y = "NO2 [µg/m³]", x = "Datum") +
  theme_minimal()






########

# --- 1. Příprava základních dat ---
data_denni <- data_denni %>%
  mutate(
    mesic = month(datum),
    den_v_roce = yday(datum),
    is_winter = mesic >= 11 | mesic <= 4
  )

# --- 2. Výpočet středu zimy z historických zimních dat ---
data_zima <- data_denni %>% filter(is_winter)
stred_zimy <- mean(data_zima$den_v_roce)

# --- 3. Model zimní paraboly na log hodnoty ---
data_zima <- data_zima %>%
  mutate(
    den_center = if_else(
      den_v_roce > 300,             # listopad a prosinec
      den_v_roce - 366 + stred_zimy,
      den_v_roce - stred_zimy
    ),
    log_no2 = log(data_no2)
  )

model_log_zima <- lm(log_no2 ~ den_center + I(den_center^2), data = data_zima)

# --- 4. Vytvoření budoucích dat ---
future_dates <- seq(
  from = max(data_denni$datum) + 1,
  by = "day",
  length.out = 60
)

data_future <- tibble(datum = future_dates) %>%
  mutate(
    mesic = month(datum),
    den_v_roce = yday(datum),
    is_winter = mesic >= 11 | mesic <= 4,
    den_center = if_else(
      den_v_roce > 300,
      den_v_roce - 366 + stred_zimy,
      den_v_roce - stred_zimy
    )
  )

# --- 5. Spojení minulosti a budoucnosti ---
data_all <- bind_rows(
  data_denni %>% select(datum, data_no2, mesic, den_v_roce, is_winter) %>%
    mutate(historie = TRUE),
  data_future %>% mutate(data_no2 = NA, historie = FALSE)
)

data_all <- data_all %>%
  mutate(
    den_center = if_else(
      den_v_roce > 300,
      den_v_roce - 366 + stred_zimy,
      den_v_roce - stred_zimy
    )
  )

# Samostatně spočítáme predikce pouze pro zimní řádky
pred_log_values <- predict(model_log_zima, newdata = data_all %>% filter(is_winter), na.action = na.exclude)

# Nyní spojíme zpět
data_all <- data_all %>%
  mutate(
    pred_log = NA_real_
  )

data_all$pred_log[data_all$is_winter] <- pred_log_values

# B) Najdi konstantu z posledního dne zimy (např. konec dubna 2024)
last_april_value <- data_all %>%
  filter(historie, month(datum) == 4) %>%
  filter(datum == max(datum)) %>%
  pull(pred_log)

# C) Dosaď do léta konstantu
data_all <- data_all %>%
  mutate(
    pred_log = if_else(is_winter, pred_log, last_april_value),
    pred = exp(pred_log)
  )

# --- 7. Vizualizace ---
ggplot(data_all, aes(x = datum)) +
  geom_line(aes(y = data_no2), color = "black") +
  geom_line(aes(y = pred), color = "red", size = 1) +
  labs(
    title = "Predikce NO2 na 90 hodnot dopředu",
    x = "Datum",
    y = "NO2 [µg/m³]"
  ) +
  theme_minimal()
