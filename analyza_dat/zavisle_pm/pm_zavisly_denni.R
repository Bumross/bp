data_denni$den <- as.numeric(data_denni$datum - as.Date("2024-01-01")) + 1
data_denni$log_auta <- log(data_denni$valid_speed_count)

data_zima <- subset(data_denni, topna_sezona == 1)
data_leto <- subset(data_denni, topna_sezona == 0)

data_zima <- subset(
  data_zima,
  !is.na(data_pm100) &
    !is.na(prumerna_teplota_dne) &
    !is.na(valid_speed_count)
)


mean_log_pm_zima <- mean(log(data_zima$data_pm100), na.rm = TRUE)

resid_zima <- log(data_zima$data_pm100) - mean_log_pm_zima

xreg_zima <- model.matrix(
  ~ prumerna_teplota_dne + log_auta,
  data = data_zima
)


model_pm_zima_arima <- auto.arima(
  resid_zima,
  xreg = xreg_zima
)

summary(model_pm_zima_arima)





data_leto <- subset(
  data_leto,
  !is.na(data_pm100) &
    !is.na(prumerna_teplota_dne) &
    !is.na(valid_speed_count)
)



model_leto <- lm(
  log(data_pm100) ~ den + I(den^2),
  data = data_leto
)

summary(model_leto)

# rezidua
resid_leto <- residuals(model_leto)

# 2. regrese do ARIMA chyb
xreg_leto <- model.matrix(
  ~ prumerna_teplota_dne, #+ log_auta,
  data = data_leto
)

model_pm_leto_arima <- auto.arima(
  resid_leto,
  xreg = xreg_leto
)


summary(model_pm_leto_arima)

##############################################
xreg_leto_full <- model.matrix(
  ~ den + I(den^2) + prumerna_teplota_dne + log_auta,
  data = data_leto
)

model_pm_leto <- Arima(
  log(data_leto$data_pm100),
  xreg = xreg_leto_full,
  order = c(3,0,0)
)



#####################################
# leto
model_leto_lm <- lm(
  log(data_pm100) ~ den + I(den^2) + prumerna_teplota_dne + log_auta,
  data = data_leto
)

summary(model_leto_lm)
checkresiduals(model_leto_lm)
resid_leto <- residuals(model_leto_lm)
model_leto_arima <- auto.arima(resid_leto)
summary(model_leto_arima)

# zima
model_zima_lm <- lm(
  log(data_pm100) ~ prumerna_teplota_dne + log_auta,
  data = data_zima
)
checkresiduals(model_zima_lm)
summary(model_zima_lm)
resid_zima <- residuals(model_zima_lm)
model_zima_arima <- auto.arima(resid_zima)
summary(model_zima_arima)


# vkladam data predpovedi pro auta a teplotu
teplota_pred <- teplota_denni
auta_pred <- auta_denni

teplota_pred$date <- as.Date(teplota_pred$datum)
auta_pred$date <- as.Date(auta_pred$datum)

last_date <- max(data_zima$datum)
future_dates <- seq(last_date + 1, by = "day", length.out = 14)

future_teplota <- teplota_pred %>%
  filter(date %in% future_dates)

future_auta <- auta_pred %>%
  filter(date %in% future_dates)

future_data <- merge(
  future_teplota[, c("date", "pred_400")],
  future_auta[, c("date", "forecast_14d")],
  by = "date"
)

future_data$log_auta <- log(future_data$forecast_14d + 1)
xreg_future <- model.matrix(
  ~ prumerna_teplota_dne + log_auta,
  data = data.frame(
    prumerna_teplota_dne = future_data$pred_400,
    log_auta = future_data$log_auta
  )
)


coef_lm <- coef(model_zima_lm)

pred_log_pm_lm <- coef_lm[1] +
  coef_lm["prumerna_teplota_dne"] * future_data$pred_400 +
  coef_lm["log_auta"] * future_data$log_auta

# připrav xreg pro arima
xreg_future <- model.matrix(
  ~ prumerna_teplota_dne + log_auta,
  data = data.frame(
    prumerna_teplota_dne = future_data$pred_400,
    log_auta = future_data$log_auta
  )
)

# predikce ARIMA na reziduích (naváže na historii)
arima_future <- forecast(
  model_zima_arima,
  h = 14
)

# konečná predikce (lineární část + arima rezidua)
pred_log_pm_total <- pred_log_pm_lm + arima_future$mean

# zpět na PM
pred_pm_total <- exp(pred_log_pm_total)

# výsledek
predikce_final <- data.frame(
  date = future_data$date,
  pm_pred = pred_pm_total
)

last_known <- data_zima %>%
  arrange(datum) %>%
  tail(30) %>%
  select(datum, data_pm100)

# spojíme s predikcí
plot_data <- data.frame(
  date = c(last_known$datum, predikce_final$date),
  pm = c(last_known$data_pm100, predikce_final$pm_pred),
  typ = c(rep("historie", nrow(last_known)), rep("predikce", nrow(predikce_final)))
)

# knihovna pro ggplot
library(ggplot2)

# jednoduchý graf
ggplot(plot_data, aes(x = date, y = pm, color = typ)) +
  geom_line() +
  labs(
    title = "Predikce koncentrace PM v zimním období",
    x = "Datum",
    y = "PM (ug/m3)",
    color = ""
  ) +
  theme_minimal()
