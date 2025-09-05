summary(data$data_no2)

plot(data$cas, data$data_no2,
     type = "l", col = "darkred",
     main = "Hodinová koncentrace NO2",
     xlab = "Datum a čas", ylab = "NO2 [ug/m3]")


library(dplyr)
library(lubridate)
library(ggplot2)

data %>%
  mutate(hodina = hour(cas)) %>%
  group_by(hodina) %>%
  summarise(prumer_no2 = mean(data_no2, na.rm = TRUE)) %>%
  ggplot(aes(x = hodina, y = prumer_no2)) +
  geom_line(color = "blue") +
  labs(title = "Průměrná denní křivka NO2 (24h cyklus)",
       x = "Hodina", y = "NO2 [ug/m3]") +
  theme_minimal()



data %>%
  mutate(den_v_tydnu = wday(cas, label = TRUE)) %>%
  group_by(den_v_tydnu) %>%
  summarise(prumer_no2 = mean(data_no2, na.rm = TRUE)) %>%
  ggplot(aes(x = den_v_tydnu, y = prumer_no2)) +
  geom_col(fill = "steelblue") +
  labs(title = "Průměrná koncentrace NO2 podle dne v týdnu",
       x = "Den v týdnu", y = "NO2 [ug/m3]") +
    theme_minimal()

data$no2_clean <- zoo::na.approx(data$data_no2)



no2_ts <- ts(data$no2_clean, frequency = 24)
decomp <- decompose(no2_ts)
plot(decomp)


spectrum(data$no2_clean, main = "Spektrum hodinové řady NO2")


acf(data$no2_clean, lag.max = 200, main = "ACF hodinové řady NO2")
pacf(data$no2_clean, lag.max = 200, main = "PACF hodinové řady NO2")





perioda <- 365 * 24

data$cos8760 <- cos(2 * pi * seq_along(data$data_no2) / perioda)
data$sin8760 <- sin(2 * pi * seq_along(data$data_no2) / perioda)

model_lm <- lm(no2 ~ cos8760 + sin8760, data = data)
summary(model_lm)


resid_lm <- residuals(model_lm)

# vizualizace
acf(resid_lm, lag.max=200)
pacf(resid_lm, lag.max=200)

# SARIMA na residua
library(forecast)
fit_sarima <- auto.arima(resid_lm, seasonal=TRUE, stepwise=FALSE, approximation=FALSE)
summary(fit_sarima)

fitted_lm <- fitted(model_lm)
fitted_sarima <- fitted(fit_sarima)

index_start <- length(fitted_lm) - length(fitted_sarima) + 1
fitted_combined <- fitted_lm[index_start:length(fitted_lm)] + fitted_sarima


plot(data$cas[index_start:length(fitted_lm)], 
     data$data_no2[index_start:length(fitted_lm)],
     type="l", col="black", lwd=0.7,
     main="Skutečné vs. fitted hodnoty kombinovaného modelu",
     ylab="NO2 [ug/m3]", xlab="čas")

lines(data$cas[index_start:length(fitted_lm)], 
      fitted_combined, col="red", lwd=0.7)

legend("topright", legend=c("Skutečné", "Fitted"), col=c("black","red"), lwd=1)

###
y_actual <- data$data_no2[index_start:length(data$data_no2)]
y_fitted <- fitted_combined

# vyhodíme NA:
ok <- complete.cases(y_actual, y_fitted)

# metriky jen na čistých hodnotách
rmse(y_actual[ok], y_fitted[ok])
mae(y_actual[ok], y_fitted[ok])





##############################################
# log
perioda <- 365 * 24

data$cos8760 <- cos(2 * pi * seq_along(data$data_no2) / perioda)
data$sin8760 <- sin(2 * pi * seq_along(data$data_no2) / perioda)

# log-transformace
data$log_no2 <- log(data$data_no2)

# lineární model na log hodnoty
model_lm <- lm(log_no2 ~ cos8760, data = data)
summary(model_lm)
# sinus nebyl vyznamny
# rezidua
resid_lm <- residuals(model_lm)

# diagnostika reziduí
acf(resid_lm, lag.max=200, main="ACF reziduí log-modelu")
pacf(resid_lm, lag.max=200, main="PACF reziduí log-modelu")

# SARIMA na rezidua
library(forecast)
fit_sarima <- auto.arima(resid_lm, seasonal=TRUE, stepwise=FALSE, approximation=FALSE)
summary(fit_sarima)

# fitted hodnoty
fitted_lm <- fitted(model_lm)
fitted_sarima <- fitted(fit_sarima)

# kombinace: na log škále
index_start <- length(fitted_lm) - length(fitted_sarima) + 1
fitted_combined_log <- fitted_lm[index_start:length(fitted_lm)] + fitted_sarima

# zpět do původní škály
fitted_combined <- exp(fitted_combined_log)

# vizualizace
plot(data$cas[index_start:length(fitted_lm)], 
     data$data_no2[index_start:length(fitted_lm)],
     type="l", col="black", lwd=0.7,
     main="Skutečné vs. fitted hodnoty kombinovaného modelu (log transformace)",
     ylab="NO2 [ug/m3]", xlab="čas")

lines(data$cas[index_start:length(fitted_lm)], 
      fitted_combined, col="red", lwd=0.7)

legend("topright", legend=c("Skutečné", "Fitted"), col=c("black","red"), lwd=1)

# vyhodnocení
y_actual <- data$data_no2[index_start:length(data$data_no2)]
y_fitted <- fitted_combined

ok <- complete.cases(y_actual, y_fitted)

rmse(y_actual[ok], y_fitted[ok])
mae(y_actual[ok], y_fitted[ok])





#################################
library(forecast)

# periodicita pro roční cyklus
perioda <- 365 * 24
data$log_no2 <- log(data$data_no2)

data$cos8760 <- cos(2 * pi * seq_along(data$data_no2) / perioda)
data$cos24 <- cos(2*pi*(1:nrow(data))/24)
data$sin24 <- sin(2*pi*(1:nrow(data))/24)
data$cos24_2 <- cos(2*pi*(1:nrow(data))/(24/2))
data$sin24_2 <- sin(2*pi*(1:nrow(data))/(24/2))
data$cos24_3 <- cos(2*pi*(1:nrow(data))/(24/3))
data$sin24_3 <- sin(2*pi*(1:nrow(data))/(24/3))




# lineární model jen s cosinem
model_lm <- lm(log_no2 ~ cos8760 + 
                 cos24 + sin24 + 
                 cos24_2 + sin24_2 +
                 cos24_3 + sin24_3,
               data = data)

summary(model_lm)

# rezidua
resid_lm <- residuals(model_lm)

# SARIMA na rezidua
fit_sarima <- auto.arima(resid_lm, seasonal=TRUE, stepwise=FALSE, approximation=FALSE)

fit_sarima <- Arima(
  resid_lm,
  order = c(1, 0, 1),
  seasonal = list(order = c(2, 0, 1), period = 24)
)

summary(fit_sarima)

# periodicita roční
perioda <- 365 * 24

# posledních 60 hodin reálných hodnot
obs_tail <- tail(data$data_no2, 60)

# fitted modely
fitted_lm <- fitted(model_lm)
fitted_sarima <- fitted(fit_sarima)

index_start <- length(fitted_lm) - length(fitted_sarima) + 1
fitted_combined <- fitted_lm[index_start:length(fitted_lm)] + fitted_sarima

# predikce SARIMA na 60 hodin
sarima_forecast <- forecast(fit_sarima, h = 60)

new_index <- seq(length(data$data_no2)+1, length(data$data_no2)+60)

# kosinová složka roční
new_cos8760 <- cos(2 * pi * new_index / perioda)

# kosinová složka denní
new_cos24 <- cos(2 * pi * new_index / 24)

# sinusová složka denní
new_sin24 <- sin(2 * pi * new_index / 24)



# predikcni data
newdata <- data.frame(
  cos8760 = new_cos8760,
  cos24 = new_cos24,
  sin24 = new_sin24
)

# predikce trendové složky
trend_forecast <- predict(model_lm, newdata = newdata)

# kombinovaná predikce
forecast_combined <- trend_forecast + sarima_forecast$mean

# zpět z log
forecast_combined_exp <- exp(forecast_combined)
forecast_lower_exp <- exp(trend_forecast + sarima_forecast$lower[,2])
forecast_upper_exp <- exp(trend_forecast + sarima_forecast$upper[,2])

# časová osa
future_time <- seq(max(data$cas) + 3600, by = 3600, length.out = 60)
past_time <- tail(data$cas, 60)

# vykreslení
plot(c(past_time, future_time),
     c(obs_tail, rep(NA, 60)),
     type = "l", col = "black",
     ylim = range(c(obs_tail, forecast_lower_exp, forecast_upper_exp), na.rm=TRUE),
     xlab = "čas", ylab = "NO2 [µg/m3]",
     main = "Predikce na 60 hodin + posledních 60 hodin")

# interval spolehlivosti
polygon(c(future_time, rev(future_time)),
        c(forecast_lower_exp, rev(forecast_upper_exp)),
        col = adjustcolor("lightblue", alpha.f = 0.4),
        border = NA)

# predikce
lines(future_time, forecast_combined_exp, col = "blue")

# legenda
legend("topright",
       legend = c("posledních 60 hodin", "predikce", "95% interval"),
       col = c("black", "blue", adjustcolor("lightblue", alpha.f = 0.4)),
       lty = c(1, 1, NA),
       pch = c(NA, NA, 15),
       pt.cex = 2,
       bty = "n")




###############################
# fitted hodnoty v logu
# délka fitted_sarima
n_sarima <- length(fitted_sarima)

# posledních n_sarima fitted z LM:
fitted_lm_tail <- tail(fitted_lm, n_sarima)

# kombinace:
fitted_combined <- fitted_lm_tail + fitted_sarima

residuals_combined <- tail(data$log_no2, n_sarima) - fitted_combined

residuals_combined <- residuals_combined[!is.na(residuals_combined)]


par(mfrow = c(2, 2))
plot(residuals_combined, type = "l", main = "Rezidua kombinovaného modelu")
acf(residuals_combined, lag.max = 100, main = "ACF reziduí")
pacf(residuals_combined, lag.max = 100, main = "PACF reziduí")
hist(residuals_combined, breaks = 30, main = "Histogram reziduí", xlab = "rezidua")

qqnorm(residuals_combined)
qqline(residuals_combined, col = "red")

shapiro.test(residuals_combined)
Box.test(residuals_combined, lag = 24, type = "Ljung-Box")



#####################################################################
n_sarima <- length(fitted_combined_exp)

fitted_hodnoty_full <- rep(NA, length(data$cas))
fitted_hodnoty_full[(length(data$cas) - n_sarima + 1):length(data$cas)] <- fitted_combined_exp

# sloučení
df_all <- data.frame(
  datum = c(data$cas, future_time),
  typ_datumu = c(rep("historie", length(data$cas)), rep("predikce", length(future_time))),
  skutecne_hodnoty = c(data$data_no2, rep(NA, length(future_time))),
  fitted_hodnoty = c(fitted_hodnoty_full, rep(NA, length(future_time))),
  predikovane_hodnoty = c(rep(NA, length(data$cas)), forecast_combined_exp),
  predikce_lower = c(rep(NA, length(data$cas)), forecast_lower_exp),
  predikce_upper = c(rep(NA, length(data$cas)), forecast_upper_exp)
)

head(df_all, 10)