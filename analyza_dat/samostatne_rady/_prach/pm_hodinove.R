
data <- data %>%
  mutate(
    t = as.numeric(difftime(cas, min(cas), units = "hours")),  # časový index v hodinách
    mesic = month(cas),
    ln_pm = log(data_pm100)
  )


data_leto <- data %>% filter(mesic >= 3 & mesic <= 10)

model_leto <- lm(ln_pm ~ t + I(t^2), data = data_leto)
summary(model_leto)
AIC(model_leto)

start_leto <- head(data_leto, 48)
end_leto <- tail(data_leto, 48)

# Průměry predikce
start_value <- mean(predict(model_leto, newdata = start_leto), na.rm = TRUE)
end_value <- mean(predict(model_leto, newdata = end_leto), na.rm = TRUE)


fit_pm <- rep(NA, nrow(data))

fit_pm[data$mesic >= 3 & data$mesic <= 10] <-
  predict(model_leto, newdata = data[data$mesic >= 3 & data$mesic <= 10, ])

fit_pm[data$mesic %in% c(1, 2)] <- start_value
fit_pm[data$mesic %in% c(11, 12)] <- end_value


plot(data$cas, data$data_pm100, type = "l", col = "black",
     main = "Model PM v původní škále – hodinová data", xlab = "Čas", ylab = "PM")
lines(data$cas, exp(fit_pm), col = "blue", lwd = 2)



residua_ln <- data$ln_pm - fit_pm
residua_ln <- ifelse(is.na(fit_pm), NA, data$ln_pm - fit_pm)
residua_ts <- ts(residua_ln, frequency = 24)

acf(residua_ts, na.action = na.pass, lag.max = 200, main = "ACF reziduí")
pacf(residua_ts, na.action = na.pass, lag.max = 100, main = "PACF reziduí")


model_arima_resid <- auto.arima(residua_ts, d = 1, D = 1, seasonal = TRUE,
                                stepwise = FALSE, approximation = FALSE)
summary(model_arima_resid)

plot(residua_ts, main = "Rezidua modelu PM")
lines(fitted(model_arima_resid), col = "red", lwd = 2)






# periodicita roční
perioda <- 365 * 24

# harmonické členy
data$cos8760 <- cos(2 * pi * seq_along(data$data_pm100) / perioda)
data$sin8760 <- sin(2 * pi * seq_along(data$data_pm100) / perioda)

# logaritmus
data$log_pm100 <- log(data$data_pm100)

# lineární model s harmonickými členy
model_pm <- lm(log_pm100 ~ cos8760 + sin8760, data = data)
summary(model_pm)

# rezidua
resid_pm <- residuals(model_pm)

# ACF/PACF vizualizace
acf(resid_pm, lag.max = 200, main = "ACF reziduí PM modelu")
pacf(resid_pm, lag.max = 100, main = "PACF reziduí PM modelu")

# SARIMA na rezidua
fit_sarima_pm <- auto.arima(resid_pm, seasonal=TRUE, stepwise=FALSE, approximation=FALSE)
summary(fit_sarima_pm)

# kombinace fitted
fitted_pm_lm <- fitted(model_pm)
fitted_pm_sarima <- fitted(fit_sarima_pm)

# zarovnání
index_start <- length(fitted_pm_lm) - length(fitted_pm_sarima) + 1
fitted_combined_pm <- fitted_pm_lm[index_start:length(fitted_pm_lm)] + fitted_pm_sarima

# zpět z log
fitted_combined_pm_exp <- exp(fitted_combined_pm)


n_sarima <- length(fitted_combined_pm_exp)
historical_time_fitted <- tail(data$cas, n_sarima)
historical_pm100_fitted <- tail(data$data_pm100, n_sarima)

# graf
plot(historical_time_fitted,
     historical_pm100_fitted,
     type = "l",
     col = "black",
     main = "Model PM100 hodinová data",
     ylab = "PM100 [ug/m3]",
     xlab = "čas")
lines(historical_time_fitted, fitted_combined_pm_exp, col = "red")
legend("topright", legend = c("Skutečné", "Fitted"), col = c("black", "red"), lty = 1)
# případně metriky
library(Metrics)
ok <- complete.cases(data$data_pm100[index_start:length(data$data_pm100)], fitted_combined_pm_exp)
rmse(data$data_pm100[index_start:length(data$data_pm100)][ok], fitted_combined_pm_exp[ok])
mae(data$data_pm100[index_start:length(data$data_pm100)][ok], fitted_combined_pm_exp[ok])


sarima_forecast_pm <- forecast(fit_sarima_pm, h=60)

# predikce trendu (LM)
new_index <- seq(length(data$data_pm100)+1, length(data$data_pm100)+60)
new_cos <- cos(2 * pi * new_index / perioda)
new_sin <- sin(2 * pi * new_index / perioda)
trend_forecast <- predict(model_pm, newdata=data.frame(cos8760=new_cos, sin8760=new_sin))

# kombinace
forecast_combined_pm <- trend_forecast + sarima_forecast_pm$mean
forecast_combined_pm_exp <- exp(forecast_combined_pm)

# 95% intervaly
forecast_lower_exp <- exp(trend_forecast + sarima_forecast_pm$lower[,2])
forecast_upper_exp <- exp(trend_forecast + sarima_forecast_pm$upper[,2])

# časová osa
future_time <- seq(max(data$cas) + 3600, by=3600, length.out=60)

n_sarima <- length(fitted_combined_pm_exp)
historical_time_fitted <- tail(data$cas, n_sarima)

lines(historical_time_fitted, fitted_combined_pm_exp, col="red")

# graf
plot(c(tail(data$cas, 60), future_time),
     c(tail(data$data_pm100, 60), rep(NA,60)),
     type="l", col="black",
     ylim=range(c(tail(data$data_pm100, 60), forecast_lower_exp, forecast_upper_exp), na.rm=TRUE),
     xlab="čas", ylab="PM100 [ug/m3]",
     main="60 hodin predikce + posledních 60 hodin")

# interval
polygon(
  c(future_time, rev(future_time)),
  c(forecast_lower_exp, rev(forecast_upper_exp)),
  col = adjustcolor("lightblue", alpha.f = 0.4),
  border = NA
)
lines(future_time, forecast_combined_pm_exp, col="blue")

legend("topright", legend=c("posledních 60 hod", "predikce", "95% interval"),
       col=c("black","blue","lightblue"), lty=c(1,1,NA), pch=c(NA,NA,15), pt.cex=2, bty="n")



n_sarima <- length(fitted_combined_pm_exp)

# poslední n_sarima časových bodů pro fitted
historical_time_fitted <- tail(data$cas, n_sarima)

# vytvoříme vektor fitted hodnot pro CELOU historii
fitted_hodnoty_full <- rep(NA, length(data$cas))
fitted_hodnoty_full[(length(data$cas) - n_sarima + 1):length(data$cas)] <- fitted_combined_pm_exp

# dataframe
df_pm <- data.frame(
  datum = c(data$cas, future_time),
  typ_datumu = c(rep("historie", length(data$cas)), rep("predikce", 60)),
  skutecne_hodnoty = c(data$data_pm100, rep(NA, 60)),
  fitted_hodnoty = c(fitted_hodnoty_full, rep(NA, 60)),
  predikce = c(rep(NA, length(data$cas)), forecast_combined_pm_exp),
  predikce_lower = c(rep(NA, length(data$cas)), forecast_lower_exp),
  predikce_upper = c(rep(NA, length(data$cas)), forecast_upper_exp)
)

head(df_pm)




#####################################
library(ggplot2)
library(lubridate)

# předpokládám, že datový rámec se jmenuje např. df
# nahraď podle potřeby
# df <- tvoje_data

# přidáme sloupec s hodinou
data$hodina <- hour(data$cas)

# boxplot podle hodin
ggplot(data, aes(x = factor(hodina), y = data_pm100)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Rozdělení koncentrace PM100 podle hodiny",
    x = "Hodina ve dne",
    y = "Koncentrace PM100"
  ) +
  theme_minimal()