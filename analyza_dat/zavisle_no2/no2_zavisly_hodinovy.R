perioda <- 365 * 24
data$log_no2 <- log(data$data_no2)
data$log_auta <- log(data$valid_speed_count + 1)  # +1 proti log(0)
data$cos8760 <- cos(2 * pi * seq_along(data$data_no2) / perioda)


model_lm2 <- lm(log_no2 ~ cos8760 + data_temp1 + log_auta, data = data)
summary(model_lm2)


checkresiduals(model_lm2)

resid_lm2 <- ts(residuals(model_lm2), frequency =24)

fit_sarima2 <- Arima(
  resid_lm2,
  order = c(5, 1, 0),         # AR(1), MA(1)
  seasonal = list(order = c(2, 0, 0), period = 24) # SAR(1), SMA(1)
)

summary(fit_sarima2)
checkresiduals(fit_sarima2)



## fitting
n_sarima <- length(fitted(fit_sarima2))
pred_lm2_aligned <- tail(pred_lm2, n_sarima)
pred_combined_log <- pred_lm2_aligned + fitted(fit_sarima2)
pred_combined <- exp(pred_combined_log)


mean_temp <- mean(data$data_temp1, na.rm=TRUE)
mean_logauta <- mean(data$log_auta, na.rm=TRUE)

cosin_only_log <- predict(model_lm2, newdata = data.frame(
  cos8760 = data$cos8760,
  data_temp1 = mean_temp,
  log_auta = mean_logauta
))

cosin_only <- exp(cosin_only_log)
cosin_only_aligned <- tail(cosin_only, n_sarima)

# originální data taky zarovnat
no2_aligned <- tail(data$data_no2, n_sarima)

# plot
plot(
  no2_aligned,
  type = "l",
  col = "black",
  lwd = 1,
  main = "NO2 model fit",
  ylab = "NO2 [ug/m3]",
  xlab = "čas"
)
lines(cosin_only_aligned, col = "blue", lwd = 2)
lines(pred_combined, col = "red", lwd = 2)
legend(
  "topright",
  legend = c("skutečná NO2", "cos8760", "LM + SARIMA"),
  col = c("black", "blue", "red"),
  lty = 1,
  lwd = 2
)











#####################
time_idx <- seq_along(data$data_no2)

# zarovnáme fitted SARIMA
n_sarima <- length(fitted(fit_sarima2))

# posun v indexech
shift <- length(time_idx) - n_sarima + 1

# zarovnané predikce
pred_combined_log_sarima <- pred_lm2[shift:length(time_idx)] + fitted(fit_sarima2)
pred_combined_log <- pred_lm2[shift:length(time_idx)] #+ fitted(fit_sarima2)
pred_combined <- exp(pred_combined_log)
pred_combined_sarima <- exp(pred_combined_log_sarima)

# zarovnaná cosin
mean_temp <- mean(data$data_temp1, na.rm=TRUE)
mean_logauta <- mean(data$log_auta, na.rm=TRUE)
cosin_only_log <- predict(model_lm2, newdata = data.frame(
  cos8760 = data$cos8760,
  data_temp1 = mean_temp,
  log_auta = mean_logauta
))
cosin_only <- exp(cosin_only_log)



time_sarima <- shift:length(time_idx)
col_transparent_red <- rgb(1, 0, 0, alpha = 0.5)
col_transparent_blue <- rgb(0, 0, 1, alpha = 0.4)



# kreslit
plot(
  time_idx,
  data$data_no2,
  type = "l",
  col = "black",
  lwd = 1,
  main = "NO2 model fit",
  ylab = "NO2 [ug/m3]",
  xlab = "čas"
)
lines(time_sarima, pred_combined_sarima, col = col_transparent_blue, lwd = 1)
lines(time_sarima, pred_combined, col = col_transparent_red, lwd = 1)
legend(
  "topright",
  legend = c("skutečná NO2", "LM", "LM + SARIMA"),
  col = c("black", "red", "blue"),
  lty = 1,
  lwd = 2
)







#######################
data$day_of_year <- -(seq_along(data$data_no2) / 24) %% 365
data$day_of_year2 <- -(data$day_of_year^2)

model_lm3 <- lm(
  log_no2 ~ day_of_year + day_of_year2 + data_temp1 + log_auta,
  data = data
)
summary(model_lm3)

resid_lm3 <- residuals(model_lm3)
resid_lm3_ts <- ts(resid_lm3, frequency = 24)
checkresiduals(model_lm3)

fit_sarima3 <- Arima(
  resid_lm3_ts,
  order = c(5, 1, 0),
  seasonal = list(order = c(2, 0, 0), period = 24)
)
summary(fit_sarima3)


# predikce
pred_lm3 <- predict(model_lm3, newdata = data)
pred_sarima3 <- fitted(fit_sarima3)

# zarovnání délek
n_sarima <- length(pred_sarima3)
shift <- length(pred_lm3) - n_sarima + 1

pred_combined_log <- pred_lm3[shift:length(pred_lm3)] + pred_sarima3
pred_combined <- exp(pred_combined_log)



mean_temp <- mean(data$data_temp1, na.rm=TRUE)
mean_logauta <- mean(data$log_auta, na.rm=TRUE)

quad_only_log <- predict(model_lm3, newdata = data.frame(
  day_of_year = data$day_of_year,
  day_of_year2 = data$day_of_year2,
  data_temp1 = mean_temp,
  log_auta = mean_logauta
))
quad_only <- exp(quad_only_log)

# připrav indexy
time_idx <- seq_along(data$data_no2)
time_sarima <- shift:length(time_idx)

# vykreslit
plot(
  time_idx,
  data$data_no2,
  type = "l",
  col = "black",
  lwd = 1,
  main = "NO2 model fit (kvadratický trend)",
  ylab = "NO2 [ug/m3]",
  xlab = "čas"
)
lines(time_idx, quad_only, col = "blue", lwd = 2)
lines(time_sarima, pred_combined, col = rgb(1, 0, 0, 0.5), lwd = 1)
legend(
  "topright",
  legend = c("skutečná NO2", "kvadratický trend", "LM + SARIMA"),
  col = c("black", "blue", "red"),
  lty = 1,
  lwd = 2
)

###############################################################################
auta <- auta_hodinova
teplota <- teplota_hodinova

start <- as.POSIXct("2025-01-16 01:00:00", tz="CET")
end   <- as.POSIXct("2025-01-29 23:00:00", tz="CET")

auta_pred <- subset(auta, cas >= start & cas <= end)
teplota_pred <- subset(teplota, cas >= start & cas <= end)

stopifnot(nrow(auta_pred) == nrow(teplota_pred)) # musí souhlasit časové kroky

perioda <- 365 * 24
time_idx <- seq_along(auta_pred$cas) + as.numeric(difftime(start, min(auta$cas), units="hours"))
cos8760 <- cos(2 * pi * time_idx / perioda)

# log vozidel
log_auta <- log(auta_pred$pred_14dni + 1)

# teplota
temp <- teplota_pred$pred_30

# sestavení predikčních dat
newdata <- data.frame(
  cos8760 = cos8760,
  data_temp1 = temp,
  log_auta = log_auta
)

# predikce lineárního modelu
pred_lm2 <- predict(model_lm2, newdata=newdata)

# SARIMA predikce
sarima_forecast <- forecast(fit_sarima2, h=nrow(newdata))
pred_sarima <- sarima_forecast$mean

# kombinace
pred_combined_log <- pred_lm2 + pred_sarima
pred_combined <- exp(pred_combined_log)

# spojíme
pred_result <- data.frame(
  cas = auta_pred$cas,
  pred_lm2 = exp(pred_lm2),
  pred_combined = pred_combined
)

# 14 dnů zpět ze skutečných hodnot
end_real <- as.POSIXct("2025-01-16 00:00:00", tz="CET")
start_real <- end_real - (14*24-1)*3600


pred_result$cas <- as.POSIXct(pred_result$cas, tz="CET")

pred_times <- seq(
  from = start,
  by = "hour",
  length.out = nrow(newdata)
)

pred_result <- data.frame(
  cas = pred_times,
  pred_lm2 = exp(pred_lm2),
  pred_combined = pred_combined
)

# plot
plot(
  real_window$cas,
  exp(real_window$log_no2),
  type = "l",
  col = "black",
  ylab = "NO2 [ug/m3]",
  xlab = "čas",
  main = "Posledních 14 dní + predikce",
  xlim = c(min(real_window$cas), max(pred_result$cas))
)
lines(pred_result$cas, pred_result$pred_combined, col=rgb(1,0,0,0.5), lwd=2)
lines(pred_result$cas, pred_result$pred_lm2, col="blue", lwd=2)


plot(pred_result$cas, pred_result$pred_combined)


plot_df <- data.frame(
  cas = c(real_window$cas, pred_result$cas),
  NO2 = c(exp(real_window$log_no2), pred_result$pred_combined),
  typ = c(rep("skutečné NO2", nrow(real_window)),
          rep("predikce LM+SARIMA", nrow(pred_result)))
)

# pro LM-only predikci další vrstva
plot_df_lm <- data.frame(
  cas = pred_result$cas,
  NO2 = pred_result$pred_lm2,
  typ = "predikce LM"
)

# spojíme
plot_df <- rbind(plot_df, plot_df_lm)

# graf
ggplot(plot_df, aes(x = cas, y = NO2, color = typ)) +
  geom_line() +
  scale_color_manual(
    values = c(
      "skutečné NO2" = "black",
      "predikce LM+SARIMA" = "red",
      "predikce LM" = "blue"
    )
  ) +
  scale_x_datetime(
    date_breaks = "3 day",
    date_labels = "%d/%m/%Y"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle=45, hjust=1)
  ) +
  labs(
    x = "čas",
    y = "NO2 [ug/m3]",
    color = "Legenda",
    title = "Posledních 14 dní + predikce"
  )
