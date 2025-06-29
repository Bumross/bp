data_hodinova$hodina <- as.numeric(
  difftime(data_hodinova$cas, as.POSIXct("2024-01-01 00:00"), units = "hours")
)

data_hodinova$den <- as.numeric(as.Date(data_hodinova$cas) - as.Date("2024-01-01")) + 1
data_hodinova$log_auta <- log(data_hodinova$valid_speed_count)

data_zima_hod <- subset(data_hodinova, topna_sezona == 1)
data_leto_hod <- subset(data_hodinova, topna_sezona == 0)


data_zima_hod <- subset(
  data_zima_hod,
  !is.na(data_pm100) &
    !is.na(data_temp1) &
    !is.na(valid_speed_count)
)

data_leto_hod <- subset(
  data_leto_hod,
  !is.na(data_pm100) &
    !is.na(data_temp1) &
    !is.na(valid_speed_count)
)


model_leto_lm_hod <- lm(
  log(data_pm100) ~ den + I(den^2) + data_temp1 + log_auta,
  data = data_leto_hod
)

summary(model_leto_lm_hod)
checkresiduals(model_leto_lm_hod)
# nema sezonni lagy, takze arima bez sezonnosti, popsano autama

resid_leto_hod <- residuals(model_leto_lm_hod)



model_leto_arima_hod <- auto.arima(resid_leto_hod)
# 4 0 0
summary(model_leto_arima_hod)



#####################
model_zima_lm_hod <- lm(
  log(data_pm100) ~ data_temp1 + log_auta,
  data = data_zima_hod
)

summary(model_zima_lm_hod)
checkresiduals(model_zima_lm_hod)

resid_zima_hod <- residuals(model_zima_lm_hod)

model_zima_arima_hod <- auto.arima(resid_zima_hod)
summary(model_zima_arima_hod)