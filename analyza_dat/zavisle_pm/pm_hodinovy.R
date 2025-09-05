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
  log(data_pm100) ~ data_temp1, #+ log_auta,
  data = data_zima_hod
)

summary(model_zima_lm_hod)
checkresiduals(model_zima_lm_hod)

resid_zima_hod <- residuals(model_zima_lm_hod)

model_zima_arima_hod <- auto.arima(resid_zima_hod)
summary(model_zima_arima_hod)


###########################################x
auta <- auta_hodinova
teplota <- teplota_hodinova

start <- as.POSIXct("2025-01-16 01:00:00", tz="CET")
end   <- as.POSIXct("2025-01-29 23:00:00", tz="CET")

auta$cas <- ifelse(
  nchar(auta$cas) == 10,                  # yyyy-mm-dd
  paste0(auta$cas, " 00:00:00"),
  auta$cas
)
auta$cas <- as.POSIXct(auta$cas, tz="CET")

teplota$cas <- ifelse(
  nchar(teplota$cas) == 10,
  paste0(teplota$cas, " 00:00:00"),
  teplota$cas
)
teplota$cas <- as.POSIXct(teplota$cas, tz="CET")

auta_pred <- subset(auta, cas >= start & cas <= end)
teplota_pred <- subset(teplota, cas >= start & cas <= end)


future_data <- auta_pred %>%
  select(cas, valid_speed_count = pred_14dni) %>%
  inner_join(
    teplota_pred %>% select(cas, data_temp1 = pred_30),
    by = "cas"
  ) %>%
  arrange(cas)

future_data$log_auta <- log(future_data$valid_speed_count)

pred_zima <- predict(
  model_zima_lm_hod,
  newdata = future_data,
  interval = "confidence"
)

future_data$pred_pm100  <- exp(pred_zima[, "fit"])
future_data$lower_pm100 <- exp(pred_zima[, "lwr"])
future_data$upper_pm100 <- exp(pred_zima[, "upr"])
future_data$type <- "Predikce"


ggplot(future_data, aes(x = cas)) +
  geom_ribbon(aes(ymin = lower_pm100, ymax = upper_pm100), fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = pred_pm100), color = "blue") +
  labs(
    title = "Predikce koncentrace PM₁₀ v zimním období",
    x = "Čas",
    y = "PM₁₀ [µg/m³]"
  ) +
  theme_minimal()

historical <- data_hodinova %>%
  filter(
    cas >= as.POSIXct("2025-01-02 00:00:00", tz="CET"),
    cas <  as.POSIXct("2025-01-16 00:00:00", tz="CET")
  ) %>%
  mutate(
    pred_pm100 = data_pm100,
    lower_pm100 = NA,
    upper_pm100 = NA,
    type = "Skutečnost"
  ) %>%
  select(cas, pred_pm100, lower_pm100, upper_pm100, type)

combined <- bind_rows(
  historical,
  future_data %>% select(cas, pred_pm100, lower_pm100, upper_pm100, type)
)

ggplot(combined, aes(x = cas)) +
  geom_ribbon(
    data = combined %>% filter(type == "Predikce"),
    aes(ymin = lower_pm100, ymax = upper_pm100),
    fill = "lightblue", alpha = 0.5
  ) +
  geom_line(aes(y = pred_pm100, color = type)) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce" = "blue")) +
  labs(
    title = "Predikce koncentrace PM₁₀ s historickými hodnotami",
    x = "Čas",
    y = "PM₁₀ [µg/m³]",
    color = ""
  ) +
  theme_minimal()
