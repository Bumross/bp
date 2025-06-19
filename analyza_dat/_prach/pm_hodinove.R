
data <- data %>%
  mutate(
    t = as.numeric(difftime(cas, min(cas), units = "hours")),  # časový index v hodinách
    mesic = month(cas),
    ln_pm = log(data_pm100)
  )


data_leto <- data %>% filter(mesic >= 3 & mesic <= 10)

model_leto <- lm(ln_pm ~ t + I(t^2), data = data_leto)
summary(model_leto)


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
