library(dplyr)
library(lubridate)


data_denni <- data_denni %>%
  mutate(
    den = as.numeric(datum - ymd("2024-01-01")),
    mesic = month(datum),
    topeni = case_when(
      mesic %in% c(1, 2, 3, 11, 12) ~ 1,     # topná sezóna
      mesic %in% c(4:9) ~ 3,                 # netopná sezóna
      mesic == 10 ~ 4                        # přechod
    ),
    ln_pm = log(data_pm100)
  )


boxplot(ln_pm ~ mesic, data = data_denni)
boxplot(ln_pm ~ topeni, data = data_denni)
plot(ln_pm ~ den, data = data_denni)



data_leto <- data_denni %>% filter(mesic > 2 & mesic < 11)

model_leto <- lm(ln_pm ~ den + I(den^2), data = data_leto)
summary(model_leto)

# Zimní konstanta
#data_zima <- data_denni %>% filter(mesic < 4 | mesic > 10)
#model_zima <- lm(ln_pm ~ den + I(den^2), data = data_zima)

zima_prumer <- mean(data_denni$ln_pm[data_denni$mesic < 3 | data_denni$mesic > 10], na.rm = TRUE)


# Kombinace modelu
fit_pm <- rep(NA, nrow(data_denni))
fit_pm[data_denni$mesic < 3 | data_denni$mesic > 10] <- zima_prumer
fit_pm[data_denni$mesic > 2 & data_denni$mesic < 11] <- predict(model_leto, newdata = data_leto)


plot(data_denni$ln_pm, type = "l", main = "Model logaritmu PM")
lines(fit_pm, col = "red")

plot(data_denni$data_pm100, type = "l", main = "Model PM v původní škále")
lines(exp(fit_pm), col = "blue")



#####
# neseděj mi tam ty návaznosti, tak zkouším dělat prasárny
# zimní období = konstanta poslední hodnoty letního období :-)

i_start_leto <- which(data_denni$mesic == 3)[1]       # první den března
i_end_leto   <- tail(which(data_denni$mesic == 10), 1) # poslední den října

# hodnoty z predikce
start_value <- predict(model_leto, newdata = data_denni[i_start_leto, ])
end_value   <- predict(model_leto, newdata = data_denni[i_end_leto, ])

fit_pm <- rep(NA, nrow(data_denni))

fit_pm[data_denni$mesic %in% c(1, 2)] <- start_value

fit_pm[data_denni$mesic > 2 & data_denni$mesic < 11] <-
  predict(model_leto, newdata = data_denni[data_denni$mesic > 2 & data_denni$mesic < 11, ])

fit_pm[data_denni$mesic %in% c(11, 12)] <- end_value

plot(data_denni$datum, data_denni$data_pm100, type = "l", col = "black",
     main = "Model PM v původní škále", xlab = "Datum", ylab = "PM")
lines(data_denni$datum, exp(fit_pm), col = "blue", lwd = 2)


