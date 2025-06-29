plot(data_denni$datum, data_denni$data_pm100, type = "l", main = "Denní průměr PM100")

# ulož si do objektu
pm_day <- daily$pm
dates_day <- daily$date

ggplot(data_denni, aes(x = datum, y = data_pm100)) +
  geom_line() +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(title = "Denní koncentrace PM10", x = "", y = "PM10") +
  theme_minimal()


ggplot(data_denni, aes(x = datum, y = data_pm100)) +
  geom_line(color = "blue") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(title = "Denní koncentrace PM10", x = "", y = "PM10") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2️⃣ ACF pro denní data
acf(data_denni$data_pm100, na.action = na.pass, main = "ACF denních PM10")

# 3️⃣ sezónní boxplot podle měsíce
data_denni$mesic <- factor(format(data_denni$datum, "%m"))

ggplot(data_denni, aes(x = mesic, y = data_pm100)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Rozložení PM10 podle měsíců", x = "měsíc", y = "PM10") +
  theme_minimal()





data_denni$den_v_tydnu <- factor(weekdays(data_denni$datum, abbreviate = TRUE))

ggplot(data_denni, aes(x = den_v_tydnu, y = data_pm100)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Rozložení PM10 podle dne v týdnu", x = "den v týdnu", y = "PM10") +
  theme_minimal()

# topná sezóna
data_denni$mesic <- as.numeric(format(data_denni$datum, "%m"))
data_denni$topna_sezona <- ifelse(data_denni$mesic %in% c(11,12,1,2,3), 1, 0)

ggplot(data_denni, aes(x = factor(topna_sezona), y = data_pm100)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Rozložení PM10 podle topné sezóny", x = "topná sezóna (0=ne, 1=ano)", y = "PM10") +
  theme_minimal()






data_denni$den <- as.numeric(data_denni$datum - as.Date("2024-01-01")) + 1

# označení topné sezóny jemněji
data_denni$topna_sezona2 <- case_when(
  data_denni$mesic %in% 4:9 ~ "leto",
  TRUE ~ "zima"
)

# letní data
leto <- data_denni %>%
  filter(topna_sezona2 == "leto")

# parabola na letní část
model_leto <- lm(log(data_pm100) ~ den + I(den^2), data = leto)
summary(model_leto)

# zimní část - průměr log
mean_zima <- mean(log(data_denni$data_pm100[data_denni$topna_sezona2 == "zima"]), na.rm = TRUE)


# složení fitted hodnot
fit_pm <- rep(NA, nrow(data_denni))

# predikce léto
fit_leto <- predict(model_leto, newdata = leto)
fit_pm[data_denni$topna_sezona2 == "leto"] <- fit_leto

# zimní konstanta
fit_pm[data_denni$topna_sezona2 == "zima"] <- mean_zima

# kontrolní graf
plot(log(data_denni$data_pm100), type = "l", main = "Log PM10 s trendem")
lines(fit_pm, col = 2)

# zpět do úrovně PM
plot(data_denni$data_pm100, type = "l", main = "PM10 s trendem")
lines(exp(fit_pm), col = 2)




###########
data_denni$sezona <- case_when(
  data_denni$mesic %in% 4:9 ~ "leto",
  data_denni$mesic %in% c(11,12,1,2) ~ "zima",
  data_denni$mesic %in% c(3,10) ~ "prechod"
)

# parabola v létě
leto <- filter(data_denni, sezona == "leto")
model_leto <- lm(log(data_pm100) ~ den + I(den^2), data = leto)

# konstanta v zimě
mean_zima <- mean(log(data_denni$data_pm100[data_denni$sezona == "zima"]), na.rm = TRUE)

# připravit vektor predikcí
fit_pm <- rep(NA, nrow(data_denni))

# predikce léto
fit_pm[data_denni$sezona == "leto"] <- predict(model_leto, newdata = leto)

# predikce zima
fit_pm[data_denni$sezona == "zima"] <- mean_zima

# predikce přechodových měsíců (vážený průměr)
prechod <- filter(data_denni, sezona == "prechod")
weight_leto <- ifelse(prechod$mesic == 3, 0.2, 0.8)   # březen 20% léto, říjen 80% léto
fit_leto_p <- predict(model_leto, newdata = prechod)
fit_pm[data_denni$sezona == "prechod"] <- weight_leto * fit_leto_p + (1 - weight_leto) * mean_zima

# kontrolní graf
plot(log(data_denni$data_pm100), type = "l", main = "Log PM10 s trendem (přechod plynulý)")
lines(fit_pm, col = 2)

# zpět do úrovně PM
plot(data_denni$data_pm100, type = "l", main = "PM10 s trendem (přechod plynulý)")
lines(exp(fit_pm), col = 2)



ln.res <- log(data_denni$data_pm100) - fit_pm
plot(ln.res, type = "l", main = "Rezidua po odečtení trendu", ylab = "residua log(PM10)")

# histogram
hist(ln.res, breaks = 30, main = "Histogram reziduí")

# QQ plot
qqnorm(ln.res)
qqline(ln.res)

# ACF reziduí
acf(ln.res, na.action = na.pass, main = "ACF reziduí")


library(forecast)

ln.res.ts <- ts(ln.res, frequency = 7)  # sezónnost 7 dní je hypoteticky možná
arima.model <- auto.arima(ln.res.ts, max.p=3, max.q=3, seasonal=FALSE)

summary(arima.model)

# kontrolní graf
plot(ln.res.ts, main = "Rezidua (log) s ARIMA predikcí")
lines(fitted(arima.model), col=2)






#########
# predikce:
# poslední známé datum
last_date <- max(data_denni$datum)

# 100 dní dopředu
future_dates <- seq.Date(from = last_date + 1, by = "day", length.out = 100)

# připravíme tabulku
future <- data.frame(
  datum = future_dates
)

# doplníme potřebné proměnné
future$mesic <- as.numeric(format(future$datum, "%m"))
future$den <- as.numeric(future$datum - as.Date("2024-01-01")) + 1

future$den <- future$den %% 366
future$den[future$den == 0] <- 1

# sezóna
future$sezona <- case_when(
  future$mesic %in% 4:9 ~ "leto",
  future$mesic %in% c(11,12,1,2) ~ "zima",
  future$mesic %in% c(3,10) ~ "prechod"
)

# váha pro přechod
future$weight_leto <- ifelse(future$mesic == 3, 0.2,
                             ifelse(future$mesic == 10, 0.8, NA))



###########################
# připravíme vektor
future$fit_trend <- NA

# predikce léto
if (any(future$sezona == "leto")) {
  fit_leto_future <- predict(model_leto, newdata = filter(future, sezona == "leto"))
  future$fit_trend[future$sezona == "leto"] <- fit_leto_future
}

# predikce zima
future$fit_trend[future$sezona == "zima"] <- mean_zima

# predikce pro přechod - zatím tam nejsou, ale kdyby tam byly:
if (any(future$sezona == "prechod")) {
  prechod_future <- filter(future, sezona == "prechod")
  fit_leto_p <- predict(model_leto, newdata = prechod_future)
  future$fit_trend[future$sezona == "prechod"] <- 
    prechod_future$weight_leto * fit_leto_p + (1 - prechod_future$weight_leto) * mean_zima
}

# kontrola
head(future)


#########
future_arima <- forecast(arima.model, h = 100)

# predikované rezidua
future$resid_pred <- future_arima$mean



future$pm_pred <- exp(future$fit_trend + future$resid_pred)

# kontrolní graf
plot(future$datum, future$pm_pred, type = "l", 
     xlab = "Datum", ylab = "Predikce PM10", 
     main = "Predikce PM10 na 100 dnů dopředu")



# vezmeme střední predikci reziduí
future$resid_pred <- future_arima$mean
# dolní a horní meze
future$resid_lower <- future_arima$lower[,2]  # 95% spodní
future$resid_upper <- future_arima$upper[,2]  # 95% horní

# složíme výsledné predikce
future$pm_pred <- exp(future$fit_trend + future$resid_pred)
future$pm_lower <- exp(future$fit_trend + future$resid_lower)
future$pm_upper <- exp(future$fit_trend + future$resid_upper)

# spojíme časovou osu
pred_all <- data.frame(
  datum = c(data_denni$datum, future$datum),
  pm_pred = c(data_denni$data_pm100, future$pm_pred),
  pm_lower = c(rep(NA, nrow(data_denni)), future$pm_lower),
  pm_upper = c(rep(NA, nrow(data_denni)), future$pm_upper)
)


library(ggplot2)

ggplot(pred_all, aes(x = datum, y = pm_pred)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = pm_lower, ymax = pm_upper), fill = "lightblue", alpha = 0.4) +
  labs(title = "Predikce PM10 s 95% intervalem", x = "Datum", y = "PM10") +
  theme_minimal()




# konstanta pro rezidua (střední hodnota 0)
future$resid_pred <- 0  

# odhad standardní odchylky reziduí
se_resid <- sd(ln.res, na.rm=TRUE)

# interval 95%
future$pm_lower <- exp(future$fit_trend - 1.96 * se_resid)
future$pm_upper <- exp(future$fit_trend + 1.96 * se_resid)

# bodová predikce
future$pm_pred <- exp(future$fit_trend + future$resid_pred)


pred_all <- data.frame(
  datum = c(data_denni$datum, future$datum),
  pm_pred = c(data_denni$data_pm100, future$pm_pred),
  pm_lower = c(rep(NA, nrow(data_denni)), future$pm_lower),
  pm_upper = c(rep(NA, nrow(data_denni)), future$pm_upper)
)

ggplot(pred_all, aes(x = datum, y = pm_pred)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = pm_lower, ymax = pm_upper), fill = "lightblue", alpha = 0.4) +
  labs(title = "Predikce PM10 na 100 dnů dopředu", x = "Datum", y = "PM10") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  theme_minimal()

data_hodinova <- data


historical_fit <- exp(fit.pm)

# historická část
historical_df <- data.frame(
  datum = data_denni$datum,
  mesic = data_denni$mesic,
  sezona = data_denni$topna_sezona,
  pm_real = data_denni$data_pm100,
  fit_hodnota = historical_fit,
  predikce = NA,
  predikce_lower = NA,
  predikce_upper = NA
)

# predikční část
future_df <- data.frame(
  datum = future$datum,
  mesic = future$mesic,
  sezona = future$sezona,
  pm_real = NA,
  fit_hodnota = exp(future$fit_trend),
  predikce = future$pm_pred,
  predikce_lower = future$pm_lower,
  predikce_upper = future$pm_upper
)

# sloučit
final_df <- rbind(historical_df, future_df)

# zkontroluj
head(final_df)
tail(final_df)










###################################
fit.pm <- rep(NA, nrow(data_denni))

# letní část
leto <- filter(data_denni, sezona == "leto")
fit.pm[data_denni$sezona == "leto"] <- predict(model_leto, newdata = leto)

# zimní část
fit.pm[data_denni$sezona == "zima"] <- mean_zima

# přechod
prechod <- filter(data_denni, sezona == "prechod")
if (nrow(prechod) > 0) {
  weight_leto <- ifelse(prechod$mesic == 3, 0.2, 0.8)
  fit_leto_p <- predict(model_leto, newdata = prechod)
  fit.pm[data_denni$sezona == "prechod"] <- weight_leto * fit_leto_p + (1 - weight_leto) * mean_zima
}


ln.res <- log(data_denni$data_pm100) - fit.pm

# ARIMA na reziduích
ln.res.ts <- ts(ln.res, frequency=7)
arima.model <- auto.arima(ln.res.ts, max.p=3, max.q=3, seasonal=FALSE)

summary(arima.model)
# fitted hodnoty
arima_fit <- fitted(arima.model)

model_log_fit <- fit.pm + arima_fit

# zpět do PM
model_pm_fit <- exp(model_log_fit)


##################
# vygenerovat datumy
last_date <- max(data_denni$datum)
future_dates <- seq.Date(last_date + 1, by="day", length.out=100)

future <- data.frame(
  datum = future_dates,
  mesic = as.numeric(format(future_dates, "%m"))
)

# den v roce omezený na 1–365
future$den <- as.numeric(future$datum - as.Date("2024-01-01")) + 1
future$den <- ifelse(future$den > 365, future$den - 365, future$den)

# sezóna
future$sezona <- case_when(
  future$mesic %in% 4:9 ~ "leto",
  future$mesic %in% c(11,12,1,2) ~ "zima",
  future$mesic %in% c(3,10) ~ "prechod"
)

# predikce trendu
future$fit_trend <- NA

# letní
if(any(future$sezona == "leto")) {
  future$fit_trend[future$sezona=="leto"] <- predict(model_leto, newdata=filter(future, sezona=="leto"))
}

# zimní
future$fit_trend[future$sezona=="zima"] <- mean_zima

# přechod
if(any(future$sezona=="prechod")) {
  prechod_future <- filter(future, sezona=="prechod")
  weight_leto <- ifelse(prechod_future$mesic==3, 0.2, 0.8)
  fit_leto_p <- predict(model_leto, newdata=prechod_future)
  future$fit_trend[future$sezona=="prechod"] <- weight_leto * fit_leto_p + (1 - weight_leto) * mean_zima
}

# predikce ARIMA reziduí
future_arima <- forecast(arima.model, h=100)

# složení
future$log_pred <- future$fit_trend + future_arima$mean
future$log_lower <- future$fit_trend + future_arima$lower[,2]
future$log_upper <- future$fit_trend + future_arima$upper[,2]

# zpět do PM
future$pm_pred <- exp(future$log_pred)
future$pm_lower <- exp(future$log_lower)
future$pm_upper <- exp(future$log_upper)


final_df <- data.frame(
  datum = c(data_denni$datum, future$datum),
  mesic = c(data_denni$mesic, future$mesic),
  sezona = c(data_denni$sezona, future$sezona),
  pm_real = c(data_denni$data_pm100, rep(NA, nrow(future))),
  fit_hodnota = c(model_pm_fit, rep(NA, nrow(future))),
  predikce = c(rep(NA, nrow(data_denni)), future$pm_pred),
  predikce_lower = c(rep(NA, nrow(data_denni)), future$pm_lower),
  predikce_upper = c(rep(NA, nrow(data_denni)), future$pm_upper)
)



library(ggplot2)

ggplot(final_df, aes(x=datum)) +
  geom_line(aes(y=pm_real), color="grey60") +
  geom_line(aes(y=fit_hodnota), color="red") +
  geom_line(aes(y=predikce), color="black") +
  geom_ribbon(aes(ymin=predikce_lower, ymax=predikce_upper), fill="lightblue", alpha=0.4) +
  labs(title="PM10: trend + ARIMA (fit + predikce)", y="PM10", x="Datum") +
  theme_minimal()