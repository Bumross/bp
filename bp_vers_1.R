library(dplyr)
library(ggplot2)
library(lubridate)
library(tseries)
library(zoo)
library(lmtest)
library(forecast)

# lmtests prikaz, ktery mi rovnou da p hodnoty k modelu

################### Načtení dat
# vitr
path <- "C:\\Users\\bruli\\OneDrive\\UJEP_BP\\data\\meteo_2024_11_01.csv"
wind <- read.csv(path)

wind$date <- as.POSIXct(wind$date, format="%Y-%m-%d %H:%M:%OS")

hourly_summary <- wind %>%
  mutate(hour = floor_date(date, "hour")) %>%
  group_by(sensor_id, hour) %>%
  summarise(
    avg_temp = mean(data_temp1, na.rm = TRUE),
    avg_hum = mean(data_hum1, na.rm = TRUE),
    avg_pressure = mean(data_pressure, na.rm = TRUE),
    avg_windSpeed = mean(data_windSpeed, na.rm = TRUE),
    avg_windImpact = mean(data_windImpact, na.rm = TRUE),
    avg_volume = mean(data_volumeMm, na.rm = TRUE),
    .groups = "drop"
  )


final_summary <- hourly_summary %>%
  group_by(hour) %>%
  summarise(
    total_temp = mean(avg_temp, na.rm = TRUE),
    total_hum = mean(avg_hum, na.rm = TRUE),
    total_pressure = mean(avg_pressure, na.rm = TRUE),
    total_windSpeed = mean(avg_windSpeed, na.rm = TRUE),
    total_windImpact = mean(avg_windImpact, na.rm = TRUE),
    total_volume = mean(avg_volume, na.rm = TRUE),
    .groups = "drop"
  )


# auta
path <- "C:\\Users\\bruli\\OneDrive\\UJEP_BP\\data\\ddb_data"

csv_files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

# načtení dat ddb do jednoho data frame:
combined_data <- csv_files %>%
  lapply(read.csv) %>%
  bind_rows()


senzor_density_60 <- combined_data %>%
  mutate(
    X_time = ymd_hms(X_time),
    time_interval = floor_date(X_time, "60 minutes")  
  )

senzor_density_60 <- senzor_density_60 %>%
  group_by(sensor, time_interval) %>%
  summarise(vehicle_count = n(), .groups = "drop")

sum_60 <- senzor_density_60 %>%
  group_by(time_interval) %>%
  summarise(vehicle_count = sum(vehicle_count), .groups = "drop")

sum_60$time_interval <- as.POSIXct(sum_60$time_interval, format="%Y-%m-%d %H:%M:%OS")
sum_60 <- sum_60 %>%
  rename(hour = time_interval)


# ovzdusi
path <- "C:\\Users\\bruli\\OneDrive\\UJEP_BP\\data\\pm_2024_11_01.csv"
polution <- read.csv(path)

polution$date <- as.POSIXct(polution$date, format="%Y-%m-%d %H:%M:%OS")

# pro kazdy senzor zvlast
polution_summary <- polution %>%
  mutate(hour = floor_date(date, "hour")) %>% 
  group_by(sensor_id, hour) %>%
  summarise(
    sum_pm10 = sum(data_pm10, na.rm = TRUE),
    sum_pm25 = sum(data_pm25, na.rm = TRUE),
    sum_pm40 = sum(data_pm40, na.rm = TRUE),
    sum_pm100 = sum(data_pm100, na.rm = TRUE),
    avg_no2 = mean(data_no2, na.rm = TRUE),
    .groups = "drop"
  )

# pro vsechny senzory dohromady
hourly_summary <- polution_summary %>%
  group_by(hour) %>%
  summarise(
    total_pm10 = sum(sum_pm10, na.rm = TRUE),
    total_pm25 = sum(sum_pm25, na.rm = TRUE),
    total_pm40 = sum(sum_pm40, na.rm = TRUE),
    total_pm100 = sum(sum_pm100, na.rm = TRUE),
    avg_no2 = mean(avg_no2, na.rm = TRUE),  # Průměr pro NO2
    .groups = "drop"
  )

hourly_summary_combined <- hourly_summary %>%
  mutate(total_pm = total_pm10 + total_pm25 + total_pm40 + total_pm100) %>%
  select(hour, total_pm, avg_no2)


### Sjednoceni vsech dat do jednoho DF
merged_data <- final_summary %>%
  inner_join(hourly_summary_combined, by = "hour") %>%
  inner_join(sum_60, by = "hour")










##########################################################################
par(mfrow=c(2,1))
acf(merged_data$total_pm, na.action=na.pass, main="ACF PM koncentrace")
pacf(merged_data$total_pm, na.action=na.pass, main="PACF PM koncentrace")
par(mfrow=c(1,1))

diff_pm <- diff(merged_data$total_pm, differences = 1)

par(mfrow=c(2,1))
acf(diff_pm)
pacf(diff_pm)
par(mfrow=c(1,1))

# diference = 1, musím s tím počítat dále


# zadat frekvenci - vybrat tyden nebo den <- ts. - navolit frekvenci
auto_model <- auto.arima(merged_data$total_pm, d=1)  
summary(auto_model)

# ARIMA MODEL 5 1 1

par(mfrow=c(2,1))
acf(residuals(auto_model), main="ACF reziduí ARIMA modelu")
pacf(residuals(auto_model), main="PACF reziduí ARIMA modelu")
par(mfrow=c(1,1))

Box.test(residuals(auto_model), type="Ljung-Box")
# nekorelovana reziua

qqnorm(residuals(auto_model))
qqline(residuals(auto_model), col="red")
# má FAT TAILS - existují extrémní výkyvy


########################
# matice regresorů
x_reg <- as.matrix(merged_data[, c("total_temp", "total_hum", "total_pressure", 
                                   "total_windSpeed", "total_windImpact", 
                                   "total_volume", "vehicle_count", "avg_no2")])

arimax_model <- auto.arima(merged_data$total_pm, xreg=x_reg, d=1)
summary(arimax_model)

arimax_model$order

BIC(auto_model)
BIC(arimax_model)
# hodnota BIC klesla - model s regresory je lepsiiii


par(mfrow=c(2,1))
acf(residuals(arimax_model), main="ACF reziduí ARIMAX modelu")
pacf(residuals(arimax_model), main="PACF reziduí ARIMAX modelu")
par(mfrow=c(1,1))

Box.test(residuals(arimax_model), type="Ljung-Box")
#nekorelovana rezidua

qqnorm(residuals(arimax_model))
qqline(residuals(arimax_model), col="red")
# porad tezke chvosty, ale asi ok?




# vynechame zbytecne regresory (pressure, windimpact, total_volume)
# Vytvoření matice regresorů bez nevýznamných proměnných
x_reg_opt <- as.matrix(merged_data[, c("total_temp", "total_hum", "total_windSpeed", "vehicle_count", "avg_no2")])

arimax_model_opt <- auto.arima(merged_data$total_pm, xreg=x_reg_opt, d=1)  
summary(arimax_model_opt)

#AR2 má potíže - s.e > než ona samotná - nespolehlivý koeficient
# no_2 má potíže - s. e. blízko dvojnásobku - vynechávám
x_reg_opt <- as.matrix(merged_data[, c("total_temp", "total_hum", "total_windSpeed", "vehicle_count")])

arimax_model_fixed <- auto.arima(merged_data$total_pm, xreg=x_reg_opt, d=1, max.p=4)  
summary(arimax_model_fixed)

#########
# škálování hodnot
merged_data_scaled <- merged_data
merged_data_scaled[, c("total_temp", "total_hum", "total_windSpeed", "vehicle_count")] <- scale(merged_data[, c("total_temp", "total_hum", "total_windSpeed", "vehicle_count")])

x_reg_scaled <- as.matrix(merged_data_scaled[, c("total_temp", "total_hum", "total_windSpeed", "vehicle_count")])
arimax_model_scaled <- auto.arima(merged_data$total_pm, xreg=x_reg_scaled, d=1)
summary(arimax_model_scaled)

# ar2 a ar4 zbytečné
arimax_model_restricted <- auto.arima(merged_data$total_pm, xreg=x_reg_scaled, d=1, max.p=3, max.)  
summary(arimax_model_restricted)

##########################################################################
##########################################################################
##########################################################################
#                 nejlepsi model
# vypnutí stepwise
arimax_model_opt <- auto.arima(merged_data$total_pm, xreg=x_reg_scaled, d=1, stepwise=FALSE, approximation=FALSE)
summary(arimax_model_opt)


### nejoptimalnejsi model ARIMAX_MODEL_OPT
# nizce BIC
# rozumne smodch u vsech parametru
# vehicle count je vyznamna :-))

par(mfrow=c(2,1))
acf(residuals(arimax_model_opt), main="ACF reziduí finálního ARIMAX modelu")
pacf(residuals(arimax_model_opt), main="PACF reziduí finálního ARIMAX modelu")
par(mfrow=c(1,1))

Box.test(residuals(arimax_model_opt), type="Ljung-Box")

qqnorm(residuals(arimax_model_opt))
qqline(residuals(arimax_model_opt), col="red")


### vraceni skalovani zpet
mean_pm <- mean(merged_data$total_pm, na.rm=TRUE)
sd_pm <- sd(merged_data$total_pm, na.rm=TRUE)

fitted_pm_original <- fitted(arimax_model_opt) * sd_pm + mean_pm
fitted_pm_original <- data.frame(
  hour = merged_data$hour,  # Správná časová osa
  fitted_value = fitted(arimax_model_opt)  # Odpovídající fitted hodnoty
)

plot(merged_data$hour, merged_data$total_pm, type="l", col="black", lwd=2, main="Reálné vs. Predikované hodnoty PM", xlab="Čas", ylab="PM koncentrace")
lines(fitted_pm_original$hour, fitted_pm_original$fitted_value, col="red", lwd=2)
legend("topright", legend=c("Skutečná data", "ARIMAX predikce"), col=c("black", "red"), lty=1, lwd=2)


#######################################################################################
#########################################################################################

# NOVY DATA

## export 24 08 + 24 11 + 25 02
### data 0
path <- "C:\\Users\\bruli\\OneDrive\\UJEP_BP\\data\\data_nove\\export_2024_08\\ddb_data0"

csv_files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

# načtení dat ddb do jednoho data frame:
combined_data_new <- csv_files %>%
  lapply(read.csv) %>%
  bind_rows()


combined_data_new$hour <- floor_date(as.POSIXct(combined_data_new$X_time, tz = "UTC"), unit = "hour")

min_time <- min(combined_data_new$hour, na.rm = TRUE)
max_time <- max(combined_data_new$hour, na.rm = TRUE)

all_hours <- data.frame(hour = seq.POSIXt(min_time, max_time, by = "hour"))

hourly_counts <- combined_data_new %>%
  count(hour)

complete_hourly_data <- full_join(all_hours, hourly_counts, by = "hour") %>%
  mutate(n = ifelse(is.na(n), 0, n))

ggplot(complete_hourly_data, aes(x = hour, y = n)) +
  geom_line() +
  geom_point() +
  labs(x = "Čas (hodiny)", y = "Počet záznamů", title = "Počet záznamů za hodinu (včetně chybějících hodin)") +
  theme_minimal()



## meteo
path <- "C:\\Users\\bruli\\OneDrive\\UJEP_BP\\data\\data_nove\\export_2024_08\\pm_2024.csv"
wind_new <- read.csv(path)

wind_new$hour <- floor_date(as.POSIXct(wind_new$date, tz = "UTC"), unit = "hour")

min_time <- min(wind_new$hour, na.rm = TRUE)
max_time <- max(wind_new$hour, na.rm = TRUE)

all_hours <- data.frame(hour = seq.POSIXt(min_time, max_time, by = "hour"))

hourly_counts <- wind_new %>%
  count(hour)

complete_hourly_data <- full_join(all_hours, hourly_counts, by = "hour") %>%
  mutate(n = ifelse(is.na(n), 0, n))

ggplot(complete_hourly_data, aes(x = hour, y = n)) +
  geom_line() +
  geom_point() +
  labs(x = "Čas (hodiny)", y = "Počet záznamů", title = "Počet záznamů za hodinu (včetně chybějících hodin)") +
  theme_minimal()


## pm
path <- "C:\\Users\\bruli\\OneDrive\\UJEP_BP\\data\\data_nove\\export_2024_08\\pm_2024.csv"
pm_new <- read.csv(path)

pm_new$day <- as.Date(pm_new$date)

min_date <- min(pm_new$day, na.rm = TRUE)
max_date <- max(pm_new$day, na.rm = TRUE)

all_days <- data.frame(day = seq.Date(min_date, max_date, by = "day"))


daily_counts <- pm_new %>%
  count(day)

complete_data <- full_join(all_days, daily_counts, by = "day") %>%
  mutate(n = ifelse(is.na(n), 0, n))

ggplot(complete_data, aes(x = day, y = n)) +
  geom_line() +
  geom_point() +
  labs(x = "Datum", y = "Počet záznamů", title = "Počet záznamů za den (včetně chybějících dnů)") +
  theme_minimal()