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



##########################################################################
par(mfrow=c(2,1))
acf(merged_data$total_pm, lag.max=24, main="ACF PM koncentrace (24 hodin)")
pacf(merged_data$total_pm, lag.max=24, main="PACF PM koncentrace (24 hodin)")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
acf(merged_data$total_pm, lag.max=168, main="ACF PM koncentrace (168 hodin)")
pacf(merged_data$total_pm, lag.max=168, main="PACF PM koncentrace (168 hodin)")
par(mfrow=c(1,1))


# diference = 1, musím s tím počítat dále
# pokračuji zatím pouze s denní sezónností
# acf naznačuje sezónní nestacionaritu - volím d=1

ts_pm <- ts(merged_data$total_pm, frequency = 24, start = c(year(min(merged_data$hour)), yday(min(merged_data$hour))))
plot(ts_pm, main="Časová řada PM koncentrace", ylab="PM")


sarima_model <- auto.arima(ts_pm, d=1, D=1, 
                           seasonal=TRUE, stepwise=FALSE, approximation=FALSE, 
                           trace=TRUE, lambda=NULL)

summary(sarima_model)

# sarima 1 1 1, 2 1 0
par(mfrow=c(2,1))
acf(residuals(sarima_model), main="ACF reziduí SARIMA modelu")
pacf(residuals(sarima_model), main="PACF reziduí SARIMA modelu")
par(mfrow=c(1,1))

Box.test(residuals(sarima_model), type="Ljung-Box")
qqnorm(residuals(sarima_model))
qqline(residuals(sarima_model), col="red")


#######################
# logaritmuju - rezidua nejsou normálního rozložení

merged_data$log_total_pm <- log(merged_data$total_pm)
ts_pm_log <- ts(merged_data$log_total_pm, frequency = 24, start = c(year(min(merged_data$hour)), yday(min(merged_data$hour))))

sarima_model_log <- auto.arima(ts_pm_log, d=1, D=1, 
                               seasonal=TRUE, stepwise=FALSE, approximation=FALSE, 
                               trace=TRUE, lambda=NULL)

summary(sarima_model_log)
# sarima 1 1 1, 2 1 0

par(mfrow=c(2,1))
acf(residuals(sarima_model_log), main="ACF reziduí SARIMA modelu (log)")
pacf(residuals(sarima_model_log), main="PACF reziduí SARIMA modelu (log)")
par(mfrow=c(1,1))

Box.test(residuals(sarima_model_log), type="Ljung-Box")
qqnorm(residuals(sarima_model_log))
qqline(residuals(sarima_model_log), col="red")


# lepsi presnost, logaritmizace vyrazne zlepsila model


par(mfrow=c(2,3))  
hist(merged_data$vehicle_count, main="Počet aut", xlab="Hodnoty", breaks=30)
hist(merged_data$avg_no2, main="NO2", xlab="Hodnoty", breaks=30)
hist(merged_data$total_temp, main="Teplota", xlab="Hodnoty", breaks=30)
hist(merged_data$total_hum, main="Vlhkost", xlab="Hodnoty", breaks=30)
hist(merged_data$total_windSpeed, main="Rychlost větru", xlab="Hodnoty", breaks=30)
hist(merged_data$total_pressure, main="Tlak", xlab="Hodnoty", breaks=30)
hist(merged_data$total_windImpact, main="Vliv větru", xlab="Hodnoty", breaks=30)


par(mfrow=c(1,1))


## zlogaritmovani dalsich???

## zlogaritmizuju jen ty, kde je rozlozeni fakt nenormalni 

## ja to urcite poseru, sleduj

merged_data$log_vehicle_count <- log(merged_data$vehicle_count + 1)
merged_data$log_avg_no2 <- log(merged_data$avg_no2 + 1)
merged_data$log_total_windSpeed <- log(merged_data$total_windSpeed + 1)
merged_data$log_total_windImpact <- log(merged_data$total_windImpact + 1)
merged_data$log_total_hum <- log(merged_data$total_hum + 1)
merged_data$log_total_temp <- log(merged_data$total_temp + 1)
merged_data$log_total_pressure <- log(merged_data$total_pressure + 1)


#############################x
## CCF
##

par(mfrow=c(2,2))  

ccf(merged_data$log_total_pm, merged_data$log_vehicle_count, lag.max=12, main="CCF: log(PM) vs. log(Auta)")
ccf(merged_data$log_total_pm, merged_data$log_avg_no2, lag.max=12, main="CCF: log(PM) vs. log(NO2)")
ccf(merged_data$log_total_pm, merged_data$log_total_windSpeed, lag.max=12, main="CCF: log(PM) vs. log(Rychlost větru)")
ccf(merged_data$log_total_pm, merged_data$log_total_hum, lag.max=12, main="CCF: log(PM) vs. log(Vlhkost)")


ccf(merged_data$log_total_pm, merged_data$total_temp, lag.max=12, main="CCF: log(PM) vs. Teplota")
ccf(merged_data$log_total_pm, merged_data$total_hum, lag.max=12, main="CCF: log(PM) vs. Vlhkost")
ccf(merged_data$log_total_pm, merged_data$total_pressure, lag.max=12, main="CCF: log(PM) vs. Tlak")
ccf(merged_data$log_total_pm, merged_data$total_windSpeed, lag.max=12, main="CCF: log(PM) vs. Rychlost větru")

par(mfrow=c(1,1))  



######################
## nastavení logů
merged_data$log_vehicle_count_lag5 <- dplyr::lag(merged_data$log_vehicle_count, 1)
merged_data$log_avg_no2_lag6 <- dplyr::lag(merged_data$log_avg_no2, 0)
merged_data$log_total_windSpeed_lag1 <- dplyr::lag(merged_data$log_total_windSpeed, 1)
merged_data$total_pressure_lag2 <- dplyr::lag(merged_data$total_pressure, 1)
merged_data$total_temp_lag2 <- dplyr::lag(merged_data$total_temp, 1)
merged_data$log_total_hum_lag5 <- dplyr::lag(merged_data$log_total_hum, 0)


merged_data_clean <- merged_data %>% na.omit()

ts_pm_log_clean <- ts(merged_data_clean$log_total_pm, frequency = , 
                      start = c(year(min(merged_data_clean$hour)), 
                                yday(min(merged_data_clean$hour))))


#########################
####################x###xx
# Vytvoření matice regresorů
x_reg <- as.matrix(merged_data_clean[, c("log_vehicle_count_lag5", 
                                   "log_avg_no2_lag6", 
                                   "log_total_windSpeed_lag1", 
                                   "total_pressure_lag2", 
                                   "total_temp_lag2",
                                   "log_total_hum_lag5")])


arimax_seasonal <- auto.arima(ts_pm_log_clean, 
                              xreg = x_reg, 
                              d = 1,      
                              D = 1,      
                              seasonal = TRUE,  
                              stepwise = FALSE, 
                              approximation = FALSE, 
                              trace = TRUE)  

summary(arimax_seasonal)


par(mfrow=c(2,1))
acf(residuals(arimax_seasonal), main="ACF reziduí Sezónního ARIMAX modelu")
pacf(residuals(arimax_seasonal), main="PACF reziduí Sezónního ARIMAX modelu")
par(mfrow=c(1,1))

Box.test(residuals(arimax_seasonal), type="Ljung-Box")
qqnorm(residuals(arimax_seasonal))
qqline(residuals(arimax_seasonal), col="red")

#################################################################
##################################################################
## predem ten model je zvlustni, zkusim to udelat bez tech lagu
x_reg_nolag <- as.matrix(merged_data[, c("log_vehicle_count", 
                                               "log_avg_no2", 
                                               "log_total_windSpeed",
                                               "log_total_pressure", 
                                               "log_total_temp",
                                               "log_total_hum")])

# Spuštění ARIMAX modelu bez lagů
arimax_nolag <- auto.arima(ts_pm_log, 
                           xreg = x_reg_nolag, 
                           d = 1,      
                           D = 1,      
                           seasonal = TRUE,  
                           stepwise = FALSE, 
                           approximation = FALSE, 
                           trace = TRUE)

summary(arimax_nolag)

coeftest(arimax_nolag)

residuals_arimax <- residuals(arimax_nolag)
lm_model <- lm(residuals_arimax ~ x_reg_nolag)
bgtest(lm_model)
bptest(lm_model) ## to je tim, ze jsem zapomnel skalovat :-)

#####################################################################
# skalovani dat

# zatim nejlepsi model

merged_data_scaled <- merged_data %>%
  mutate(across(c(vehicle_count, avg_no2, total_windSpeed, 
                  total_pressure, total_temp, total_hum), scale))

x_reg_scaled <- as.matrix(merged_data_scaled[, c("vehicle_count", "avg_no2", 
                                                 "total_windSpeed", "total_pressure", 
                                                 "total_temp", "total_hum")])


arimax_scaled <- auto.arima(ts(merged_data_scaled$log_total_pm, frequency = 24), 
                            xreg = x_reg_scaled, 
                            d = 1,      
                            D = 1,      
                            seasonal = TRUE,  
                            stepwise = FALSE, 
                            approximation = FALSE, 
                            trace = TRUE)

summary(arimax_scaled)

par(mfrow=c(2,1))
acf(residuals(arimax_scaled), main="ACF reziduí ARIMAX modelu (škálovaný)")
pacf(residuals(arimax_scaled), main="PACF reziduí ARIMAX modelu (škálovaný)")
par(mfrow=c(1,1))

Box.test(residuals(arimax_scaled), type="Ljung-Box")

qqnorm(residuals(arimax_scaled))
qqline(residuals(arimax_scaled), col="red")

coeftest(arimax_scaled)

residuals_arimax_scaled <- residuals(arimax_scaled)
lm_model_scaled <- lm(residuals_arimax_scaled ~ x_reg_scaled)
bgtest(lm_model_scaled)
bptest(lm_model_scaled) 


#########################################


arimax_pm <- Arima(ts(merged_data_scaled$log_total_pm, frequency = 24), 
                   order = c(1,1,1),        
                   seasonal = list(order = c(2,1,0), period = 24),  
                   xreg = x_reg_scaled,  
                   method = "CSS-ML")            

summary(arimax_pm)

par(mfrow=c(2,1))
acf(residuals(arimax_pm), main="ACF reziduí ARIMAX modelu (PM koncentrace)")
pacf(residuals(arimax_pm), main="PACF reziduí ARIMAX modelu (PM koncentrace)")
par(mfrow=c(1,1))

Box.test(residuals(arimax_pm), type="Ljung-Box")

qqnorm(residuals(arimax_pm))
qqline(residuals(arimax_pm), col="red")

coeftest(arimax_pm)

residuals_arimax_pm <- residuals(arimax_pm)
lm_model_pm <- lm(residuals_arimax_pm ~ x_reg_log_scaled)

summary(lm_model_pm)


bgtest(lm_model_pm)
bptest(lm_model_pm)

###############################
# bez toho tlaku

x_reg_scaled <- as.matrix(merged_data_scaled[, c("vehicle_count", "avg_no2", 
                                                 "total_windSpeed", "total_temp", 
                                                 "total_hum")])

ts_pm_log_scaled <- ts(merged_data_scaled$log_total_pm, frequency = 24)

arimax_pm <- Arima(ts_pm_log_scaled, 
                   order = c(1,1,1),        
                   seasonal = list(order = c(2,1,0), period = 24),  
                   xreg = x_reg_scaled,  
                   method = "CSS-ML") 5           

summary(arimax_pm)

par(mfrow=c(2,1))
acf(residuals(arimax_pm), main="ACF reziduí ARIMAX modelu (PM koncentrace)")
pacf(residuals(arimax_pm), main="PACF reziduí ARIMAX modelu (PM koncentrace)")
par(mfrow=c(1,1))

Box.test(residuals(arimax_pm), type="Ljung-Box")

qqnorm(residuals(arimax_pm))
qqline(residuals(arimax_pm), col="red")

coeftest(arimax_pm)

residuals_arimax_pm <- residuals(arimax_pm)
lm_model_pm <- lm(residuals_arimax_pm ~ x_reg_scaled)

summary(lm_model_pm)

bgtest(lm_model_pm)
bptest(lm_model_pm)



checkresiduals(arimax_pm)



###############################
# vyzkouseni AR=2
# Počet Inf v regresorech

arimax_pm_adj <- Arima(ts_pm_log_scaled, 
                       order = c(2,1,1),        
                       seasonal = list(order = c(2,1,0), period = 24),  
                       xreg = x_reg_scaled,  
                       method = "CSS-ML")            

summary(arimax_pm_adj)




########################################################################
# skalovane + logaritmovane

merged_data_log_scaled <- merged_data %>%
  mutate(across(c(vehicle_count, avg_no2, total_windSpeed, 
                  total_pressure, total_temp, total_hum), ~ log(. + 1))) %>%
  mutate(across(c(vehicle_count, avg_no2, total_windSpeed, 
                  total_pressure, total_temp, total_hum), scale))

x_reg_log_scaled <- as.matrix(merged_data_log_scaled[, c("vehicle_count", "avg_no2", 
                                                         "total_windSpeed", "total_pressure", 
                                                         "total_temp", "total_hum")])


ts_pm_log_scaled <- ts(merged_data_log_scaled$log_total_pm, frequency = 24)

arimax_log_scaled <- auto.arima(ts_pm_log_scaled, 
                                xreg = x_reg_log_scaled, 
                                d = 1,      
                                D = 1,      
                                seasonal = TRUE,  
                                stepwise = FALSE, 
                                approximation = FALSE, 
                                trace = TRUE)

summary(arimax_log_scaled)

par(mfrow=c(2,1))
acf(residuals(arimax_log_scaled), main="ACF reziduí ARIMAX modelu (log+škálovaný)")
pacf(residuals(arimax_log_scaled), main="PACF reziduí ARIMAX modelu (log+škálovaný)")
par(mfrow=c(1,1))

Box.test(residuals(arimax_log_scaled), type="Ljung-Box")

qqnorm(residuals(arimax_log_scaled))
qqline(residuals(arimax_log_scaled), col="red")

coeftest(arimax_log_scaled)

residuals_arimax_log_scaled <- residuals(arimax_log_scaled)
lm_model_log_scaled <- lm(residuals_arimax_log_scaled ~ x_reg_log_scaled)


bgtest(lm_model_log_scaled)
bptest(lm_model_log_scaled)

########################################################################
########################################################################
########################################################################
# model pouze na pocasi


x_reg_weather <- as.matrix(merged_data_scaled[, c("avg_no2", 
                                                  "total_windSpeed", 
                                                  "total_temp", 
                                                  "total_hum")])  # Bez vehicle_count


arimax_weather <- auto.arima(ts_pm_log_scaled, 
                                  xreg = x_reg_weather, 
                                  d = 1,      
                                  D = 1,      
                                  seasonal = TRUE,  
                                  stepwise = FALSE, 
                                  approximation = FALSE, 
                                  trace = TRUE) 
summary(arimax_weather)

par(mfrow=c(2,1))
acf(residuals(arimax_weather), main="ACF reziduí ARIMAX modelu (Počasí)")
pacf(residuals(arimax_weather), main="PACF reziduí ARIMAX modelu (Počasí)")
par(mfrow=c(1,1))

Box.test(residuals(arimax_weather), type="Ljung-Box")

qqnorm(residuals(arimax_weather))
qqline(residuals(arimax_weather), col="red")

coeftest(arimax_weather)





##########################################################################
##########################################################################
##########################################################################
# model pocasi + auta


#######################################################################################
#########################################################################################
## sezonnost 168


ts_pm_log_scaled <- ts(merged_data_scaled$log_total_pm, frequency = 168)

x_reg_scaled <- as.matrix(merged_data_scaled[, c("vehicle_count", "avg_no2", 
                                                 "total_windSpeed", "total_pressure", 
                                                 "total_temp", "total_hum")])


arimax_manual <- Arima(ts_pm_log_scaled, 
                       order = c(1,1,1),         
                       seasonal = list(order = c(2,1,0), period = 168),  
                       xreg = x_reg_scaled,     
                       method = "ML") 

summary(arimax_scaled_168)

par(mfrow=c(2,1))
acf(residuals(arimax_scaled_168), main="ACF reziduí ARIMAX modelu (škálovaný, týdenní sezónnost)")
pacf(residuals(arimax_scaled_168), main="PACF reziduí ARIMAX modelu (škálovaný, týdenní sezónnost)")
par(mfrow=c(1,1))

Box.test(residuals(arimax_scaled_168), type="Ljung-Box")

qqnorm(residuals(arimax_scaled_168))
qqline(residuals(arimax_scaled_168), col="red")

coeftest(arimax_scaled_168)

residuals_arimax_scaled_168 <- residuals(arimax_scaled_168)
lm_model_scaled_168 <- lm(residuals_arimax_scaled_168 ~ x_reg_scaled)

bgtest(lm_model_scaled_168)
bptest(lm_model_scaled_168) # nekoukat na cisla, okometrie >>>>


#######################################################################################
#########################################################################################
## model pouze na autech
x_reg_car <- as.matrix(merged_data[, c("vehicle_count")])

ts_pm_log <- ts(merged_data$log_total_pm, frequency = 24)

arimax_car <- auto.arima(ts_pm_log_scaled, 
                             xreg = x_reg_car, 
                             d = 1,      
                             D = 1,      
                             seasonal = TRUE,  
                             stepwise = FALSE, 
                             approximation = FALSE, 
                             trace = TRUE) 
summary(arimax_car)

par(mfrow=c(2,1))
acf(residuals(arimax_car), main="ACF reziduí ARIMAX modelu (Počasí)")
pacf(residuals(arimax_car), main="PACF reziduí ARIMAX modelu (Počasí)")
par(mfrow=c(1,1))

Box.test(residuals(arimax_car), type="Ljung-Box")

qqnorm(residuals(arimax_car))
qqline(residuals(arimax_car), col="red")

coeftest(arimax_car)

checkresiduals(arimax_car)
