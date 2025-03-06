library(dplyr)
library(ggplot2)
library(lubridate)
library(tseries)
library(zoo)
library(lmtest)
library(forecast)


### nutno mit nacteny data frame ze souboru "nacteni_dat.R"

# merged_data = všechna data sjednocená, neupravená
# log_date = logaritmovaná data
# log_scaled_data = logaritmovaná a škálovaná data
# scaled_data = škálovaná data

#############################################################################
#############################################################################
#############################################################################

## CCF
##

par(mfrow=c(2,2))  

ccf(log_data$total_pm, merged_data$vehicle_count, lag.max=12, main="CCF: log(PM) vs. Auta")
ccf(log_data$total_pm, merged_data$avg_no2, lag.max=12, main="CCF: log(PM) vs. NO2")
ccf(log_data$total_pm, merged_data$total_windSpeed, lag.max=12, main="CCF: log(PM) vs. Rychlost větru")
ccf(log_data$total_pm, merged_data$total_hum, lag.max=12, main="CCF: log(PM) vs. Vlhkost")


ccf(log_data$total_pm, merged_data$total_temp, lag.max=12, main="CCF: log(PM) vs. Teplota")
ccf(log_data$total_pm, merged_data$total_hum, lag.max=12, main="CCF: log(PM) vs. Vlhkost")
ccf(log_data$total_pm, merged_data$total_pressure, lag.max=12, main="CCF: log(PM) vs. Tlak")
ccf(log_data$total_pm, merged_data$total_windImpact, lag.max=12, main="CCF: log(PM) vs. Vliv větru")

par(mfrow=c(1,1))  

# teplota 1

################################################################################
## nastavení lagů
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

