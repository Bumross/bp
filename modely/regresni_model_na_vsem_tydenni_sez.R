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


#######################################################################################
#########################################################################################
# tydenni sezonnost
## sezonnost 21 (3 denni zapisy x 7)

par(mfrow=c(2,2))  

ccf(log_data$total_pm, merged_data_week$vehicle_count, lag.max=21, main="CCF: log(PM) vs. Auta")
ccf(log_data$total_pm, merged_data_week$avg_no2, lag.max=21, main="CCF: log(PM) vs. NO2")
ccf(log_data$total_pm, merged_data_week$total_windSpeed, lag.max=21, main="CCF: log(PM) vs. Rychlost větru")
ccf(log_data$total_pm, merged_data_week$total_hum, lag.max=21, main="CCF: log(PM) vs. Vlhkost")


ccf(log_data$total_pm, merged_data_week$total_temp, lag.max=21, main="CCF: log(PM) vs. Teplota")
ccf(log_data$total_pm, merged_data_week$total_hum, lag.max=21, main="CCF: log(PM) vs. Vlhkost")
ccf(log_data$total_pm, merged_data_week$total_pressure, lag.max=21, main="CCF: log(PM) vs. Tlak")
ccf(log_data$total_pm, merged_data_week$total_windImpact, lag.max=21, main="CCF: log(PM) vs. Vliv větru")

par(mfrow=c(1,1))  



ts_pm_log_week <- ts(log_data_week$total_pm, frequency = 21)


###############################################################################
##  škálovaná
x_reg_scaled_week <- as.matrix(scaled_data_week[, c("vehicle_count", "avg_no2", 
                                                 "total_windSpeed", "total_pressure", 
                                                 "total_temp", "total_hum", "total_windImpact")])


arimax_car_scaled_week <- auto.arima(ts_pm_log_week, 
                              xreg = x_reg_scaled_week, 
                              d = 1,      
                              D = 1,      
                              seasonal = TRUE,  
                              stepwise = FALSE, 
                              approximation = FALSE, 
                              trace = TRUE) 
# 310, 210

coeftest(arimax_car_scaled_week)
# nema smysl: windspeed, pressure

x_reg_scaled_week <- as.matrix(scaled_data_week[, c("vehicle_count",
                                                    "total_temp", "total_hum", "total_windImpact")])


arimax_car_scaled_week <- Arima(ts_pm_log_week, 
                         order = c(3,1,0),        
                         seasonal = list(order = c(2,1,0), period = 24),  
                         xreg = x_reg_scaled_week,  
                         method = "CSS-ML")  



summary(arimax_car_scaled_week)



par(mfrow=c(2,1))
acf(residuals(arimax_car_scaled_week), main="ACF reziduí ARIMAX modelu (škálovaný, týdenní sezónnost)")
pacf(residuals(arimax_car_scaled_week), main="PACF reziduí ARIMAX modelu (škálovaný, týdenní sezónnost)")
par(mfrow=c(1,1))

Box.test(residuals(arimax_car_scaled_week), type="Ljung-Box")

qqnorm(residuals(arimax_car_scaled_week))
qqline(residuals(arimax_car_scaled_week), col="red")

coeftest(arimax_car_scaled_week)

residuals_arimax_scaled_week <- residuals(arimax_car_scaled_week)
lm_model_scaled_week <- lm(residuals_arimax_scaled_week ~ x_reg_scaled_week)

checkresiduals(arimax_car_scaled_week)





###############################################################################
## normální
x_reg_week <- as.matrix(merged_data_week[, c("vehicle_count", "avg_no2", 
                                                    "total_windSpeed", "total_pressure", 
                                                    "total_temp", "total_hum", "total_windImpact")])


arimax_car_merged_week <- auto.arima(ts_pm_log_week, 
                              xreg = x_reg_week, 
                              d = 1,      
                              D = 1,      
                              seasonal = TRUE,  
                              stepwise = FALSE, 
                              approximation = FALSE, 
                              trace = TRUE) 
# 213, 010


arimax_car_merged_week <- Arima(ts_pm_log_week, 
                                order = c(2,1,3),        
                                seasonal = list(order = c(0,1,0), period = 24),  
                                xreg = x_reg_week,  
                                method = "CSS-ML")  


summary(arimax_car_merged_week)
# moc spatny proste, k nicemu, nebudu delat ani zadne dalsi testy





###############################################################################
## logaritmovaná + škálovaná
x_reg_log_scaled_week <- as.matrix(log_scaled_data_week[, c("vehicle_count", "avg_no2", 
                                             "total_windSpeed", "total_pressure", 
                                             "total_temp", "total_hum", "total_windImpact")])


arimax_car_log_scaled_week <- auto.arima(ts_pm_log_week, 
                                     xreg = x_reg_log_scaled_week, 
                                     d = 1,      
                                     D = 1,      
                                     seasonal = TRUE,  
                                     stepwise = FALSE, 
                                     approximation = FALSE, 
                                     trace = TRUE) 
# 021, 210
arimax_car_log_scaled_week <- Arima(ts_pm_log_week, 
                                order = c(0,2,1),        
                                seasonal = list(order = c(2,1,0), period = 24),  
                                xreg = x_reg_log_scaled_week,  
                                method = "CSS-ML")  


summary(arimax_car_log_scaled_week)

# pryc: vlhkost, no2, impact
x_reg_log_scaled_week <- as.matrix(log_scaled_data_week[, c("vehicle_count", 
                                                            "total_windSpeed", "total_pressure", 
                                                            "total_temp")])

arimax_car_log_scaled_week <- Arima(ts_pm_log_week, 
                                    order = c(0,2,1),        
                                    seasonal = list(order = c(2,1,0), period = 24),  
                                    xreg = x_reg_log_scaled_week,  
                                    method = "CSS-ML")  


summary(arimax_car_log_scaled_week)
# teplota je zaporna wtf?


par(mfrow=c(2,1))
acf(residuals(arimax_car_log_scaled_week), main="ACF reziduí ARIMAX modelu (logaritmovaná, týdenní sezónnost)")
pacf(residuals(arimax_car_log_scaled_week), main="PACF reziduí ARIMAX modelu (logaritmovaná, týdenní sezónnost)")
par(mfrow=c(1,1))

Box.test(residuals(arimax_car_log_scaled_week), type="Ljung-Box")

qqnorm(residuals(arimax_car_log_scaled_week))
qqline(residuals(arimax_car_log_scaled_week), col="red")

coeftest(arimax_car_log_scaled_week)

residuals_arimax_log_scaled_week <- residuals(arimax_car_log_scaled_week)
lm_model_log_scaled_week <- lm(residuals_arimax_log_scaled_week ~ x_reg_log_scaled_week)

checkresiduals(arimax_car_log_scaled_week)







###############################################################################
## logaritmovaná
x_reg_log_week <- as.matrix(log_data_week[, c("vehicle_count", "avg_no2", 
                                              "total_windSpeed", "total_pressure", 
                                              "total_temp", "total_hum", "total_windImpact")])


arimax_car_log_week <- auto.arima(ts_pm_log_week, 
                                  xreg = x_reg_log_week, 
                                  d = 1,      
                                  D = 1,      
                                  seasonal = TRUE,  
                                  stepwise = FALSE, 
                                  approximation = FALSE, 
                                  trace = TRUE) 


summary(arimax_car_log_week)

par(mfrow=c(2,1))
acf(residuals(arimax_car_log_week), main="ACF reziduí ARIMAX modelu (logaritmovaná, týdenní sezónnost)")
pacf(residuals(arimax_car_log_week), main="PACF reziduí ARIMAX modelu (logaritmovaná, týdenní sezónnost)")
par(mfrow=c(1,1))

Box.test(residuals(arimax_car_log_week), type="Ljung-Box")

qqnorm(residuals(arimax_car_log_week))
qqline(residuals(arimax_car_log_week), col="red")

coeftest(arimax_car_log_week)

residuals_arimax_log_week <- residuals(arimax_car_log_week)
lm_model_log_week <- lm(residuals_arimax_log_week ~ x_reg_log_week)

checkresiduals(arimax_car_log_week)


###################################

# nejlepsi model - skalovane hodnoty