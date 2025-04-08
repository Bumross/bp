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

# teplota 1, vlhkost 0, tlak 1, vliv 0-1, no2 0-1, rychlost větru 0-1, auta 0-1






################################################################################
# použití lagů 

######################x
## pro normální data

merged_data$vehicle_count_0 <- dplyr::lag(merged_data$vehicle_count, 0)
merged_data$avg_no2_0 <- dplyr::lag(merged_data$avg_no2, 0)
merged_data$total_windSpeed_0 <- dplyr::lag(merged_data$total_windSpeed, 0)
merged_data$total_pressure_1 <- dplyr::lag(merged_data$total_pressure, 1)
merged_data$total_temp_1 <- dplyr::lag(merged_data$total_temp, 1)
merged_data$total_hum_0 <- dplyr::lag(merged_data$total_hum, 0)
merged_data$total_windImpact <- dplyr::lag(merged_data$total_windImpact, 0)



merged_data_lags <- merged_data %>% na.omit()

ts_pm_merged_lag <- ts(log_data$total_pm, frequency = 24, 
                      start = c(year(min(merged_data_lags_1$hour)), 
                                yday(min(merged_data_lags_1$hour))))






######################x
## pro škálovaná
scaled_data$vehicle_count_0 <-dplyr::lag(scaled_data$vehicle_count, 0)
scaled_data$avg_no2_0 <-dplyr::lag(scaled_data$avg_no2, 0)
scaled_data$total_windSpeed_0 <-dplyr::lag(scaled_data$total_windSpeed, 0)
scaled_data$total_pressure_1 <-dplyr::lag(scaled_data$total_pressure, 1)
scaled_data$total_temp_1 <-dplyr::lag(scaled_data$total_temp, 1)
scaled_data$total_hum_0 <-dplyr::lag(scaled_data$total_hum, 0)
scaled_data$total_windImpact_0 <-dplyr::lag(scaled_data$total_windImpact, 0)


scaled_data_lags <- scaled_data %>% na.omit()


ts_pm_log_lag <- ts(log_data$total_pm[-1], frequency = 24, 
                      start = c(year(min(scaled_data_lags$hour)), 
                                yday(min(scaled_data_lags$hour))))

# potrebuju oriznout casovou radu, aby sedely delky (lag posun o 1 hodnotu)
# [-1] znamena - vynechani prvni hodnoty





######################x
## pro logaritmická
log_data$vehicle_count_0 <-dplyr::lag(log_data$vehicle_count, 0)
log_data$avg_no2_0 <-dplyr::lag(log_data$avg_no2, 0)
log_data$total_windSpeed_0 <-dplyr::lag(log_data$total_windSpeed, 0)
log_data$total_pressure_1 <-dplyr::lag(log_data$total_pressure, 1)
log_data$total_temp_1 <-dplyr::lag(log_data$total_temp, 1)
log_data$total_hum_0 <-dplyr::lag(log_data$total_hum, 0)
log_data$total_windImpact_0 <-dplyr::lag(log_data$total_windImpact, 0)


log_data_lags <- log_data %>% na.omit()


ts_pm_log_lag <- ts(log_data$total_pm, frequency = 24, 
                      start = c(year(min(log_data_lags$hour)), 
                                yday(min(log_data_lags$hour))))




######################x
## pro škálovaná logaritmická
log_scaled_data$vehicle_count_0 <-dplyr::lag(log_scaled_data$vehicle_count, 0)
log_scaled_data$avg_no2_0 <-dplyr::lag(log_scaled_data$avg_no2, 0)
log_scaled_data$total_windSpeed_0 <-dplyr::lag(log_scaled_data$total_windSpeed, 0)
log_scaled_data$total_pressure_1 <-dplyr::lag(log_scaled_data$total_pressure, 1)
log_scaled_data$total_temp_1 <-dplyr::lag(log_scaled_data$total_temp, 1)
log_scaled_data$total_hum_0 <-dplyr::lag(log_scaled_data$total_hum, 0)
log_scaled_data$total_windImpact_0 <-dplyr::lag(log_scaled_data$total_windImpact, 0)


log_scaled_data_lags <- log_data %>% na.omit()


ts_pm_log_lag <- ts(log_data$total_pm, frequency = 24, 
                    start = c(year(min(log_data_lags$hour)), 
                              yday(min(log_data_lags$hour))))




#########################
####################x###xx
# Vytvoření matice regresorů


## s lagy, škálované
# pouzivam pouze skalovana z historickych duvodu
# (pred tim nic jinyho nez skalovany data nedavaly smysl)

x_reg_scaled <- as.matrix(scaled_data_lags[, c(
                                   "vehicle_count_0", 
                                   "avg_no2_0", 
                                   "total_windSpeed_0", 
                                   "total_pressure_1", 
                                   "total_temp_1",
                                   "total_hum_0",
                                   "total_windImpact_0")])

# nepoustet, nize je arima uz s vypocitanyma slozkama
arimax_seasonal <- auto.arima(ts_pm_log_lag, 
                              xreg = x_reg_scaled, 
                              d = 1,      
                              D = 1,      
                              seasonal = TRUE,  
                              stepwise = FALSE, 
                              approximation = FALSE, 
                              trace = TRUE)  
## 111, 210

arimax_seasonal <- Arima(ts_pm_log_lag, 
                   order = c(1,1,1),        
                   seasonal = list(order = c(2,1,0), period = 24),  
                   xreg = x_reg_scaled,  
                   method = "CSS-ML")  


summary(arimax_seasonal)
# lag 1 u teploty neprospiva, zkusim pouzit spise lag 0 pro teplotu

x_reg_scaled <- as.matrix(scaled_data_lags[, c(
  "vehicle_count_0", 
  "avg_no2_0", 
  "total_windSpeed_0", 
  "total_pressure", 
  "total_temp",
  "total_hum_0",
  "total_windImpact_0")])

arimax_seasonal <- Arima(ts_pm_log_lag, 
                         order = c(1,1,1),        
                         seasonal = list(order = c(2,1,0), period = 24),  
                         xreg = x_reg_scaled,  
                         method = "CSS-ML")  


summary(arimax_seasonal)
### odebírám tlak a windspeed

x_reg_scaled <- as.matrix(scaled_data_lags[, c(
  "vehicle_count_0", 
  "avg_no2_0", 
  "total_temp",
  "total_hum_0",
  "total_windImpact_0")])

arimax_seasonal <- Arima(ts_pm_log_lag, 
                         order = c(1,1,1),        
                         seasonal = list(order = c(2,1,0), period = 24),  
                         xreg = x_reg_scaled,  
                         method = "CSS-ML")  


summary(arimax_seasonal)


par(mfrow=c(2,1))
acf(residuals(arimax_seasonal), main="ACF reziduí Sezónního ARIMAX modelu")
pacf(residuals(arimax_seasonal), main="PACF reziduí Sezónního ARIMAX modelu")
par(mfrow=c(1,1))

Box.test(residuals(arimax_seasonal), type="Ljung-Box")
qqnorm(residuals(arimax_seasonal))
qqline(residuals(arimax_seasonal), col="red")

checkresiduals(arimax_seasonal)


coeftest(arimax_seasonal)
# nejlepsi model, ktereho jsem mohl dosahnout


# koeficient determinace:

predikce <- arimax_seasonal$fitted
skutecne <- arimax_seasonal$x
rezidua <- skutecne - predikce
var_res <- var(rezidua, na.rm = TRUE)
var_celk <- var(skutecne, na.rm = TRUE)

R2 <- 1 - (var_res / var_celk)

cat("Koeficient determinace (R²):", R2)


########################################################################
########################################################################
########################################################################

