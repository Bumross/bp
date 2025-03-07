library(dplyr)
library(ggplot2)
library(lubridate)
library(tseries)
library(zoo)
library(lmtest)
library(forecast)

### nutno mit nacteny data frame "merged_data" ze souboru "nacteni_dat.R"


###############################################################################
###############################################################################
## model pouze na autech
x_reg_car <- as.matrix(merged_data[, c("vehicle_count")])

ts_pm_log <- ts(log_data$total_pm, frequency = 24)

##
arimax_car <- auto.arima(ts_pm_log, 
                         xreg = x_reg_car, 
                         d = 1,      
                         D = 1,      
                         seasonal = TRUE,  
                         stepwise = FALSE, 
                         approximation = FALSE, 
                         trace = TRUE) 
##

arimax_car <- Arima(ts_pm_log, 
                   order = c(1,1,1),        
                   seasonal = list(order = c(2,1,0), period = 24),  
                   xreg = x_reg_car,  
                   method = "CSS-ML")    



summary(arimax_car)
# spatny


#########################
# skalovana auta

x_reg_car <- as.matrix(scaled_data[, c("vehicle_count")])

arimax_car <- Arima(ts_pm_log, 
                    order = c(1,1,1),        
                    seasonal = list(order = c(2,1,0), period = 24),  
                    xreg = x_reg_car,  
                    method = "CSS-ML") 


summary(arimax_car)

########################
# logaritmicka auta

x_reg_car <- as.matrix(log_data[, c("vehicle_count")])

arimax_car <- Arima(ts_pm_log, 
                    order = c(1,1,1),        
                    seasonal = list(order = c(2,1,0), period = 24),  
                    xreg = x_reg_car,  
                    method = "CSS-ML") 


summary(arimax_car)
# spatny


########################
# skalovana logaritmicka auta

x_reg_car <- as.matrix(log_scaled_data[, c("vehicle_count")])

arimax_car <- Arima(ts_pm_log, 
                    order = c(1,1,1),        
                    seasonal = list(order = c(2,1,0), period = 24),  
                    xreg = x_reg_car,  
                    method = "CSS-ML") 


summary(arimax_car)
# spatny


########################
# pouziti skalovanych dat
# vsude jinde moc velke chyby
#
#
# pridam lag do aut

##############################################################
# lag aut = 1

scaled_data$vehicle_count_1 <- dplyr::lag(scaled_data$vehicle_count, 1)

scaled_data <- scaled_data %>%
  mutate(vehicle_count_1 = dplyr::lag(vehicle_count, 1))

auta_clean <- scaled_data %>% filter(!is.na(vehicle_count_1))


ts_log_pm_lag_auta <- ts(auta_clean$total_pm, frequency = 24, 
                         start = c(year(min(auta_clean$hour)), yday(min(auta_clean$hour))))


x_reg_car <- as.matrix(auta_clean[, "vehicle_count_1"])


arimax_car <- Arima(ts_log_pm_lag_auta,
                    order = c(1,1,1),
                    seasonal = list(order = c(2,1,0), period = 24),
                    xreg = x_reg_car,
                    method = "CSS-ML")

#
arimax_car <- auto.arima(ts_log_pm_lag_auta, 
                         xreg = x_reg_car, 
                         d = 1,      
                         D = 1,      
                         seasonal = TRUE,  
                         stepwise = FALSE, 
                         approximation = FALSE, 
                         trace = TRUE) 
#


summary(arimax_car)
# spatne, zkusim vetsi lag

########################################################################
# lag auta = 2

scaled_data <- scaled_data %>%
  mutate(vehicle_count_2 = dplyr::lag(vehicle_count, 2))

auta_clean <- scaled_data %>% filter(!is.na(vehicle_count_2))


ts_log_pm_lag_auta <- ts(auta_clean$total_pm, frequency = 24, 
                         start = c(year(min(auta_clean$hour)), yday(min(auta_clean$hour))))


x_reg_car <- as.matrix(auta_clean[, "vehicle_count_2"])


arimax_car <- Arima(ts_log_pm_lag_auta,
                    order = c(1,1,1),
                    seasonal = list(order = c(2,1,0), period = 24),
                    xreg = x_reg_car,
                    method = "CSS-ML")

summary(arimax_car)
# spatne,
####################################################
# lag auta = 3

scaled_data <- scaled_data %>%
  mutate(vehicle_count_3 = dplyr::lag(vehicle_count, 3))

auta_clean <- scaled_data %>% filter(!is.na(vehicle_count_3))


ts_log_pm_lag_auta <- ts(auta_clean$total_pm, frequency = 24, 
                         start = c(year(min(auta_clean$hour)), yday(min(auta_clean$hour))))


x_reg_car <- as.matrix(auta_clean[, "vehicle_count_3"])


arimax_car <- Arima(ts_log_pm_lag_auta,
                    order = c(1,1,1),
                    seasonal = list(order = c(2,1,0), period = 24),
                    xreg = x_reg_car,
                    method = "CSS-ML")

summary(arimax_car)
# spatne

###############################################################################
###############################################################################
###############################################################################
# nejlepsi model bez lagu

x_reg_car <- as.matrix(scaled_data[, c("vehicle_count")])

arimax_car <- Arima(ts_pm_log, 
                    order = c(1,1,1),        
                    seasonal = list(order = c(2,1,0), period = 24),  
                    xreg = x_reg_car,  
                    method = "CSS-ML") 


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