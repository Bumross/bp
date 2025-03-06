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

ts_pm_log <- ts(merged_data$log_total_pm, frequency = 24)

arimax_car <- auto.arima(ts_pm_log, 
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