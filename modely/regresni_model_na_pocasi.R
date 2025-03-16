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



##############################################################################
##############################################################################
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
