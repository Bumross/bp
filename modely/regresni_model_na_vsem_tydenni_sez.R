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
