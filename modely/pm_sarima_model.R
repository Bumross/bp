library(dplyr)
library(ggplot2)
library(lubridate)
library(tseries)
library(zoo)
library(lmtest)
library(forecast)
#################################################
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

ts_pm_log <- ts(log_data$total_pm, frequency = 24, start = c(year(min(merged_data$hour)), yday(min(merged_data$hour))))

sarima_model_log <- auto.arima(ts_pm_log, d=1, D=1, 
                               seasonal=TRUE, stepwise=FALSE, approximation=FALSE, 
                               trace=TRUE, lambda=NULL)


sarima_model_log <- Arima(ts_pm_log, 
                          order = c(1,1,1),         
                          seasonal = list(order = c(2,1,0), period = 24),     
                          method = "ML") 


summary(sarima_model_log)
# sarima 1 1 1, 2 1 0

par(mfrow=c(2,1))
acf(residuals(sarima_model_log), main="ACF reziduí SARIMA modelu (log)")
pacf(residuals(sarima_model_log), main="PACF reziduí SARIMA modelu (log)")
par(mfrow=c(1,1))

Box.test(residuals(sarima_model_log), type="Ljung-Box")
qqnorm(residuals(sarima_model_log))
qqline(residuals(sarima_model_log), col="red")

checkresiduals(sarima_model_log)

# lepsi presnost, logaritmizace vyrazne zlepsila model

# sarima_model_log 
# nejlepší model samostatné časové řady



############################################################################
############################################################################
############################################################################
# tydenni sezonnost


par(mfrow=c(2,1))
acf(merged_data_week$total_pm, lag.max=21, main="ACF PM koncentrace (týden)")
pacf(merged_data_week$total_pm, lag.max=21, main="PACF PM koncentrace (týden)")
par(mfrow=c(1,1))



# diference = 1, musím s tím počítat dále
# pokračuji zatím pouze s denní sezónností
# acf naznačuje sezónní nestacionaritu - volím d=1

ts_pm_week <- ts(merged_data_week$total_pm, frequency = 21, start = c(year(min(merged_data$hour)), yday(min(merged_data$hour))))
plot(ts_pm_week, main="Časová řada PM koncentrace", ylab="PM")


sarima_model_week <- auto.arima(ts_pm_week, d=1, D=1, 
                           seasonal=TRUE, stepwise=FALSE, approximation=FALSE, 
                           trace=TRUE, lambda=NULL)

summary(sarima_model_week)

# sarima 


#######################
# logaritmuju - rezidua nejsou normálního rozložení

ts_pm_log_week <- ts(log_data$total_pm, frequency = 21, start = c(year(min(merged_data$hour)), yday(min(merged_data$hour))))

sarima_model_log_week <- auto.arima(ts_pm_log_week, d=1, D=1, 
                               seasonal=TRUE, stepwise=FALSE, approximation=FALSE, 
                               trace=TRUE, lambda=NULL)


sarima_model_log_week <- Arima(ts_pm_log_week, 
                          order = c(1,1,1),         
                          seasonal = list(order = c(2,1,0), period = 24),     
                          method = "ML") 


summary(sarima_model_log_week)
# sarima 1 1 1, 2 1 0

par(mfrow=c(2,1))
acf(residuals(sarima_model_log_week), main="ACF reziduí SARIMA modelu (log)")
pacf(residuals(sarima_model_log_week), main="PACF reziduí SARIMA modelu (log)")
par(mfrow=c(1,1))

Box.test(residuals(sarima_model_log_week), type="Ljung-Box")
qqnorm(residuals(sarima_model_log_week))
qqline(residuals(sarima_model_log_week), col="red")

checkresiduals(sarima_model_log_week)

