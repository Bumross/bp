library(forecast)
library(lubridate)
library(zoo)
library(dynlm)



auta_msts <- msts(data$valid_speed_count, seasonal.periods = c(24, 168), start = decimal_date(min(data$cas)))

reg_auta_msts <- model.matrix(~ 
                                as.factor(hour(data$cas)) +
                                as.factor(data$den_v_tydnu) +
                                as.factor(data$velke_svatky) +
                                as.factor(data$letni_prazdniny))[,-1]


## podle overeni drive, pracuji s prvni diferenciaci

model_multi <- auto.arima(auta_msts, xreg = reg_auta_msts, d = 1, D = 0,
                          seasonal = FALSE, stepwise = FALSE, approximation = FALSE)


checkresiduals(model_multi)

resid_multi <- residuals(model_multi)
ts_res <- ts(resid_multi, frequency = 24)

plot(ts_res, main = "Rezidua modelu")
plot(diff(ts_res), main = "1. diference")
plot(diff(ts_res, lag = 168), main = "168hodinová diference")


var(ts_res)
var(diff(ts_res))
var(diff(ts_res, lag = 168))


acf(ts_res, lag.max = 200, main = "ACF reziduí")
pacf(ts_res, lag.max = 200, main = "PACF reziduí")


summary(model_multi)
#0 1 5


###############################################################################


auta_msts <- msts(data$valid_speed_count, seasonal.periods = c(24, 168),
                                       start = decimal_date(min(data$cas)))


###############################################################################



data$auta_lag1 <- dplyr::lag(data$valid_speed_count, 1)
data$auta_lag24 <- dplyr::lag(data$valid_speed_count, 24)
data$auta_lag168 <- dplyr::lag(data$valid_speed_count, 168)



#################################################################################
## multisezonni model pro auta
# dynamicky linearni


dyn_mod_2 <- dynlm(auta_msts ~ 
                     as.factor(den_v_tydnu) + 
                     as.factor(velke_svatky) + 
                     as.factor(hodina) +
                     #as.factor(tretina_dne) +
                     as.factor(letni_prazdniny) +
                     L(auta_msts, 1) + 
                     L(auta_msts, 24) + 
                     L(auta_msts, 168),
                   data = data)
Anova(dyn_mod_2)

# tretina dne to cely prznila

checkresiduals(dyn_mod_2)

res_2 <- residuals(dyn_mod_2)
res_2.t <- ts(res_2)
plot(res_2.t)

acf(res_2.t)
acf(res_2.t, lag.max = 400)
acf(res_2.t, lag.max = 170)




fitted_vals <- fitted(dyn_mod_2)
original_vals <- data$valid_speed_count


cas <- data$cas  # nebo: seq_along(fitted_vals)

offset <- length(cas) - length(fitted_vals)


cas_fit <- cas[-(1:offset)]


plot(cas, data$valid_speed_count, type = "l", col = "black", lwd = 1.5,
     main = "Skutečné vs. fitované hodnoty (dynlm)",
     xlab = "Čas", ylab = "Počet aut")

lines(cas_fit, fitted_vals, col = "red", lwd = 2)
legend("topright", legend = c("Skutečné", "Fitované"),
       col = c("black", "red"), lwd = 2)




fitted_ts <- ts(fitted_vals, frequency = 24)
decomp_fit <- stl(fitted_ts, s.window = "periodic")
plot(decomp_fit)





