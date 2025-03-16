library(ggplot2)
library(tidyr)
library(lubridate)
library(forecast)

# nutne mit nactene ze souboru "natecni_dat.R" data frame "merged_data"

# merged_data = všechna data sjednocená, neupravená
# log_date = logaritmovaná data
# log_scaled_data = logaritmovaná a škálovaná data
# scaled_data = škálovaná data

variables <- colnames(merged_data)[-1] 

for (var in variables) {
  p <- ggplot(merged_data, aes(x = hour, y = .data[[var]])) +
    geom_line(color = "blue") +
    labs(title = paste("Časová řada:", var), x = "Čas", y = var) +
    theme_minimal()
  print(p)
}

# dekompozice časových řad - s denní sezónností
for (var in variables) {
  ts_data <- ts(merged_data[[var]], start = c(year(min(merged_data$hour)), month(min(merged_data$hour))), frequency = 24)
  
  decomposed <- stl(ts_data, s.window = "periodic")
  
  plot(decomposed, main = paste("Denní dekompozice pro:", var))
}

# dekompozice časových řad - s týdenní sezónností
for (var in variables) {
  ts_data <- ts(merged_data[[var]], start = c(year(min(merged_data$hour)), month(min(merged_data$hour))), frequency = 168)
  decomposed <- stl(ts_data, s.window = "periodic")
  
  plot(decomposed, main = paste("Týdenní dekompozice pro:", var))
}


# lepsi zobrazeni dat histogramem
par(mfrow=c(2,4)) 
hist(merged_data$vehicle_count, main="Počet aut", xlab="Hodnoty", breaks=30)
hist(merged_data$avg_no2, main="NO2", xlab="Hodnoty", breaks=30)
hist(merged_data$total_temp, main="Teplota", xlab="Hodnoty", breaks=30)
hist(merged_data$total_hum, main="Vlhkost", xlab="Hodnoty", breaks=30)
hist(merged_data$total_windSpeed, main="Rychlost větru", xlab="Hodnoty", breaks=30)
hist(merged_data$total_pressure, main="Tlak", xlab="Hodnoty", breaks=30)
hist(merged_data$total_windImpact, main="Vliv větru", xlab="Hodnoty", breaks=30)
par(mfrow=c(1,1))



#####
# CCF

# pm vs vsechno ostatni
# denni data
plot_ccf <- function(x, y, x_label, y_label) {
  ccf_values <- ccf(x, y, lag.max = 24, plot = FALSE)
  df_ccf <- data.frame(lag = ccf_values$lag, correlation = ccf_values$acf)
  
  ggplot(df_ccf, aes(x = lag, y = correlation)) +
    geom_line() +
    geom_point() +
    ggtitle(paste("CCF mezi", x_label, "a", y_label)) +
    xlab("Lag (hodiny)") +
    ylab("Korelace") +
    theme_minimal()
}


variables <- c("total_temp", "total_hum", "total_pressure", "total_windSpeed", 
               "total_windImpact", "total_volume", "avg_no2", "vehicle_count")

for (var in variables) {
  print(plot_ccf(merged_data$total_pm, merged_data[[var]], "total_pm", var))
}

#############################
# pro auta
selected_vars <- c("total_temp", "total_hum", "avg_no2")

for (var in selected_vars) {
  print(plot_ccf(merged_data$vehicle_count, merged_data[[var]], "vehicle_count", var))
}