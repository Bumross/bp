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




