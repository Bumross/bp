library(ggplot2)
library(tidyr)
library(lubridate)
library(forecast)

# nutne mit nactene ze souboru "natecni_dat.R" data frame "merged_data"

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
