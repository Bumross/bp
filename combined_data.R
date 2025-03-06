library(dplyr)
library(ggplot2)
library(lubridate)
library(tseries)
library(zoo)


###############################################################################
# AUTA
###############################################################################

path <- "C:\\Users\\bruli\\OneDrive\\UJEP_BP\\data\\ddb_data"

csv_files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

# načtení dat ddb do jednoho data frame:
combined_data <- csv_files %>%
  lapply(read.csv) %>%
  bind_rows()


senzor_density_60 <- combined_data %>%
  mutate(
    X_time = ymd_hms(X_time),
    time_interval = floor_date(X_time, "60 minutes")  
  )

senzor_density_60 <- senzor_density_60 %>%
  group_by(sensor, time_interval) %>%
  summarise(vehicle_count = n(), .groups = "drop")

senzory60 <- unique(senzor_density_60$sensor)

sensor_data_60 <- list()

# senzory zvlášť
for (sensor in senzory60) {
  sensor_data <- senzor_density_60 %>% filter(sensor == !!sensor)
  
  sensor_data_60[sensor] <- sensor_data
  
  plot <- ggplot(sensor_data, aes(x = time_interval, y = vehicle_count)) +
    geom_line() +
    labs(
      title = paste("Počet aut každou hodinu - Senzor:", sensor),
      x = "Čas",
      y = "Počet aut"
    ) +
    theme_minimal()
  
  print(plot)
}

# všechny senzory najednou
sum_60 <- senzor_density_60 %>%
  group_by(time_interval) %>%
  summarise(vehicle_count = sum(vehicle_count), .groups = "drop")

ggplot(sum_60, aes(x = time_interval, y = vehicle_count)) +
  geom_line() +
  labs(
    title = "Celkový počet aut každou hodinu",
    x = "Čas",
    y = "Počet aut"
  ) +
  theme_minimal()


## dekompozice časové řady s periodicitou 1 den
sum_60.ts <- ts(sum_60$vehicle_count, frequency = 24, start = c(2024, 214/365))
plot(sum_60.ts)
sum_60.dec <- stl(sum_60.ts, s.window = "periodic")
plot(sum_60.dec)



###############################################################################
plot(hourly_summary_combined$hour, hourly_summary_combined$total_pm, type="l", main="Časová řada celkového PM", xlab="Čas", ylab="PM koncentrace")

par(mfrow=c(2,1))
acf(hourly_summary_combined$total_pm, na.action=na.pass, main="ACF PM koncentrace")
pacf(hourly_summary_combined$total_pm, na.action=na.pass, main="PACF PM koncentrace")
par(mfrow=c(1,1))

par(mfrow=c(3,2))
ccf(hourly_summary_combined$total_pm, hourly_summary_combined$total_temp, na.action=na.pass, main="PM vs Teplota")
ccf(hourly_summary_combined$total_pm, hourly_summary_combined$total_hum, na.action=na.pass, main="PM vs Vlhkost")
ccf(hourly_summary_combined$total_pm, hourly_summary_combined$total_pressure, na.action=na.pass, main="PM vs Tlak")
ccf(hourly_summary_combined$total_pm, hourly_summary_combined$total_windSpeed, na.action=na.pass, main="PM vs Rychlost větru")
ccf(hourly_summary_combined$total_pm, hourly_summary_combined$total_windImpact, na.action=na.pass, main="PM vs Dopad větru")
ccf(hourly_summary_combined$total_pm, hourly_summary_combined$total_volume, na.action=na.pass, main="PM vs Srážky")
par(mfrow=c(1,1))







