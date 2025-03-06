library(dplyr)
library(ggplot2)
library(lubridate)
library(tseries)
library(zoo)
library(lmtest)
library(forecast)

# lmtests prikaz, ktery mi rovnou da p hodnoty k modelu

################### Načtení dat
# vitr
path <- "C:\\Users\\bruli\\OneDrive\\UJEP_BP\\data\\meteo_2024_11_01.csv"
wind <- read.csv(path)

wind$date <- as.POSIXct(wind$date, format="%Y-%m-%d %H:%M:%OS")

hourly_summary <- wind %>%
  mutate(hour = floor_date(date, "hour")) %>%
  group_by(sensor_id, hour) %>%
  summarise(
    avg_temp = mean(data_temp1, na.rm = TRUE),
    avg_hum = mean(data_hum1, na.rm = TRUE),
    avg_pressure = mean(data_pressure, na.rm = TRUE),
    avg_windSpeed = mean(data_windSpeed, na.rm = TRUE),
    avg_windImpact = mean(data_windImpact, na.rm = TRUE),
    avg_volume = mean(data_volumeMm, na.rm = TRUE),
    .groups = "drop"
  )


final_summary <- hourly_summary %>%
  group_by(hour) %>%
  summarise(
    total_temp = mean(avg_temp, na.rm = TRUE),
    total_hum = mean(avg_hum, na.rm = TRUE),
    total_pressure = mean(avg_pressure, na.rm = TRUE),
    total_windSpeed = mean(avg_windSpeed, na.rm = TRUE),
    total_windImpact = mean(avg_windImpact, na.rm = TRUE),
    total_volume = mean(avg_volume, na.rm = TRUE),
    .groups = "drop"
  )


# auta
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

sum_60 <- senzor_density_60 %>%
  group_by(time_interval) %>%
  summarise(vehicle_count = sum(vehicle_count), .groups = "drop")

sum_60$time_interval <- as.POSIXct(sum_60$time_interval, format="%Y-%m-%d %H:%M:%OS")
sum_60 <- sum_60 %>%
  rename(hour = time_interval)


# ovzdusi
path <- "C:\\Users\\bruli\\OneDrive\\UJEP_BP\\data\\pm_2024_11_01.csv"
polution <- read.csv(path)

polution$date <- as.POSIXct(polution$date, format="%Y-%m-%d %H:%M:%OS")

# pro kazdy senzor zvlast
polution_summary <- polution %>%
  mutate(hour = floor_date(date, "hour")) %>% 
  group_by(sensor_id, hour) %>%
  summarise(
    sum_pm10 = sum(data_pm10, na.rm = TRUE),
    sum_pm25 = sum(data_pm25, na.rm = TRUE),
    sum_pm40 = sum(data_pm40, na.rm = TRUE),
    sum_pm100 = sum(data_pm100, na.rm = TRUE),
    avg_no2 = mean(data_no2, na.rm = TRUE),
    .groups = "drop"
  )

# pro vsechny senzory dohromady
hourly_summary <- polution_summary %>%
  group_by(hour) %>%
  summarise(
    total_pm10 = sum(sum_pm10, na.rm = TRUE),
    total_pm25 = sum(sum_pm25, na.rm = TRUE),
    total_pm40 = sum(sum_pm40, na.rm = TRUE),
    total_pm100 = sum(sum_pm100, na.rm = TRUE),
    avg_no2 = mean(avg_no2, na.rm = TRUE),  # Průměr pro NO2
    .groups = "drop"
  )

hourly_summary_combined <- hourly_summary %>%
  mutate(total_pm = total_pm10 + total_pm25 + total_pm40 + total_pm100) %>%
  select(hour, total_pm, avg_no2)


### Sjednoceni vsech dat do jednoho DF
merged_data <- final_summary %>%
  inner_join(hourly_summary_combined, by = "hour") %>%
  inner_join(sum_60, by = "hour")






