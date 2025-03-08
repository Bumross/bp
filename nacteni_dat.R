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
path <- "~\\bo_data_analysis\\data\\meteo_2024_11_01.csv"
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
path <- "~\\bo_data_analysis\\data\\ddb_data"

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
path <- "~\\bo_data_analysis\\data\\pm_2024_11_01.csv"
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

###########################################################################
###########################################################################
###########################################################################

### Sjednoceni vsech dat do jednoho DF
merged_data <- final_summary %>%
  inner_join(hourly_summary_combined, by = "hour") %>%
  inner_join(sum_60, by = "hour")


log_data <- merged_data %>%
  mutate(across(-hour, ~ log(. + 1), .names = "{.col}"))

log_scaled_data <- log_data %>%
  mutate(across(-hour, scale, .names = "{.col}"))

scaled_data <- merged_data %>%
  mutate(across(-hour, scale, .names = "{.col}"))


# merged_data = všechna data sjednocená, neupravená
# log_date = logaritmovaná data
# log_scaled_data = logaritmovaná a škálovaná data
# scaled_data = škálovaná data


###########################################################################
###########################################################################
###########################################################################
# smazani nepotrebnych promennych

rm(final_summary)
rm(combined_data)
rm(hourly_summary)
rm(hourly_summary_combined)
rm(polution)
rm(polution_summary)
rm(senzor_density_60)
rm(sum_60)
rm(wind)

rm(csv_files)
rm(path)


##########################################################################

# agregace dat do 3 zapisu denne - 8, 16, 24

merged_data <- merged_data %>%
  mutate(day_period = case_when(
    hour(hour) >= 0 & hour(hour) < 8  ~ format(hour, "%Y-%m-%d 08:00:00"),
    hour(hour) >= 8 & hour(hour) < 16 ~ format(hour, "%Y-%m-%d 16:00:00"),
    hour(hour) >= 16 & hour(hour) < 24 ~ format(hour, "%Y-%m-%d 00:00:00", tz = "UTC")  # Další den v 00:00
  )) %>%
  mutate(day_period = as.POSIXct(day_period, tz = "UTC"))  # Převod na časový formát

merged_data_week <- merged_data %>%
  group_by(day_period) %>%
  summarise(
    total_temp = mean(total_temp, na.rm = TRUE),
    total_hum = mean(total_hum, na.rm = TRUE),
    total_pressure = mean(total_pressure, na.rm = TRUE),
    total_windSpeed = mean(total_windSpeed, na.rm = TRUE),
    total_windImpact = mean(total_windImpact, na.rm = TRUE),
    total_volume = mean(total_volume, na.rm = TRUE),
    total_pm = sum(total_pm, na.rm = TRUE),
    avg_no2 = mean(avg_no2, na.rm = TRUE),
    vehicle_count = sum(vehicle_count, na.rm = TRUE),  # SUMA pro vozidla!
    .groups = "drop"
  ) %>%
  rename(hour = day_period)

log_data_week <- merged_data_week %>%
  mutate(across(-hour, ~ log(. + 1), .names = "{.col}"))

log_scaled_data_week <- log_data_week %>%
  mutate(across(-hour, scale, .names = "{.col}"))

scaled_data_week <- merged_data_week %>%
  mutate(across(-hour, scale, .names = "{.col}"))


