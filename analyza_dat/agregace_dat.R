polution <- polution %>%
  mutate(
    hour = floor_date(as.POSIXct(as.character(date)), unit = "hour")
  ) %>%
  group_by(hour) %>%
  summarise(across(c(data_pm10, data_pm25, data_pm40, data_pm100, data_no2),
                   ~ mean(.x, na.rm = TRUE))) %>%
  ungroup()



meteo <- meteo %>%
  mutate(hour = floor_date(as.POSIXct(date), unit = "hour")) %>%  # zaokrouhlení času
  group_by(hour) %>%
  summarise(across(c(data_temp1, data_hum1, data_pressure, data_windSpeed,
                     data_windImpact, data_volumeMm),
                   ~ mean(.x, na.rm = TRUE))) %>%
  ungroup()



### sjednoceni vseho do jednoho data frame:

