library(GGally)
library(ggplot2)
library(dplyr)

data_plot <- data %>%
  select(
    `PM (prachové částice)` = data_pm100,
    `NO2` = data_no2,
    `Počet aut` = valid_speed_count,
    `Teplota [°C]` = data_temp1,
    `Vlhkost [%]` = data_hum1,
    `Rychlost větru [m/s]` = data_windSpeed,
    `Srážky [mm]` = data_volumeMm
  ) %>%
  na.omit()

ggpairs(data_plot,
        lower = list(continuous = wrap("points", alpha = 0.3, size = 0.4)),
        upper = list(continuous = wrap("cor", size = 3.5)),
        diag = list(continuous = wrap("densityDiag")),
        title = "Korelační matice")
