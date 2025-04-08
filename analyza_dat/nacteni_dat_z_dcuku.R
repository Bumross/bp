library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

## tohle je kód pouze pro načtení dat ze souborů získaných od dcuku 
            # nedoporučuju zapínat, pokud nemáte >16 Gbit RAM
# pro načtení dat prosím využívat soubor "nacteni_dat.R" ve složce "nacteni_dat"


wind_path <- list.files(path = "C://Users//bruli//Desktop//nove_meteo", pattern = "\\.csv$", full.names = TRUE)
polution_path <- list.files(path = "C://Users//bruli//Desktop//nove_pm", pattern = "\\.csv$", full.names = TRUE)
auta_path <- list.files(path = "C://Users//bruli//Desktop//nove_ddb", pattern = "\\.csv$", full.names = TRUE)



#############
# znecisteni

polution <- polution_path %>%
  lapply(read.csv) %>%
  bind_rows()

#vybrani jen hodnot, ktere jsou realne merene:
polution_filtered <- polution %>%
  select(date, sensor_id, data_pm10, data_pm25, data_pm40, data_pm100, data_no2)

write.csv(polution_filtered, "polution.csv", row.names = FALSE)


###
# meteo
wind <- wind_path %>%
  lapply(read.csv) %>%
  bind_rows()

wind_filtered <- wind %>%
  select(-id, -data_windDir, -data_leafWetness)

write.csv(wind_filtered, "meteo.csv", row.names = FALSE)


### auta
auta <- auta_path %>%
  lapply(read.csv) %>%
  bind_rows()


auta <- auta %>%
  mutate(hour = floor_date(as_datetime(X_time), unit = "hour")) %>%
  filter(speed != -1 & speed != 0) %>%
  group_by(hour) %>%
  summarise(valid_speed_count = n()) %>%
  arrange(hour)

write.csv(auta, "auta.csv", row.names = FALSE)
