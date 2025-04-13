#### popis polution

dim(polution)

n_distinct(polution$date)
n_distinct(polution$id)

colSums(!is.na(polution))

str(polution)
glimpse(polution)


sapply(polution, class)

range(polution$date, na.rm = TRUE)

difftime(max(polution$date, na.rm = TRUE), min(polution$date, na.rm = TRUE), units = "days")

polution %>%
  mutate(day = as.Date(date)) %>%
  count(day) %>%
  summary()

# Vypsat proměnné + základní statistiky
continuous_vars <- c("data_pm10", "data_pm25", "data_pm40", "data_pm100", "data_no2")
summary(polution[ , continuous_vars])

# Ověření, že jde o numeric
sapply(polution[ , continuous_vars], is.numeric)

# Počet jedinečných hodnot v indexech
polution %>% 
  select(starts_with("recommendation_metrics")) %>% 
  summarise_all(~n_distinct(.))

# Indexy jako ordinalní proměnné (1–6)
cat_vars <- names(polution)[grepl("index", names(polution))]
lapply(polution[ , cat_vars], table, useNA = "ifany")

# Převod na faktor (pokud budeš dělat modely nebo vizualizace)
polution$recommendation_metrics_no2_index <- factor(polution$recommendation_metrics_no2_index, ordered = TRUE)





########################################
########################################
#### popis wind
dim(wind)  # počet řádků a sloupců

n_distinct(wind$id)
n_distinct(wind$date)

colSums(!is.na(wind)) 

str(wind)       # základní přehled
sapply(wind, class)
glimpse(wind)   # pokud používáš dplyr

range(wind$date, na.rm = TRUE)
difftime(max(wind$date, na.rm = TRUE), min(wind$date, na.rm = TRUE), units = "days")

wind %>%
  mutate(day = as.Date(date)) %>%
  count(day) %>%
  summary()

continuous_vars <- c("data_temp1", "data_hum1", "data_pressure", "data_windSpeed", "data_windImpact", "data_volumeMm")
summary(wind[ , continuous_vars])

# Většinou jen jedna: směr větru
table(wind$data_windDir, useNA = "ifany")





############################
###########################
# auta
dim(auta)

sapply(auta, class)
str(auta)
summary(auta)

table(auta$speed)

# Nebo konkrétně:
sum(auta$speed == -1, na.rm = TRUE)
sum(auta$speed == 0, na.rm = TRUE)

# Procentuálně:
mean(auta$speed == -1, na.rm = TRUE)
mean(auta$speed == 0, na.rm = TRUE)

auta <- auta %>%
  filter(speed > 0)

summary(auta)
