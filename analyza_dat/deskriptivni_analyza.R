library(dplyr)
library(ggplot2)
library(psych)
library(corrplot)
library(scales)

# nutno mit nactene data ze slozky "nacteni_dat" ze souboru "nacteni_dat.R"

mesicni_breaks <- seq(from = as.Date("2024-01-01"), to = as.Date("2025-02-01"), by = "1 month")
###################################################################### 
##                                  
##       kvalita ovzdusi (pm)       
##                                  
###################################################################### 
 

# korelace mezi pm
cor_matrix <- cor(
  polution[, c("data_pm10", "data_pm25", "data_pm40", "data_pm100")],
  use = "complete.obs",  # ignoruje NA
  method = "pearson"     # klasická Pearsonova korelace
)

print(cor_matrix)
#### vhodne zvolit pouze jednu - data_pm100, volim si tebe!



ggplot(polution, aes(x = data_pm100)) +
  geom_histogram(binwidth = 2, fill = "grey30", color = "white") +
  labs(title = "Rozložení koncentrace PM10", x = "PM10 [µg/m³]", y = "Počet výskytů") +
  theme_minimal()


ggplot(polution, aes(x = data_no2)) +
  geom_histogram(binwidth = 2, fill = "grey30", color = "white") +
  labs(title = "Rozložení koncentrace NO2", x = "NO2 [µg/m³]", y = "Počet výskytů") +
  theme_minimal()


# zobrazeni denniho prumeru
polution_daily <- polution %>%
  mutate(den = as.Date(cas)) %>%
  group_by(den) %>%
  summarise(
    prumer_pm10 = mean(data_pm100, na.rm = TRUE),
    prumer_no2 = mean(data_no2, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(polution_daily, aes(x = den, y = prumer_pm10)) +
  geom_line(color = "grey30", linewidth = 0.7) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%m/%Y"
  ) +
  labs(title = "Denní průměr koncentrace PM10",
       x = "Měsíc", y = "PM10 [µg/m³]") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(polution_daily, aes(x = den, y = prumer_no2)) +
  geom_line(color = "grey30", linewidth = 0.7) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%m/%Y"
  ) +
  labs(title = "Denní průměr koncentrace NO2",
       x = "Měsíc", y = "NO2 [µg/m³]") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###
polution %>%
  mutate(hodina = hour(cas)) %>%
  group_by(hodina) %>%
  summarise(prumer_pm100 = mean(data_pm100, na.rm = TRUE)) %>%
  ggplot(aes(x = hodina, y = prumer_pm100)) +
  geom_col(fill = "grey30") +
  labs(title = "Průměrná koncentrace PM10 podle hodiny",
       x = "Hodina", y = "PM100 (µg/m³)") +
  theme_minimal()

polution %>%
  mutate(hodina = hour(cas)) %>%
  group_by(hodina) %>%
  summarise(prumer_no2 = mean(data_no2, na.rm = TRUE)) %>%
  ggplot(aes(x = hodina, y = prumer_no2)) +
  geom_col(fill = "grey30") +
  labs(title = "Průměrná koncentrace NO2 podle hodiny",
       x = "Hodina", y = "NO2 (µg/m³)") +
  theme_minimal()


##
polution %>%
  mutate(den_v_tydnu = wday(cas, week_start = 1)) %>%
  mutate(den_v_tydnu = factor(den_v_tydnu,
                              levels = 1:7,
                              labels = c("po", "ut", "st", "ct", "pa", "so", "ne"))) %>%
  group_by(den_v_tydnu) %>%
  summarise(prumer_pm100 = mean(data_pm100, na.rm = TRUE)) %>%
  ggplot(aes(x = den_v_tydnu, y = prumer_pm100)) +
  geom_col(fill = "grey30") +
  labs(title = "Průměrná koncentrace PM10 podle dne v týdnu",
       x = "Den v týdnu", y = "PM10 (µg/m³)") +
  theme_minimal()

polution %>%
  mutate(den_v_tydnu = wday(cas, week_start = 1)) %>%
  mutate(den_v_tydnu = factor(den_v_tydnu,
                              levels = 1:7,
                              labels = c("po", "ut", "st", "ct", "pa", "so", "ne"))) %>%
  group_by(den_v_tydnu) %>%
  summarise(prumer_no2 = mean(data_no2, na.rm = TRUE)) %>%
  ggplot(aes(x = den_v_tydnu, y = prumer_no2)) +
  geom_col(fill = "grey30") +
  labs(title = "Průměrná koncentrace NO2 podle dne v týdnu",
       x = "Den v týdnu", y = "NO2 (µg/m³)") +
  theme_minimal()


##
polution %>%
  mutate(rok_mesic = floor_date(as.Date(cas), "month")) %>%
  group_by(rok_mesic) %>%
  summarise(prumer_pm100 = mean(data_pm100, na.rm = TRUE)) %>%
  ggplot(aes(x = rok_mesic, y = prumer_pm100)) +
  geom_col(fill = "grey30") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(title = "Průměrná koncentrace PM10 podle měsíce",
       x = "Měsíc", y = "PM10 (µg/m³)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


polution %>%
  mutate(rok_mesic = floor_date(as.Date(cas), "month")) %>%
  group_by(rok_mesic) %>%
  summarise(prumer_no2 = mean(data_no2, na.rm = TRUE)) %>%
  ggplot(aes(x = rok_mesic, y = prumer_no2)) +
  geom_col(fill = "grey30") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(title = "Průměrná koncentrace NO podle měsíce",
       x = "Měsíc", y = "NO (µg/m³)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####
##### Boxploty
polution %>%
  mutate(hodina = hour(cas)) %>%
  ggplot(aes(x = as.factor(hodina), y = data_pm100)) +
  geom_boxplot(fill = "grey70") +
  labs(title = "Rozdělení PM10 podle hodin", x = "Hodina", y = "PM10 (µg/m³)") +
  theme_minimal()

polution %>%
  mutate(hodina = hour(cas)) %>%
  ggplot(aes(x = as.factor(hodina), y = data_no2)) +
  geom_boxplot(fill = "grey70") +
  labs(title = "Rozdělení NO2 podle hodin", x = "Hodina", y = "NO2 (µg/m³)") +
  theme_minimal()

##
polution %>%
  mutate(den_v_tydnu = wday(cas, week_start = 1)) %>%
  mutate(den_v_tydnu = factor(den_v_tydnu,
                              levels = 1:7,
                              labels = c("po", "ut", "st", "ct", "pa", "so", "ne"))) %>%
  ggplot(aes(x = den_v_tydnu, y = data_pm100)) +
  geom_boxplot(fill = "grey70") +
  labs(title = "Rozdělení PM10 podle dne v týdnu", x = "Den", y = "PM10 (µg/m³)") +
  theme_minimal()

polution %>%
  mutate(den_v_tydnu = wday(cas, week_start = 1)) %>%
  mutate(den_v_tydnu = factor(den_v_tydnu,
                              levels = 1:7,
                              labels = c("po", "ut", "st", "ct", "pa", "so", "ne"))) %>%
  ggplot(aes(x = den_v_tydnu, y = data_no2)) +
  geom_boxplot(fill = "grey70") +
  labs(title = "Rozdělení NO2 podle dne v týdnu", x = "Den", y = "NO2 (µg/m³)") +
  theme_minimal()

##
polution %>%
  mutate(rok_mesic = floor_date(as.Date(cas), "month")) %>%
  ggplot(aes(x = rok_mesic, y = data_pm100, group = rok_mesic)) +
  geom_boxplot(fill = "grey70") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(title = "Rozdělení PM10 podle měsíce", x = "Měsíc", y = "PM10 (µg/m³)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


polution %>%
  mutate(rok_mesic = floor_date(as.Date(cas), "month")) %>%
  ggplot(aes(x = rok_mesic, y = data_no2, group=rok_mesic)) +
  geom_boxplot(fill = "grey70") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(title = "Rozdělení NO2 podle měsíce", x = "Měsíc", y = "NO2 (µg/m³)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################################################################### 
##                                  
##       podmínky počasí (meteo)    
##                                  
###################################################################### 

cor_matrix <- cor(
  meteo[, c("data_windSpeed", "data_windImpact")],
  use = "complete.obs",  # ignoruje NA
  method = "pearson"     # klasická Pearsonova korelace
)

print(cor_matrix)



weather_cols <- c("data_temp1", "data_hum1", "data_pressure", 
                  "data_windSpeed", "data_windImpact", "data_volumeMm")

deskriptiva_wind <- psych::describe(meteo[, weather_cols])
print(deskriptiva_wind)

# Příklad pro teplotu
meteo %>%
  mutate(den = as.Date(cas)) %>%
  group_by(den) %>%
  summarise(prumera_teplota = mean(data_temp1, na.rm = TRUE)) %>%
  ggplot(aes(x = den, y = prumera_teplota)) +
  geom_line(color = "grey40", alpha = 0.8) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(
    title = "Denní průměr teploty",
    x = "Měsíc", y = "Teploty (°C)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


meteo %>%
  mutate(den = as.Date(cas)) %>%
  group_by(den) %>%
  summarise(prumerna_vlhkost = mean(data_hum1, na.rm = TRUE)) %>%
  ggplot(aes(x = den, y = prumerna_vlhkost)) +
  geom_line(color = "grey40", alpha = 0.8) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(
    title = "Denní průměr vlhkosti",
    x = "Měsíc", y = "Vlhkost (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

meteo %>%
  mutate(den = as.Date(cas)) %>%
  group_by(den) %>%
  summarise(prumerna_rychlost = mean(data_windSpeed, na.rm = TRUE)) %>%
  ggplot(aes(x = den, y = prumerna_rychlost)) +
  geom_line(color = "grey40", alpha = 0.8) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(title = "Denní průměr rychlosti větru", x = "Měsíc", y = "Rychlost větru (m/s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

meteo %>%
  mutate(den = as.Date(cas)) %>%
  group_by(den) %>%
  summarise(prumer_tlaku = mean(data_pressure, na.rm = TRUE)) %>%
  ggplot(aes(x = den, y = prumer_tlaku)) +
  geom_line(color = "grey40", alpha = 0.8) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(title = "Denní průměr atmosférického tlaku", x = "Měsíc", y = "Tlak (Pa)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


meteo %>%
  mutate(den = as.Date(cas)) %>%
  group_by(den) %>%
  summarise(prumer_srazek = mean(data_volumeMm, na.rm = TRUE)) %>%
  ggplot(aes(x = den, y = prumer_srazek)) +
  geom_line(color = "grey40", alpha = 0.8) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(title = "Denní průměr srážek", x = "Měsíc", y = "Srážky (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

meteo %>%
  mutate(den = as.Date(cas)) %>%
  group_by(den) %>%
  summarise(prumer_impakt = mean(data_windImpact, na.rm = TRUE)) %>%
  ggplot(aes(x = den, y = prumer_impakt)) +
  geom_line(color = "grey40", alpha = 0.8) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(title = "Denní průměr nárazu větru", x = "Měsíc", y = "Síla větru (m/s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###################################################################### 
##                                  
##       auta (ddb)                 
##                                  
###################################################################### 


summary(cars$valid_speed_count)
psych::describe(cars$valid_speed_count)


# denni cyklus aut - v prumeru celym datasetem
cars %>%
  mutate(hodina_dne = hour(cas)) %>%  # z hodinového sloupce vytáhne pouze hodinu (0–23)
  group_by(hodina_dne) %>%
  summarise(prumer_aut = mean(valid_speed_count, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(hodina_dne), y = prumer_aut)) +
  geom_col(fill = "grey30") +
  labs(
    title = "Průměrný počet aut podle denní hodiny",
    x = "Hodina dne",
    y = "Průměrný počet aut"
  ) +
  theme_minimal()






# tydenni cyklus aut - v prumeru celym datasetem
# přidání dnů v týdnu:
cars <- cars %>%
  mutate(den_v_tydnu = wday(cas, week_start = 1)) %>%
  mutate(den_v_tydnu = factor(den_v_tydnu,
                              levels = 1:7,
                              labels = c("po", "ut", "st", "ct", "pa", "so", "ne")))

cars %>%
  group_by(den_v_tydnu) %>%
  summarise(prumer_aut = mean(valid_speed_count, na.rm = TRUE)) %>%
  ggplot(aes(x = den_v_tydnu, y = prumer_aut)) +
  geom_col(fill = "grey30") +
  labs(title = "Průměrný počet aut podle dne v týdnu", x = "Den", y = "Počet aut") +
  theme_minimal()




# casovy vyvoj
Sys.setlocale("LC_TIME", "Czech")

mesicni_breaks <- seq(from = as.Date("2024-01-01"),
                      to = as.Date("2025-02-01"),
                      by = "1 month")

cars %>%
  mutate(datum = as.Date(cas)) %>%
  group_by(datum) %>%
  summarise(denni_pocet = sum(valid_speed_count)) %>%
  ggplot(aes(x = datum, y = denni_pocet)) +
  geom_line() +
  scale_x_date(
    breaks = mesicni_breaks,
    labels = function(x) format(x, "%m/%Y")
  ) +
  labs(title = "Denní počet projetých aut", x = "Měsíc", y = "Počet aut") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


cars %>%
  mutate(rok_mesic = floor_date(as.Date(cas), "month")) %>%
  group_by(rok_mesic) %>%
  summarise(prumer_aut = mean(valid_speed_count, na.rm = TRUE)) %>%
  ggplot(aes(x = rok_mesic, y = prumer_aut)) +
  geom_col(fill = "grey30") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(title = "Průměrný počet aut podle měsíce",
       x = "Měsíc", y = "Počet aut") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


cars %>%
  mutate(hodina = hour(cas)) %>%
  ggplot(aes(x = factor(hodina), y = valid_speed_count)) +
  geom_boxplot(fill = "grey70") +
  labs(title = "Rozdělení počtu aut podle hodiny",
       x = "Hodina dne", y = "Počet aut") +
  theme_minimal()

cars %>%
  mutate(den_v_tydnu = wday(cas, week_start = 1)) %>%
  mutate(den_v_tydnu = factor(den_v_tydnu,
                              levels = 1:7,
                              labels = c("po", "ut", "st", "ct", "pa", "so", "ne"))) %>%
  ggplot(aes(x = den_v_tydnu, y = valid_speed_count)) +
  geom_boxplot(fill = "grey70") +
  labs(title = "Rozdělení počtu aut podle dne v týdnu",
       x = "Den v týdnu", y = "Počet aut") +
  theme_minimal()


cars %>%
  mutate(rok_mesic = floor_date(as.Date(cas), "month")) %>%
  ggplot(aes(x = rok_mesic, y = valid_speed_count, group = rok_mesic)) +
  geom_boxplot(fill = "grey70") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
  labs(title = "Rozdělení počtu aut podle měsíce",
       x = "Měsíc", y = "Počet aut") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


