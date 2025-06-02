library(ggplot2)
library(dplyr)





# Histogramy s hustotou
ggplot(data, aes(x = data_pm100)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "lightgray", color = "black") +
  geom_density(color = "blue") +
  labs(title = expression("Rozložení koncentrací " * PM[10]), x = expression(PM[10]~(µg/m^3)), y = "Hustota")

ggplot(data, aes(x = data_no2)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "lightgray", color = "black") +
  geom_density(color = "blue") +
  labs(title = expression("Rozložení koncentrací " * NO[2]), x = expression(NO[2]~(µg/m^3)), y = "Hustota")

ggplot(data, aes(x = valid_speed_count)) +
  geom_histogram(bins = 50, fill = "lightgray", color = "black") +
  labs(title = "Rozložení počtu vozidel za hodinu", x = "Počet vozidel", y = "Počet výskytů")










# Boxploty podle měsíce
ggplot(data, aes(x = month, y = data_pm100)) +
  geom_boxplot() +
  labs(
    title = expression("Rozdělení koncentrací " * PM[10] * " podle měsíce"),
    x = "Měsíc",
    y = expression(PM[10]~(µg/m^3))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = month, y = data_no2)) +
  geom_boxplot() +
  labs(
    title = expression("Rozdělení koncentrací " * NO[2] * " podle měsíce"),
    x = "Měsíc",
    y = expression(NO[2]~(µg/m^3))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










# Boxploty podle třetiny dne
ggplot(data, aes(x = factor(hodina), y = valid_speed_count)) +
  geom_boxplot() +
  labs(title = "Počet vozidel podle hodin v rámci dne",
       x = "Hodina (0–23)", y = "Počet vozidel za hodinu")










# Scatterploty – předběžné vztahy
ggplot(data, aes(x = valid_speed_count, y = data_pm100)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = expression("Závislost mezi počtem vozidel a koncentrací " * PM[10]),
       x = "Počet vozidel", y = expression(PM[10]~(µg/m^3)))

ggplot(data, aes(x = valid_speed_count, y = data_no2)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = expression("Závislost mezi počtem vozidel a koncentrací " * NO[2]),
       x = "Počet vozidel", y = expression(NO[2]~(µg/m^3)))










#### topna sezona vs netopna sezona
ggplot(data, aes(x = factor(topna_sezona), y = data_pm100)) +
  geom_boxplot() +
  labs(title = expression("Rozdíl koncentrací " * PM[10] * " podle topné sezóny"),
       x = "Topná sezóna (0 = ne, 1 = ano)", y = expression(PM[10]~(µg/m^3)))

ggplot(data, aes(x = factor(topna_sezona), y = data_no2)) +
  geom_boxplot() +
  labs(title = expression("Rozdíl koncentrací " * NO[2] * " podle topné sezóny"),
       x = "Topná sezóna (0 = ne, 1 = ano)", y = expression(NO[2]~(µg/m^3)))


# svatky a prazdniny vs bezne dny
ggplot(data, aes(x = factor(statni_svatky), y = valid_speed_count)) +
  geom_boxplot() +
  labs(title = "Počet vozidel během státních svátků vs. běžných dní",
       x = "Státní svátek (0 = ne, 1 = ano)", y = "Počet vozidel za hodinu")

ggplot(data, aes(x = factor(letni_prazdniny), y = valid_speed_count)) +
  geom_boxplot() +
  labs(title = "Počet vozidel během letních prázdnin vs. běžných dní",
       x = "Letní prázdniny (0 = ne, 1 = ano)", y = "Počet vozidel za hodinu")

### tyden vs vikend
# Počet vozidel podle dne v týdnu (pracovní vs. víkend)
ggplot(data, aes(x = factor(vsedni_den), y = valid_speed_count)) +
  geom_boxplot() +
  labs(title = "Počet vozidel: pracovní den vs. víkend",
       x = "Pracovní den (0 = víkend, 1 = pracovní den)",
       y = "Počet vozidel za hodinu")

# PM10 podle dne v týdnu
ggplot(data, aes(x = factor(vsedni_den), y = data_pm100)) +
  geom_boxplot() +
  labs(title = expression("Koncentrace " * PM[10] * ": pracovní den vs. víkend"),
       x = "Pracovní den (0 = víkend, 1 = pracovní den)",
       y = expression(PM[10]~(µg/m^3)))

# NO₂ podle dne v týdnu
ggplot(data, aes(x = factor(vsedni_den), y = data_no2)) +
  geom_boxplot() +
  labs(title = expression("Koncentrace " * NO[2] * ": pracovní den vs. víkend"),
       x = "Pracovní den (0 = víkend, 1 = pracovní den)",
       y = expression(NO[2]~(µg/m^3)))


