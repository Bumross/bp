library(readr)
library(dplyr)
library(ggplot2)
library(GGally)


#################
# topna sezona ma velky vliv na no2 a pm100
# proto rozdeluji zavislosti zvlast na tyto dve obdobi a hledam zavislosti zvlast


vars <- c("valid_speed_count", "data_pm100", "data_no2", "data_temp1",
          "data_hum1", "data_pressure", "data_windSpeed", "data_volumeMm")




topna <- data %>%
  filter(topna_sezona == 1) %>%
  select(all_of(vars)) %>%
  na.omit()

netopna <- data %>%
  filter(topna_sezona == 0) %>%
  select(all_of(vars)) %>%
  na.omit()


nove_nazvy <- c(
  "Počet vozidel",          # valid_speed_count
  "PM10",              # data_pm100
  "NO2",                # data_no2
  "Teplota",            # data_temp1
  "Vlhkost",            # data_hum1
  "Tlak",               # data_pressure
  "Rychlost větru",     # data_windSpeed
  "Srážky"              # data_volumeMm
)

colnames(topna) <- nove_nazvy
colnames(netopna) <- nove_nazvy

library(psych)
corr.test(netopna)
corr.test(topna)

# Vykreslení: Topná sezóna
ggpairs(topna, title = "Korelační matice s bodovými grafy – Topná sezóna")

# Vykreslení: Netopná sezóna
ggpairs(netopna, title = "Korelační matice s bodovými grafy – Netopná sezóna")


###########################################################
# vysokou miru korelace ma i tlak i teplota
# proto se tlak nezda byt dobry v modelu, protoze je silne korelovan





############
# pro denni data:
vars <- c("valid_speed_count", "data_pm100", "data_no2", "prumerna_teplota_dne",
          "data_hum1", "data_pressure", "data_windSpeed", "data_volumeMm")


topna <- data_denni %>%
  filter(topna_sezona == 1) %>%
  select(all_of(vars)) %>%
  na.omit()

netopna <- data_denni %>%
  filter(topna_sezona == 0) %>%
  select(all_of(vars)) %>%
  na.omit()


nove_nazvy <- c(
  "Počet vozidel",     # valid_speed_count
  "PM10",              # data_pm100
  "NO2",               # data_no2
  "Teplota",           # data_temp1
  "Vlhkost",           # data_hum1
  "Tlak",              # data_pressure
  "Rychlost větru",    # data_windSpeed
  "Srážky"             # data_volumeMm
)

colnames(topna) <- nove_nazvy
colnames(netopna) <- nove_nazvy


corr.test(netopna)
corr.test(topna)

ggpairs(topna, title = "Topná sezóna – korelační matice s bodovými grafy")

ggpairs(netopna, title = "Netopná sezóna – korelační matice s bodovými grafy")


##
# tady mame oproti hodinovemu rozlozeni
# kladny vliv teploty
# kladny vliv vozidel