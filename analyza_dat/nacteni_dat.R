meteo_path <- "~\\bo_data_analysis\\data\\data_procesovane\\meteo.csv"
cars_path <- "~\\bo_data_analysis\\data\\data_procesovane\\auta.csv"
ovzdusi_path <- "~\\bo_data_analysis\\data\\data_procesovane\\polution.csv"

meteo <- read.csv(meteo_path)
meteo <- meteo %>%
  mutate(
    hour = as.character(hour),
    hour = if_else(
      nchar(hour) == 10,
      paste0(hour, " 00:00:00"),
      hour
    )
  )

meteo <- meteo %>%
  rename(cas = hour)


polution <- read.csv(ovzdusi_path)
polution <- polution %>%
  mutate(
    hour = as.character(hour),
    hour = if_else(
      nchar(hour) == 10,
      paste0(hour, " 00:00:00"),
      hour
    )
  )

polution <- polution %>%
  rename(cas = hour)


cars <- read.csv(cars_path)
cars <- cars %>%
  mutate(
    hour = as.character(hour),
    hour = if_else(
      nchar(hour) == 10,
      paste0(hour, " 00:00:00"),
      hour
    )
  )

cars <- cars %>%
  rename(cas = hour)
