library(lubridate)
library(dplyr)


# úprava času na naše časové pásmo
data$cas <- with_tz(dmy_hm(data$cas, tz = "UTC"), tzone = "Europe/Prague")
data$cas <- as.POSIXct(data$cas, tz = "Europe/Prague")

data <- data %>%
  mutate(den_v_tydnu = as.numeric(format(cas, tz = "Europe/Prague", "%u")))

data[] <- lapply(data, function(x) {
  if (is.numeric(x)) round(x, 2) else x
})


# pridani promenne oznacujici mesic
data$month <- format(as.Date(data$cas, tz = "Europe/Prague"), "%m/%Y")

# pridani ukazetele tretiny dne (pro tydenni sezonnosti)
hodiny <- as.integer(format(as.POSIXct(data$cas, tz = "Europe/Prague"), "%H"))
data$tretina_dne <- cut(hodiny,
                        breaks = c(-1, 7, 15, 23),
                        labels = c(1, 2, 3),
                        right = TRUE)

# pridani letnich prazdnin
data$letni_prazdniny <- ifelse(format(as.Date(data$cas, tz = "Europe/Prague"), "%m") %in% c("07", "08"), 1, 0)

# topna sezona
data$topna_sezona <- ifelse(format(as.Date(data$cas, tz = "Europe/Prague"), "%m") %in% c("10", "11", "12", "01", "02", "03", "04"), 1, 0)

# pridani svatku
statni_svatky <- as.Date(c("2024-01-01", "2024-04-01", "2024-05-01", "2024-05-08",
                           "2024-07-05", "2024-07-06", "2024-09-28", "2024-10-28",
                           "2024-11-17", "2024-12-24", "2024-12-25", "2024-12-26"))
data$statni_svatky <- ifelse(as.Date(data$cas, tz = "Europe/Prague") %in% statni_svatky, 1, 0)

# pridani prumerne teploty (7 + 2*14 + 21)
data <- data %>%
  mutate(datum = as.Date(cas, tz = "Europe/Prague")) %>%
  group_by(datum) %>%
  mutate(prumerna_teplota_dne = {
    t7  <- data_temp1[format(as.POSIXct(cas, tz = "Europe/Prague"), "%H") == "07"]
    t14 <- data_temp1[format(as.POSIXct(cas, tz = "Europe/Prague"), "%H") == "14"]
    t21 <- data_temp1[format(as.POSIXct(cas, tz = "Europe/Prague"), "%H") == "21"]
    
    if (length(t7) == 0 | length(t14) == 0 | length(t21) == 0) {
      NA
    } else {
      rep((t7[1] + t14[1] + 2 * t21[1]) / 4, n())
    }
  }) %>%
  ungroup()





# winsorizace pm10 a no2 (pro cervenec, kde se vyskytuji velmi odlehla pozorovani)
data$data_pm100[data$data_pm100 > 150] <- 150

data$data_no2[data$data_no2 > 125] <- 125





saveRDS(data, "data/data_procesovane/spojena_data_upravena.rds")

