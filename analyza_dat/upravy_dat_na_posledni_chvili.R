# jsem blbec, změna času mi zduplikovala časovou hodnotu 27.10.2024 : 3:00 :-)
# a já na to zapomněl
data <- data %>%
  distinct(cas, .keep_all = TRUE)

saveRDS(data, "data/data_procesovane/spojena_data_upravena.rds")


# přidání další proměnné, která má pouze významné svátky

velke_svatky_dny <- as_date(c(
  "2024-01-01",       
  "2024-04-01",       
  "2024-10-28",       
  "2024-12-24",       
  "2024-12-25",       
  "2024-12-26",       
  "2025-01-01"
))

data <- data %>%
  mutate(
    velke_svatky = if_else(as_date(cas) %in% velke_svatky_dny, 1, 0)
  )

saveRDS(data, "data/data_procesovane/spojena_data_upravena.rds")
