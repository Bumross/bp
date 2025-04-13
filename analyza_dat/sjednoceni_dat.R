spojena_data <- cars %>%
  left_join(polution %>% select(cas, data_pm100, data_no2), by = "cas") %>%
  left_join(meteo, by = "cas")



### kontrola spojeni
spojena_data <- spojena_data %>%
  mutate(cas = as.POSIXct(cas, format = "%Y-%m-%d %H:%M:%S"))

vsechny_hodiny <- data.frame(
  cas = seq(
    from = min(spojena_data$cas, na.rm = TRUE),
    to   = max(spojena_data$cas, na.rm = TRUE),
    by   = "hour"
  )
)

spojena_data <- vsechny_hodiny %>%
  left_join(spojena_data, by = "cas")

spojena_data %>%
  filter(if_any(everything(), is.na)) %>%
  View()

spojena_data <- spojena_data %>%
  filter(cas <= as.POSIXct("2025-01-15 23:59:59"))

spojena_data %>%
  filter(if_any(everything(), is.na)) %>%
  View()

#write.csv(spojena_data, "spojena_data.csv")
