data_ts_temp <- data %>%
  select(cas, data_temp1) %>%
  mutate(
    hodina = hour(cas),
    den_v_roce = yday(cas),
    den_v_tydnu = wday(cas, label = TRUE), # pondělí = 1
    mesic = factor(month(cas), levels = 1:12),
    datum = as_date(cas)
  ) %>%
  as_tsibble(index = cas) %>%
  fill_gaps()


ggplot(data, aes(x = datum, y = prumerna_teplota_dne)) +
  geom_line(color = "black") +
  labs(
    x = "Datum",
    y = "Teplota (°C)",
    title = "Průměrná denní teplota během roku"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.minor = element_blank()
  )  + scale_x_date(
    date_labels = "%d.%m.%y",
    date_breaks = "1 month")




