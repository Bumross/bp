
# data ktere se oriznou
actual_data <- data_ts_denni %>%
  as_tibble() %>%
  tail(30) %>%
  select(datum, y = valid_speed_count) %>%
  mutate(typ = "Skutečnost")

# vypocitani reziduals
resid_sd <- model_denni %>% 
  residuals() %>% 
  as_tibble() %>% 
  pull(.resid) %>% 
  sd(na.rm = TRUE)

# prirazeni predikce, lower, upper
forecast_denni <- forecast_denni %>%
  mutate(
    pred = pmax(.mean, 0),
    lower = pmax(.mean - 1.96 * resid_sd, 0),
    upper = .mean + 1.96 * resid_sd
  )

# priprava na plot
forecast_plot <- forecast_denni %>%
  as_tibble() %>%
  select(datum, pred, lower, upper) %>%
  mutate(
    typ = "Predikce",
    y = pred
  )

actual_data <- actual_data %>%
  select(datum, y) %>%
  mutate(
    typ = "Skutečnost",
    y = y
  )


# kombinace
combined_data <- bind_rows(actual_data, forecast_plot)





# graf
ggplot(combined_data, aes(x = datum)) +
  geom_ribbon(
    data = combined_data %>% filter(typ == "Predikce"),
    aes(ymin = lower, ymax = upper),
    fill = "lightblue", alpha = 0.4
  ) +
  geom_line(aes(y = y, color = typ), linewidth = 0.7) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce" = "blue")) +
  labs(
    title = "Predikce počtu projetých vozidel na 30 dní",
    x = "Datum",
    y = "Počet projetých vozidel",
    color = "Typ dat"
  ) +
  scale_x_date(date_labels = "%d.%m.", date_breaks = "3 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
