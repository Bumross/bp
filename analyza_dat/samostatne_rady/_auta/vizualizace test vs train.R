accuracy(forecast_test, test_data)
fc <- forecast(model_train, new_data = data_test)
accuracy(fc, data_test)





ggplot(results, aes(x = cas)) +
  geom_line(aes(y = valid_speed_count.y, color = "Skutečnost"), linewidth = 0.7) +
  geom_line(aes(y = .mean, color = "Predikce"), linewidth = 0.7) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce" = "blue")) +
  labs(
    title = "Predikce počtu projetých vozidel vs. skutečnost",
    x = "Datum",
    y = "Počet projetých vozidel",
    color = "Typ dat"
  ) +
  scale_x_datetime(date_labels = "%d.%m.%y", date_breaks = "7 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))