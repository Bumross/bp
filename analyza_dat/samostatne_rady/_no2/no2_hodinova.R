library(dplyr)
library(lubridate)
library(ggplot2)
library(tibble)




data <- data %>%
  mutate(
    hodina = hour(cas),
    den_v_tydnu = wday(cas, label = TRUE, week_start = 1),
    mesic = month(cas, label = TRUE),
    log_no2 = log(data_no2),
    den_v_roce = yday(cas),
    sin_day = sin(2 * pi * hodina / 24),
    cos_day = cos(2 * pi * hodina / 24),
    sin_year = sin(2 * pi * den_v_roce / 365),
    cos_year = cos(2 * pi * den_v_roce / 365)
  )

g1 <- ggplot(data, aes(x = data_no2)) +
  geom_histogram(bins = 60, fill = "skyblue", color = "black") +
  labs(title = "Histogram koncentrace NO2", x = "NO2", y = "Počet")

g2 <- ggplot(data, aes(sample = data_no2)) +
  stat_qq() + stat_qq_line(color = "red") +
  labs(title = "Q-Q plot NO2")

g3 <- ggplot(data, aes(x = log_no2)) +
  geom_histogram(bins = 60, fill = "lightgreen", color = "black") +
  labs(title = "Histogram log(NO2)", x = "log(NO2)")

g4 <- ggplot(data, aes(sample = log_no2)) +
  stat_qq() + stat_qq_line(color = "red") +
  labs(title = "Q-Q plot log(NO2)")

ggarrange(g1, g2, g3, g4, ncol = 2, nrow = 2)



ggplot(data_hodinova, aes(x = cas, y = no2)) +
  geom_line() +
  labs(
    title = "Hodinová koncentrace NO2",
    x = "Datum",
    y = "NO2 [ug/m3]"
  ) +
  theme_minimal() +
  scale_x_datetime(date_labels = "%m/%Y", date_breaks = "1 month")
  





ggplot(data, aes(x = as.factor(hodina), y = data_no2)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "NO₂ podle hodiny dne", x = "Hodina", y = "NO₂ [µg/m³]")


ggplot(data, aes(x = den_v_tydnu, y = data_no2)) +
  geom_boxplot(fill = "lightpink") +
  labs(title = "NO₂ podle dne v týdnu", x = "Den v týdnu", y = "NO₂ [µg/m³]")


ggplot(data, aes(x = mesic, y = data_no2)) +
  geom_boxplot(fill = "lightgray") +
  labs(title = "NO₂ podle měsíce", x = "Měsíc", y = "NO₂ [µg/m³]")


ggplot(data, aes(x = hodina, y = data_no2)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "darkred") +
  labs(title = "Denní sezónnost NO₂", x = "Hodina", y = "NO₂ [µg/m³]")


ggplot(data, aes(x = den_v_roce, y = data_no2)) +
  geom_point(alpha = 0.02) +
  geom_smooth(method = "loess", se = FALSE, color = "darkblue") +
  labs(title = "Roční sezónnost NO₂", x = "Den v roce", y = "NO₂ [µg/m³]")





###############################################################################
data_model <- data %>%
  mutate(
    hodina = hour(cas),
    den_v_tydnu = factor(wday(cas, label = TRUE, week_start = 1)),
    den_v_roce = yday(cas),
    sin_day = sin(2 * pi * hodina / 24),
    cos_day = cos(2 * pi * hodina / 24),
    sin_year = sin(2 * pi * den_v_roce / 365),
    cos_year = cos(2 * pi * den_v_roce / 365),
    log_no2 = log(data_no2)
  ) %>%
  filter(is.finite(log_no2))

model_lm <- lm(
  log_no2 ~ sin_day + cos_day + sin_year + cos_year + den_v_tydnu,
  data = data_model
)

summary(model_lm)
Anova(model_lm)






data_model <- data_model %>%
  mutate(pred_log = predict(model_lm),
         pred = exp(pred_log))  # zpětná transformace

# Graf
ggplot(data_model, aes(x = cas)) +
  geom_line(aes(y = data_no2), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred), color = "blue", alpha = 0.8) +
  labs(title = "Fit lineárního modelu na hodinových datech NO2", x = "Čas", y = "NO₂ [µg/m³]") +
  theme_minimal()




#####################
# predikce:
future_times <- tibble(
  cas = seq(
    from = max(data_model$cas) + hours(1),
    by = "hour",
    length.out = 90
  )
) %>%
  mutate(
    hodina = hour(cas),
    den_v_tydnu = factor(wday(cas, label = TRUE, week_start = 1),
                         levels = levels(data_model$den_v_tydnu)),
    den_v_roce = yday(cas),
    sin_day = sin(2 * pi * hodina / 24),
    cos_day = cos(2 * pi * hodina / 24),
    sin_year = sin(2 * pi * den_v_roce / 365),
    cos_year = cos(2 * pi * den_v_roce / 365)
  )

future_times <- future_times %>%
  mutate(
    pred_log = predict(model_lm, newdata = .),
    pred = exp(pred_log)
  )

data_plot <- data_model %>%
  select(cas, data_no2) %>%
  mutate(typ = "historie") %>%
  bind_rows(
    future_times %>% select(cas, data_no2 = pred) %>% mutate(typ = "predikce")
  )

ggplot(data_plot, aes(x = cas, y = data_no2, color = typ)) +
  geom_line(size = 0.4) +
  scale_color_manual(values = c("historie" = "black", "predikce" = "red")) +
  labs(
    title = "Predikce NO₂ na 90 hodin dopředu",
    x = "Čas",
    y = "NO₂ [µg/m³]",
    color = "Typ dat"
  ) +
  theme_minimal()

