library(tidyverse)
library(lubridate)
library(ggthemes)
library(viridis)
library(patchwork)
library(scales)

# vyvoj v case    

data %>%
  ggplot(aes(x = cas, y = valid_speed_count)) +
  geom_line(color = "black", alpha = 0.6) +
  labs(
    title = "Vývoj počtu projetých vozidel v čase",
    x = "Datum a čas",
    y = "Počet aut za hodinu"
  ) +
  theme_minimal() +
  scale_x_datetime(date_labels = "%d/%m/%Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


data %>%
  ggplot(aes(x = cas, y = valid_speed_count)) +
  geom_line(color = "black", alpha = 0.6) +
  labs(
    title = "Vývoj počtu projetých vozidel v čase",
    x = "Datum a čas",
    y = "Počet aut za hodinu"
  ) +
  theme_minimal() +
  scale_x_datetime(
    date_labels = "%d/%m/%Y",
    date_breaks = "1 month",
    limits = c(as.POSIXct("2024-10-01"), NA)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# denni prumer
denni_data <- data %>%
  group_by(datum) %>%
  summarise(denni_prumer = mean(valid_speed_count, na.rm = TRUE), .groups = "drop")

denni_data_trimmed <- denni_data %>%
  slice_head(n = nrow(.) - 3)

ggplot(denni_data_trimmed, aes(x = datum, y = denni_prumer)) +
  geom_line(color = "black") +
  labs(
    title = "Denní průměr počtu projetých vozidel",
    x = "Datum",
    y = "Průměrný počet aut"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%d/%m/%Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# stl
data_hod <- data %>%
  arrange(cas) %>%
  drop_na(valid_speed_count)

ts_hod <- ts(data_denni$valid_speed_count, frequency = 7)

dekompozice_hod <- stl(ts_hod, s.window = "periodic")
plot(dekompozice_hod, main = "STL dekompozice hodinového počtu projetých vozidel s periodou 168 hodin")


ts_hod_multi <- msts(data_hod$valid_speed_count, seasonal.periods = c(24, 168))

dekompozice_multi <- mstl(ts_hod_multi)
autoplot(dekompozice_multi) + theme_minimal()

# autokorelacni a sezooni
ts_hod <- ts(data_hod$valid_speed_count, frequency = 24)  # nebo 168 pro týdenní

ggAcf(ts_hod, lag.max = 168) +
  ggtitle("ACF: Autokorelace počtu projetých aut") +
  theme_minimal()

ggPacf(ts_hod, lag.max = 168) +
  ggtitle("PACF: Částečná autokorelace počtu projetých aut") +
  theme_minimal()







# tydenni vzorce
data_ts <- ts(data$valid_speed_count, frequency = 24 * 7)

ggseasonplot(data_ts, season.labels = TRUE, col = "black") +
  labs(title = "Sezónní vzorce v rámci týdne", x = "Den v týdnu", y = "Počet aut") +
  theme_minimal()




# auta za hodinu
data %>%
  mutate(
    den_v_tydnu = wday(cas, label = TRUE, week_start = 1),
    den_v_tydnu = fct_recode(den_v_tydnu,
                             "Po" = "Mon",
                             "Ut" = "Tue",
                             "St" = "Wed",
                             "Ct" = "Thu",
                             "Pa" = "Fri",
                             "So" = "Sat",
                             "Ne" = "Sun"
    )
  ) %>%
  ggplot(aes(x = den_v_tydnu, y = valid_speed_count)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(
    title = "Rozložení počtu vozidel podle dne v týdnu",
    x = "Den v týdnu",
    y = "Počet aut"
  ) +
  theme_minimal()






# podle mesice
data %>%
  mutate(mesic = month(cas)) %>%
  ggplot(aes(x = factor(mesic), y = valid_speed_count)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(
    title = "Rozložení počtu aut podle měsíce",
    x = "Měsíc",
    y = "Počet aut"
  ) +
  theme_minimal()




# heatmapa
data %>%
  mutate(
    den_v_tydnu = wday(cas, label = TRUE, week_start = 1),
    den_v_tydnu = fct_recode(den_v_tydnu,
                             "Po" = "Mon",
                             "Ut" = "Tue",
                             "St" = "Wed",
                             "Ct" = "Thu",
                             "Pa" = "Fri",
                             "So" = "Sat",
                             "Ne" = "Sun"),
    hodina = hour(cas)
  ) %>%
  group_by(den_v_tydnu, hodina) %>%
  summarise(prumer = mean(valid_speed_count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = hodina, y = fct_rev(den_v_tydnu), fill = prumer)) +
  geom_tile(color = "white") +
  scale_fill_gradient(name = "Průměr", low = "grey90", high = "black") +
  labs(
    title = "Průměrný počet aut podle hodiny a dne v týdnu",
    x = "Hodina",
    y = "Den v týdnu"
  ) +
  theme_minimal()


################################################################################


df_fitted <- model_hodinovy %>%
  augment() %>%
  select(cas, .fitted)


df_real <- data_ts_hodinove %>%
  select(cas, valid_speed_count, letni_prazdniny, velke_svatky)


df_test <- forecast_test %>%
  as_tibble() %>%
  select(cas, pred_30prct = .mean)

df_forecast <- forecast_hodinovy_fixed %>%
  as_tibble() %>%
  select(cas, pred_14dni = pred, lower_14dni = lower, upper_14dni = upper)



last_time <- max(data_ts_hodinove$cas)
future_data <- tibble(
  cas = seq(from = last_time + hours(1), by = "1 hour", length.out = 24 * 14),
  velke_svatky = factor(0, levels = c(0, 1)),
  letni_prazdniny = factor(0, levels = c(0, 1))
)

future_data_extended <- future_data %>%
  mutate(valid_speed_count = NA_real_)

df_real_extended <- data_ts_hodinove %>%
  select(cas, valid_speed_count, letni_prazdniny, velke_svatky) %>%
  bind_rows(future_data_extended)


data_merged <- df_real_extended %>%
  left_join(df_fitted, by = "cas") %>%
  left_join(df_test, by = "cas") %>%
  left_join(df_forecast, by = "cas")

data_merged_clean <- data_merged %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(. < 0, 0, .)
  ))


write.csv(data_merged_clean, "auta_hodinova.csv", row.names = FALSE)



###########################################################################
# auta denni

## fitovani celeho
  

model_denni <- data_ts_denni %>%
  model(
    sarima7 = ARIMA(
      valid_speed_count ~ 
        den_v_tydnu + velke_svatky + letni_prazdniny + pdq(1, 0, 1)
    )
  )

fitted_all <- model_denni %>%
  fitted() %>%
  as_tibble()

fitted_all <- data_ts_denni %>%
  as_tibble() %>%
  select(datum, valid_speed_count, velke_svatky) %>%
  left_join(fitted_all, by = "datum") %>%
  mutate(
    fitted_corrected = .fitted 
  )



n_total <- nrow(data_ts_denni)
n_train <- round(n_total * 0.7)

data_test <- data_ts_denni %>%
  slice((n_train + 1):n_total) %>%
  as_tibble()


n_total <- nrow(data_ts_denni)
n_train <- round(n_total * 0.7)

data_train <- data_ts_denni %>% slice(1:n_train)
data_test <- data_ts_denni %>% slice((n_train + 1):n_total)

model_train <- data_train %>%
  model(
    ARIMA(valid_speed_count ~ den_v_tydnu + letni_prazdniny 
          + pdq(0,0,2)
          + PDQ(2,0,1,7))
  )


forecast_test <- model_train %>%
  forecast(new_data = data_test)

forecast_test_tbl <- forecast_test %>%
  as_tibble() %>%
  mutate(
    velke_svatky = as.numeric(as.character(velke_svatky)),
    predikce_test = if_else(velke_svatky == 1, .mean + vliv_svatku, .mean)
  ) %>%
  select(datum, predikce_test)


### predpoved dopredu

forecast_14d_data <- tibble(
  datum = seq(max(data_ts_denni$datum) + 1, by = "1 day", length.out = 14)
) %>%
  mutate(
    den_v_tydnu = wday(datum, week_start = 1),
    den_v_tydnu = factor(den_v_tydnu, levels = 1:7),
    velke_svatky = factor(0, levels = c(0, 1)),
    letni_prazdniny = factor(0, levels = c(0, 1))
  ) %>%
  select(datum, den_v_tydnu, velke_svatky, letni_prazdniny) %>%  # jen tyto čtyři sloupce
  as_tsibble(index = datum)


# Predikce na 14 dní
forecast_14d <- forecast(model_denni$sarima7[[1]], new_data = forecast_14d_data) %>%
  as_tibble() %>%
  select(datum, forecast_14d = .mean)


dataframe_final <- fitted_all %>%
  left_join(forecast_test_tbl, by = "datum") %>%
  left_join(forecast_14d, by = "datum") %>%
  select(
    datum,
    valid_speed_count,
    fitted = fitted_corrected,
    predikce = predikce_test,
    forecast_14d
  )

future_rows <- forecast_14d %>%
  mutate(
    valid_speed_count = NA_real_,
    fitted = NA_real_,
    predikce = NA_real_
  ) %>%
  select(datum, valid_speed_count, fitted, predikce, forecast_14d)

dataframe_final <- dataframe_final %>%
  bind_rows(future_rows)


regresory_historie <- data_ts_denni %>%
  as_tibble() %>%
  select(datum, den_v_tydnu, velke_svatky, letni_prazdniny)

regresory_budoucnost <- forecast_14d_data %>%
  as_tibble() %>%
  select(datum, den_v_tydnu, velke_svatky, letni_prazdniny)

regresory_all <- bind_rows(regresory_historie, regresory_budoucnost)


dataframe_final <- dataframe_final %>%
  left_join(regresory_all, by = "datum") %>%
  relocate(den_v_tydnu, velke_svatky, letni_prazdniny, .after = datum)


write.csv(dataframe_final, "auta_denni.csv", row.names = FALSE)

 #################################xx

forecast_14d_full <- forecast(model_denni$sarima7[[1]], new_data = forecast_14d_data)

autoplot(forecast_14d_full, data = data_ts_denni) +
  labs(
    title = "Predikce počtu vozidel na 14 dní dopředu",
    x = "Datum",
    y = "Počet projetých vozidel"
  ) +
  scale_x_date(
    date_labels = "%d.%m.%y",
    date_breaks = "14 days"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


results_test <- forecast_test_tbl %>%
  left_join(data_test %>% select(datum, valid_speed_count), by = "datum")

# Vykreslení
ggplot(results_test, aes(x = datum)) +
  geom_line(aes(y = valid_speed_count, color = "Skutečnost")) +
  geom_line(aes(y = predikce_test, color = "Predikce")) +
  labs(
    title = "Predikce vs. skutečnost na testovacích datech",
    x = "Datum",
    y = "Počet projetých aut",
    color = ""
  ) +
  scale_color_manual(values = c("Skutečnost" = "black", "Predikce" = "blue")) +
  scale_x_date(date_labels = "%d.%m.%y", date_breaks = "5 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#################################################################################
# Vizualizace
