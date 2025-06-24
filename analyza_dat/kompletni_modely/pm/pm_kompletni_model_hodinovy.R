library(tidyverse)
library(lubridate)
library(broom)
library(ggplot2)
library(ggpubr)
library(mgcv)

##### prvni pokus o model
# zkusil jsem postupne pridavat promenne:





data <- data %>%
  mutate(
    ln_pm100 = log(data_pm100),
    t = as.numeric(difftime(cas, min(cas), units = "hours")),
    t2 = t^2,
    lag_auta_1h = lag(valid_speed_count)
  )

model_vars <- c(
  "ln_pm100", "t", "t2", "valid_speed_count", "lag_auta_1h",
  "data_temp1", "data_hum1", "data_pressure", "data_volumeMm", "data_windSpeed"
)

data_topna <- data %>%
  filter(topna_sezona == 1) %>%
  select(all_of(model_vars)) %>%
  drop_na()

data_netopna <- data %>%
  filter(topna_sezona == 0) %>%
  select(all_of(model_vars)) %>%
  drop_na()















model_topna <- lm(ln_pm100 ~ valid_speed_count + lag_auta_1h +
                    data_temp1 + data_hum1 + data_pressure + data_volumeMm + data_windSpeed,
                  data = data_topna)

model_netopna <- lm(ln_pm100 ~ t + t2 + valid_speed_count + lag_auta_1h +
                      data_temp1 + data_hum1 + data_pressure + data_volumeMm + data_windSpeed,
                    data = data_netopna)

# SHRNUTÍ MODELŮ
summary(model_topna)
summary(model_netopna)

# DIAGNOSTIKA
par(mfrow = c(2, 2))
plot(model_topna, main = "Topná sezóna – diagnostika")
plot(model_netopna, main = "Netopná sezóna – diagnostika")
par(mfrow = c(1, 1))








# PREDIKCE
data_topna$pred_pm100 <- exp(predict(model_topna, newdata = data_topna))
data_topna$real_pm100 <- exp(data_topna$ln_pm100)

data_netopna$pred_pm100 <- exp(predict(model_netopna, newdata = data_netopna))
data_netopna$real_pm100 <- exp(data_netopna$ln_pm100)




# GRAFY – predikce vs. realita
g1 <- ggplot(data_topna, aes(x = 1:nrow(data_topna))) +
  geom_line(aes(y = real_pm100), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100), color = "blue", alpha = 0.7) +
  labs(title = "Topná sezóna – Predikce vs. realita", x = "Index", y = "PM100")

g2 <- ggplot(data_netopna, aes(x = 1:nrow(data_netopna))) +
  geom_line(aes(y = real_pm100), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100), color = "darkgreen", alpha = 0.7) +
  labs(title = "Netopná sezóna – Predikce vs. realita", x = "Index", y = "PM100")

ggarrange(g1, g2, ncol = 1)









############################################################################
# pouziti GAM modelu
data <- data %>%
  mutate(
    ln_pm100 = log(data_pm100),
    t = as.numeric(difftime(cas, min(cas), units = "hours")),
    t2 = t^2,
    lag_auta_1h = lag(valid_speed_count)
  )

model_vars <- c(
  "ln_pm100", "t", "t2", "valid_speed_count", "lag_auta_1h",
  "data_temp1", "data_hum1", "data_pressure", "data_volumeMm", "data_windSpeed"
)

data_topna <- data %>%
  filter(topna_sezona == 1) %>%
  select(all_of(model_vars), cas) %>%
  drop_na()

data_netopna <- data %>%
  filter(topna_sezona == 0) %>%
  select(all_of(model_vars), cas) %>%
  drop_na()

### 1️⃣ GAM MODELY (nelineární vztahy)

# Modely
gam_topna <- gam(ln_pm100 ~ s(valid_speed_count) + s(lag_auta_1h) +
                   s(data_temp1) + s(data_hum1) + s(data_pressure) +
                   s(data_volumeMm) + s(data_windSpeed),
                 data = data_topna)

gam_netopna <- gam(ln_pm100 ~ s(t) + s(valid_speed_count) + #s(lag_auta_1h) +
                     s(data_temp1) + s(data_hum1) + s(data_pressure) +
                     s(data_volumeMm) + s(data_windSpeed),
                   data = data_netopna)

summary(gam_topna)
summary(gam_netopna)



# jakoby fit
data_topna$pred_pm100_gam <- exp(predict(gam_topna, newdata = data_topna))
data_netopna$pred_pm100_gam <- exp(predict(gam_netopna, newdata = data_netopna))

g1 <- ggplot(data_topna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100_gam), color = "blue", alpha = 0.7) +
  labs(title = "Topná sezóna – GAM model", y = "PM100", x = NULL)

g2 <- ggplot(data_netopna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100_gam), color = "darkgreen", alpha = 0.7) +
  labs(title = "Netopná sezóna – GAM model", y = "PM100", x = NULL)

ggarrange(g1, g2, ncol = 1)



########################
# test vs train
data <- data %>%
  mutate(
    ln_pm100 = log(data_pm100),
    time_index = as.numeric(difftime(cas, min(cas), units = "hours")),
    time_index2 = time_index^2,
    lag_auta_1h = lag(valid_speed_count)
  ) %>%
  filter(!is.na(ln_pm100), !is.na(lag_auta_1h))

# 2. Výběr proměnných
model_vars <- c(
  "ln_pm100", "time_index", "time_index2", "valid_speed_count", "lag_auta_1h",
  "data_temp1", "data_hum1", "data_pressure", "data_volumeMm", "data_windSpeed"
)

# 3. Rozdělení na sezóny a výběr proměnných
data_topna <- data %>%
  filter(topna_sezona == 1) %>%
  select(all_of(model_vars), cas)

data_netopna <- data %>%
  filter(topna_sezona == 0) %>%
  select(all_of(model_vars), cas)

# 4. Trénovací a testovací sada (70:30)
train_topna <- data_topna %>% slice(1:floor(0.7 * n()))
test_topna <- data_topna %>% slice((floor(0.7 * n()) + 1):n())

train_netopna <- data_netopna %>% slice(1:floor(0.7 * n()))
test_netopna <- data_netopna %>% slice((floor(0.7 * n()) + 1):n())

train_topna <- train_topna %>% drop_na()
train_netopna <- train_netopna %>% drop_na()


# 5. GAM modely
gam_topna <- gam(ln_pm100 ~ s(valid_speed_count) + s(lag_auta_1h) +
                   s(data_temp1) + s(data_hum1) + s(data_pressure) +
                   s(data_volumeMm) + s(data_windSpeed),
                 data = train_topna)

gam_netopna <- gam(ln_pm100 ~ s(time_index) + s(valid_speed_count) +
                     s(data_temp1) + s(data_hum1) + s(data_pressure) +
                     s(data_volumeMm) + s(data_windSpeed),
                   data = train_netopna)

# 6. Predikce na testovacích datech
test_topna$pred_pm100_gam <- exp(predict(gam_topna, newdata = test_topna))
test_netopna$pred_pm100_gam <- exp(predict(gam_netopna, newdata = test_netopna))

# 7. Grafy – predikce vs realita
g1 <- ggplot(test_topna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.6) +
  geom_line(aes(y = pred_pm100_gam), color = "blue", alpha = 0.8) +
  labs(title = "Topná sezóna – GAM model (predikce na testovacích datech)", y = "PM100", x = NULL) +
  theme_minimal()

g2 <- ggplot(test_netopna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.6) +
  geom_line(aes(y = pred_pm100_gam), color = "darkgreen", alpha = 0.8) +
  labs(title = "Netopná sezóna – GAM model (predikce na testovacích datech)", y = "PM100", x = NULL) +
  theme_minimal()

ggarrange(g1, g2, ncol = 1)

### nejlepsi model GAM




##############################################################################
# tslm 
data <- data %>%
  filter(!is.na(cas)) %>%
  arrange(cas) %>%
  distinct(cas, .keep_all = TRUE) %>%
  mutate(
    ln_pm100 = log(data_pm100),
    t = as.numeric(difftime(cas, min(cas), units = "hours")),
    t2 = t^2
  )

data_ts <- data %>%
  select(cas, ln_pm100, valid_speed_count,
         data_temp1, data_hum1, data_pressure, data_volumeMm, data_windSpeed,
         topna_sezona, t, t2) %>%
  as_tsibble(index = cas)

# ==== 2. Rozdělení na topnou a netopnou sezónu ====
data_topna <- filter(data_ts, topna_sezona == 1)
data_netopna <- filter(data_ts, topna_sezona == 0)

# ==== 3. Modely na celá data ====
model_topna_all <- data_topna %>%
  model(
    tslm = TSLM(ln_pm100 ~ trend() + season("day") +
                  valid_speed_count +
                  data_temp1 + data_hum1 + data_pressure +
                  data_volumeMm + data_windSpeed)
  )

model_netopna_all <- data_netopna %>%
  model(
    tslm = TSLM(ln_pm100 ~ t + t2 + season("day") +
                  valid_speed_count +
                  data_temp1 + data_hum1 + data_pressure +
                  data_volumeMm + data_windSpeed)
  )

# ==== 4. Výpis modelů ====
report(model_topna_all)
report(model_netopna_all)

# ==== 5. Predikce na trénovací data ====
fitted_topna_all <- fitted(model_topna_all) %>% as_tibble()
fitted_netopna_all <- fitted(model_netopna_all) %>% as_tibble()

data_topna <- data_topna %>%
  mutate(pred_pm100_tslm = exp(fitted_topna_all$.fitted))

data_netopna <- data_netopna %>%
  mutate(pred_pm100_tslm = exp(fitted_netopna_all$.fitted))

# ==== 6. Grafy na trénovacích datech ====
g1 <- ggplot(data_topna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100_tslm), color = "blue", alpha = 0.7) +
  labs(title = "Topná sezóna – TSLM model (fit)", y = "PM100", x = NULL)

g2 <- ggplot(data_netopna, aes(x = cas)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100_tslm), color = "darkgreen", alpha = 0.7) +
  labs(title = "Netopná sezóna – TSLM model (fit)", y = "PM100", x = NULL)

g1 / g2

# test vs train
# 
data <- data %>%
  filter(!is.na(cas)) %>%
  arrange(cas) %>%
  distinct(cas, .keep_all = TRUE) %>%
  mutate(
    ln_pm100 = log(data_pm100)
  )

data_ts <- data %>%
  select(cas, ln_pm100, valid_speed_count,
         data_temp1, data_hum1, data_pressure, data_volumeMm, data_windSpeed,
         topna_sezona) %>%
  as_tsibble(index = cas) %>%
  mutate(
    t = as.numeric(difftime(cas, min(cas), units = "hours")),
    t2 = t^2
  )

# ==== 2. Rozdělení na topnou a netopnou sezónu ====
data_topna <- filter(data_ts, topna_sezona == 1)
data_netopna <- filter(data_ts, topna_sezona == 0)

# ==== 3. Funkce pro split ====
split_ts_data <- function(data_season) {
  n <- nrow(data_season)
  split_point <- floor(0.7 * n)
  
  train_df <- data_season %>% slice(1:split_point)
  test_df <- data_season %>% slice((split_point + 1):n)
  
  list(
    train = as_tsibble(train_df, index = cas),
    test = as_tsibble(test_df, index = cas)
  )
}

topna_split <- split_ts_data(data_topna)
netopna_split <- split_ts_data(data_netopna)

# ==== 4. Modely ====

model_topna <- topna_split$train %>%
  model(
    tslm = TSLM(ln_pm100 ~ trend() + season("day") +
                  valid_speed_count +
                  data_temp1 + data_hum1 + data_pressure +
                  data_volumeMm + data_windSpeed)
  )

model_netopna <- netopna_split$train %>%
  model(
    tslm = TSLM(ln_pm100 ~ t + t2 + season("day") +
                  valid_speed_count +
                  data_temp1 + data_hum1 + data_pressure +
                  data_volumeMm + data_windSpeed)
  )

# ==== 5. Predikce na testovací data ====
forecast_topna <- forecast(model_topna, new_data = topna_split$test)
forecast_netopna <- forecast(model_netopna, new_data = netopna_split$test)

# ==== 6. Sloučení predikcí a dat ====
data_topna_forecasted <- topna_split$test %>%
  bind_cols(pred_pm100_tslm = exp(forecast_topna$.mean))

data_netopna_forecasted <- netopna_split$test %>%
  bind_cols(pred_pm100_tslm = exp(forecast_netopna$.mean))

# ==== 7. Grafy ====
g1 <- ggplot(data_topna_forecasted, aes(x = cas)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100_tslm), color = "blue", alpha = 0.7) +
  labs(title = "Topná sezóna – TSLM model (predikce na testovacích datech)", y = "PM100", x = NULL)

g2 <- ggplot(data_netopna_forecasted, aes(x = cas)) +
  geom_line(aes(y = exp(ln_pm100)), color = "black", alpha = 0.5) +
  geom_line(aes(y = pred_pm100_tslm), color = "darkgreen", alpha = 0.7) +
  labs(title = "Netopná sezóna – TSLM model (predikce na testovacích datech)", y = "PM100", x = NULL)

g1 / g2

