data_denni$log_auta <- log(data_denni$valid_speed_count)  # +1 proti log(0)
data_denni$den <- yday(data_denni$datum)




model_lm_denni <- lm(
  log(data_no2) ~ prumerna_teplota_dne + as.factor(topna_sezona) + log_auta,
  data = data_denni
)

summary(model_lm_denni)
























data_denni_clean <- na.omit(data_denni)


model_gls2 <- gls(
  data_no2 ~ den + I(den^2) + prumerna_teplota_dne + log_auta,
  correlation = corAR1(form = ~ den),
  data = data_denni
)

summary(model_gls2)