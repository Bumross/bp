# Explorace

data_hod <- data %>%
  arrange(cas) %>%
  drop_na(valid_speed_count)

ts_hod <- ts(data_hod$valid_speed_count, frequency = 24)

dekompozice_hod <- stl(ts_hod, s.window = "periodic")
plot(dekompozice_hod, main = "STL dekompozice hodinového počtu projetých vozidel s periodou 24 hodin")



#######
#
data_denni$den_v_tydnu <- as.factor(data_denni$den_v_tydnu)
data_denni$velke_svatky <- as.factor(data_denni$velke_svatky)
data_denni$letni_prazdniny <- as.factor(data_denni$letni_prazdniny)
data_denni$mesic <- as.factor(month(data_denni$datum))

# Lineární model
model_denni <- lm(valid_speed_count ~ den_v_tydnu + velke_svatky + letni_prazdniny + mesic, 
                  data = data_denni)

# Výpis výsledků
summary(model_denni)
