target_vars <- c("data_pm100", "data_no2")
predictors <- c("valid_speed_count", "data_temp1", "data_hum1", "data_pressure",
                "data_windSpeed", "data_windImpact", "data_volumeMm")


data <- read_csv("data_kompletni_hodinova.csv")

# 📋 Proměnné
target_vars <- c("data_pm100", "data_no2")
predictors <- c("valid_speed_count", "data_temp1", "data_hum1", "data_pressure",
                "data_windSpeed", "data_windImpact", "data_volumeMm")

# 📁 Vytvoř složku pro grafy
if (!dir.exists("grafy_ccf")) dir.create("grafy_ccf")

for (target in target_vars) {
  for (season in c(1, 0)) {
    sezona_nazev <- ifelse(season == 1, "Topná sezóna", "Netopná sezóna")
    sezona_file <- ifelse(season == 1, "topna", "netopna")
    file_name <- paste0("grafy_ccf/ccf_", target, "_", sezona_file, ".png")
    
    # ⬇️ Vyber data a odstraň NA
    df <- data %>%
      filter(topna_sezona == season) %>%
      select(all_of(c(target, predictors))) %>%
      na.omit()
    
    # 📊 Výstup do PNG
    png(file_name, width = 1600, height = 1200, res = 200)
    layout(matrix(c(1, 2:9), nrow = 3, byrow = TRUE))  # 1. řádek pro název
    par(mar = c(0, 0, 2, 0))  # pro název
    
    # 1. Panel: hlavní název
    plot.new()
    title(main = paste(toupper(target), "–", sezona_nazev), cex.main = 1.2)
    
    # Následující panely pro CCF grafy
    par(mar = c(3, 3, 3, 1))  # okraje pro grafy
    
    for (pred in predictors) {
      ccf(df[[pred]], df[[target]],
          lag.max = 12,
          main = paste(pred, "vs", target),
          ylab = "Korelace", xlab = "Lag")
    }
    
    dev.off()
  }
}



# Proměnné bez tlaku a směru větru
vars <- c("valid_speed_count", "data_temp1", "data_hum1", "data_windSpeed", "data_volumeMm")
nove_nazvy <- c("Počet vozidel", "Teplota", "Vlhkost", "Rychlost větru", "Srážky")

# Rozdělení dat
topna <- data %>% filter(topna_sezona == 1) %>% select(data_pm100, all_of(vars)) %>% drop_na()
netopna <- data %>% filter(topna_sezona == 0) %>% select(data_pm100, all_of(vars)) %>% drop_na()

# Funkce pro vykreslení CCF
plot_ccf_group <- function(df, target, label_sezona, readable_labels, max_lag = 12) {
  plots <- map2(vars, readable_labels, function(v, nice_name) {
    ggCcf(df[[target]], df[[v]], lag.max = max_lag, plot = FALSE) %>%
      {tibble(lag = .$lag, ccf = .$acf)} %>%
      ggplot(aes(x = lag, y = ccf)) +
      geom_col(fill = "gray5") +
      geom_hline(yintercept = 0, color = "gray30") +
      geom_hline(yintercept = c(0.2, -0.2), linetype = "dashed", color = "black") +
      labs(title = nice_name, x = "Zpoždění (v hodinách)", y = "Kroskorelace") +
      theme_minimal()
  })
  
  grid.arrange(grobs = plots, ncol = 2, top = paste("CCF pro PM10 –", label_sezona))
}

# Vykreslení
plot_ccf_group(topna, "data_pm100", "topná sezóna", nove_nazvy)
plot_ccf_group(netopna, "data_pm100", "netopná sezóna", nove_nazvy)



vars <- c("valid_speed_count", "data_temp1", "data_hum1", "data_windSpeed", "data_volumeMm")
nove_nazvy <- c("Počet vozidel", "Teplota", "Vlhkost", "Rychlost větru", "Srážky")




# Rozdělení dat
topna_no2 <- data %>%
  filter(topna_sezona == 1) %>%
  select(data_no2, all_of(vars)) %>%
  drop_na()

netopna_no2 <- data %>%
  filter(topna_sezona == 0) %>%
  select(data_no2, all_of(vars)) %>%
  drop_na()





# Funkce pro vykreslení CCF grafů
plot_ccf_group <- function(df, target, label_sezona, readable_labels, max_lag = 12) {
  plots <- map2(vars, readable_labels, function(v, nice_name) {
    ccf_data <- ccf(df[[target]], df[[v]], lag.max = max_lag, plot = FALSE)
    tibble(lag = ccf_data$lag, ccf = ccf_data$acf) %>%
      ggplot(aes(x = lag, y = ccf)) +
      geom_col(fill = "gray5") +
      geom_hline(yintercept = 0, color = "gray30") +
      geom_hline(yintercept = c(0.2, -0.2), linetype = "dashed", color = "black") +
      labs(title = nice_name, x = "Zpoždění (v hodinách)", y = "Kroskorelace") +
      theme_minimal()
  })
  
  grid.arrange(grobs = plots, ncol = 2, top = paste("CCF pro NO2 –", label_sezona))
}






# Vykreslení grafů pro NO2
plot_ccf_group(topna_no2, "data_no2", "topná sezóna", nove_nazvy)
plot_ccf_group(netopna_no2, "data_no2", "netopná sezóna", nove_nazvy)