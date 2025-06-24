indexy <- 4400:4700
vetsi_nez_25 <- data$data_pm100[indexy] > 25

datumy_nad_25 <- data$datum[indexy][vetsi_nez_25]

print(datumy_nad_25) # od 5.7 do 8.7 kazdy den cca 15-20 hodnot

data$data_pm100[indexy[vetsi_nez_25]] <- NA

saveRDS(data, file = "~\\bo_data_analysis\\data\\data_procesovane\\spojena_data_upravena.rds")


# na teto bazi jsem upravil i data_denni :-)

plot(data_denni$data_pm100)
