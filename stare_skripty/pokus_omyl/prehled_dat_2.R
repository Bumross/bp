## zavislost pm na mesici


merged_data$hour <- as.POSIXct(merged_data$hour)

merged_data$month <- months(merged_data$hour)

merged_data$month <- factor(merged_data$month,
                            levels = c("January", "February", "March", "April", "May", "June",
                                       "July", "August", "September", "October", "November", "December")
)


model_month <- lm(total_pm ~ month, data = merged_data)


summary(model_month)


boxplot(total_pm ~ month, data = merged_data,
        main = "Rozložení koncentrace PM podle měsíců",
        xlab = "Měsíc", ylab = "Koncentrace PM", las = 2, col = "lightblue")
