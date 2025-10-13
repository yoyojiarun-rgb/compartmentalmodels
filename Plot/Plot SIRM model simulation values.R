library(readxl)
sir_data <- read_excel("C:/Users/yoyoj/Downloads/data_SIRM.xlsx")
# Plot S line first
plot(sir_data$Day, sir_data$S, type = "l", col = "blue", lwd = 2,
     xlab = "Day", ylab = "Population", 
     main = "SIRM Model Over Time", ylim = c(0, max(sir_data$S)))


lines(sir_data$Day, sir_data$I, col = "red", lwd = 2)
lines(sir_data$Day, sir_data$R, col = "green", lwd = 2)
lines(sir_data$Day, sir_data$M, col = "purple", lwd = 2)


legend("left", legend = c("Susceptible (S)", "Infected (I)", "Removed (R)", "Immune (M)"),
       col = c("blue", "red", "green","purple"), lwd = 2)

