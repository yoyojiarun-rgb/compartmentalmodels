library(readxl)
sir_data <- read_excel("C:/data_SVIRpartial.xlsx")
# Plot S line first
plot(sir_data$Day, sir_data$S, type = "l", col = "blue", lwd = 2,
     xlab = "Day", ylab = "Population", 
     main = "SVIR-partial Model Over Time", ylim = c(0, max(sir_data$S)))

lines(sir_data$Day, sir_data$V, col = "purple", lwd = 2)
lines(sir_data$Day, sir_data$I, col = "red", lwd = 2)
lines(sir_data$Day, sir_data$R, col = "green", lwd = 2)

legend("topright", legend = c("Susceptible (S)", "Vaccinated(not infected) (V)", "Infected (I)", "Removed (R)"),
       col = c("blue","purple", "red", "green"), lwd = 2)
