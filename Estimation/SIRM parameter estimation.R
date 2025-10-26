library(readxl)
simdata <- read_excel("C:/data_SIRM.xlsx")
simdata$S.delta<- c(simdata$S[-1],0)-simdata$S
simdata$I.delta<- c(simdata$I[-1],0)-simdata$I
simdata$R.delta<- c(simdata$R[-1],0)-simdata$R
simdata$M.delta<- c(simdata$M[-1],0)-simdata$M
simdata<- simdata[-nrow(simdata),]
# estimating using regression
for (j in 1:nrow(simdata)) {
  simdata$response2.var[j] <- simdata$I.delta[j] / simdata$I[j]
  simdata$response1.var[j] <- simdata$S.delta[j] / simdata$I[j]
}
N <- 350
reg2<- glm(response2.var ~ S, data=simdata)
reg2.summary <- summary(reg2)
print(beta.hatR<- N*reg2.summary$coefficients[2,1])
print(gamma.hatR_plus_alpha.hatR_plus_omega.hatR <- -reg2.summary$coefficients[1,1])

reg1<- glm(response1.var ~ S, data=simdata)
reg1.summary <- summary(reg1)
print(beta.hatR1<- -N*reg1.summary$coefficients[2,1])
print(alpha.hatR1 <- reg1.summary$coefficients[1,1])

# estimating using method of moments and regression method

for (j in 1:nrow(simdata)) {
  simdata$gamma.est[j]<- simdata$R.delta[j]/simdata$I[j]
  simdata$omega.est[j]<- simdata$M.delta[j]/simdata$I[j]
  simdata$alpha.est[j]<- (simdata$S.delta[j]/simdata$I[j])+(beta.hatR*simdata$S[j]/N)
}
print(gamma.hatMM<- mean(simdata$gamma.est))
print(omega.hatMM<- mean(simdata$omega.est))
print(alpha.hatR<- -reg2.summary$coefficients[1,1]-gamma.hatMM-omega.hatMM)
print(alpha.hatMM<- mean(simdata$alpha.est))
print(omega.hatR<- -reg2.summary$coefficients[1,1]-gamma.hatMM-alpha.hatR)
print(gamma.hatR<- -reg2.summary$coefficients[1,1]-omega.hatR-alpha.hatR)
print(omega.hatR1<- -reg2.summary$coefficients[1,1]-gamma.hatMM-alpha.hatR1)
print(gamma.hatR1<- -reg2.summary$coefficients[1,1]-omega.hatMM-alpha.hatR1)

for (j in 1:nrow(simdata)) {
  simdata$beta.est[j]<- (simdata$S.delta[j]-simdata$I[j]*alpha.hatMM)/(simdata$S[j]*simdata$I[j])
}

print(beta.hatMM<- -N*mean(simdata$beta.est))
