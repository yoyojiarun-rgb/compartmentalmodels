library(readxl)
simdata <- read_excel("C:/Users/yoyoj/Downloads/alpha_0.1_data_SIRS.xlsx")
simdata$S.delta<- c(simdata$S[-1],0)-simdata$S
simdata$I.delta<- c(simdata$I[-1],0)-simdata$I
simdata$R.delta<- c(simdata$R[-1],0)-simdata$R
simdata<- simdata[-nrow(simdata),]

for (j in 1:nrow(simdata)) {
  simdata$response.var[j] <- simdata$I.delta[j] / simdata$I[j]
}
N <- 350
reg<- glm(response.var ~ S, data=simdata)
reg.summary <- summary(reg)
print(beta.hatR<- N*reg.summary$coefficients[2,1])
print(gamma.hatR_plus_alpha.hatR <- -reg.summary$coefficients[1,1])

#method of moments


for (j in 1:nrow(simdata)) {
  simdata$gamma.est[j]<- simdata$R.delta[j]/simdata$I[j]
  simdata$alpha.est[j]<- (simdata$S.delta[j]/simdata$I[j])+(beta.hatR*simdata$S[j]/N)
}
print(gamma.hatMM<- mean(simdata$gamma.est))
print(alpha.hatR<- -reg.summary$coefficients[1,1]-gamma.hatMM)
print(alpha.hatMM<- mean(simdata$alpha.est))

for (j in 1:nrow(simdata)) {
  simdata$beta.est[j]<- (simdata$S.delta[j]-simdata$I[j]*alpha.hatMM)/(simdata$S[j]*simdata$I[j])
}

print(beta.hatMM<- -N*mean(simdata$beta.est))

