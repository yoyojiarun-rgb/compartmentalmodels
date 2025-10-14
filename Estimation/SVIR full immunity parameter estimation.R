library(readxl)
simdata <- read_excel("C:/Users/yoyoj/Downloads/data_SVIR.xlsx")
simdata$S.delta<- c(simdata$S[-1],0)-simdata$S
simdata$I.delta<- c(simdata$I[-1],0)-simdata$I
simdata$R.delta<- c(simdata$R[-1],0)-simdata$R
simdata$V.delta<- c(simdata$V[-1],0)-simdata$V
simdata<- simdata[-nrow(simdata),]

for (j in 1:nrow(simdata)) {
  simdata$response.vartwo[j] <- simdata$I.delta[j] / simdata$I[j]
  simdata$response.varone[j] <- simdata$S.delta[j] / simdata$S[j]
}
N <- 350
reg<- glm(response.vartwo ~ S, data=simdata)
reg.summary <- summary(reg)
print(beta.hatR2<- N*reg.summary$coefficients[2,1])
print(gamma.hatR<- -reg.summary$coefficients[1,1])

reg1<- glm(response.varone ~ I, data=simdata)
reg1.summary <- summary(reg1)
print(beta.hatR1<- -N*reg1.summary$coefficients[2,1])
print(nu.hatR<- -reg1.summary$coefficients[1,1])

#method of moments


for (j in 1:nrow(simdata)) {
  simdata$gamma.est[j]<- simdata$R.delta[j]/simdata$I[j]
  simdata$nu.est[j]<- simdata$V.delta[j]/simdata$S[j]
  simdata$alpha.est[j]<- (simdata$S.delta[j]/simdata$I[j])+(beta.hatR*simdata$S[j]/N)
}
print(gamma.hatMM<- mean(simdata$gamma.est))
print(nu.hatMM<- mean(simdata$nu.est))

for (j in 1:nrow(simdata)) {
  simdata$beta.est[j]<- (simdata$S.delta[j]+simdata$S[j]*nu.hatR)/(simdata$S[j]*simdata$I[j])
}

print(beta.hatMM<- -N *mean(simdata$beta.est))
