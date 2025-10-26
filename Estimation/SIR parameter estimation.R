library(readxl)
simdata <- read_excel("C:/SIR_Data.xlsx") 
simdata$S.delta<- c(simdata$S[-1],0)-simdata$S
simdata$I.delta<- c(simdata$I[-1],0)-simdata$I
simdata$R.delta<- c(simdata$R[-1],0)-simdata$R
simdata<- simdata[-nrow(simdata),]
# difference equations
for (j in 1:nrow(simdata)) {
  simdata$beta.est[j]<- simdata$S.delta[j]/(simdata$S[j]*simdata$I[j])
  simdata$gamma.est[j]<- simdata$R.delta[j]/simdata$I[j]
  simdata$response.var[j]<- simdata$I.delta[j]/simdata$I[j]
}
# estimating using the method of moments
N <- 350
print(beta.hatMM<- -N*mean(simdata$beta.est))
print(gamma.hatMM<- mean(simdata$gamma.est))

# estimating using regression
reg<- glm(response.var ~ S, data=simdata)
reg.summary <- summary(reg)
print(beta.hatR<- N*reg.summary$coefficients[2,1])
print(gamma.hatR<- -reg.summary$coefficients[1,1])

