# simulating a dataset for SIR model
N<- 350
beta<- 0.7
gamma<- 0.2
S<- c()
I<- c()
R<- c()

S[1]<- N-1
I[1]<- 1
R[1]<- 0

for (j in 1:40) {
   S[j+1]<- S[j]-beta*S[j]*I[j]/N
   I[j+1]<- I[j]+beta*S[j]*I[j]/N-gamma*I[j]
   R[j+1]<- R[j]+gamma*I[j]
}

time<- 0:40
plot(time,S, type="l", lty=1, lwd=3, main="The Deterministic 
SIR Epidemic Model", ylim=c(0,N), col="green",ylab="")
lines(time,I,lwd=3, col="red")
lines(time,R,lwd=3, col="blue")
legend("left", c("S","I","R"), lty=1, col=c("green","red","blue"))

dataset.SIR<- cbind(S,I,R) 

# simulating a dataset for SIRS model
N<- 350
beta<- 0.7
gamma<- 0.2
alpha<- 0.1
S<- c()
I<- c()
R<- c()

S[1]<- N-1
I[1]<- 1
R[1]<- 0

for (j in 1:40) {
  S[j+1]<- S[j]-beta*S[j]*I[j]/N + alpha*I[j]
  I[j+1]<- I[j]+beta*S[j]*I[j]/N-gamma*I[j]-alpha*I[j]
  R[j+1]<- R[j]+gamma*I[j]
}

time<- 0:40
plot(time,S, type="l", lty=1, lwd=3, main="The Deterministic 
SIR Epidemic Model", ylim=c(0,N), col="green",ylab="")
lines(time,I,lwd=3, col="red")
lines(time,R,lwd=3, col="blue")
legend("left", c("S","I","R"), lty=1, col=c("green","red","blue"))

dataset.SIRS<- cbind(S,I,R) 

# simulating a dataset for SIRM model
N<- 350
beta<- 0.7
gamma<- 0.2
alpha<- 0.1
omega<- 0.1
S<- c()
I<- c()
R<- c()
M<- c()
S[1]<- N-1
I[1]<- 1
R[1]<- 0
M[1]<- 0
for (j in 1:40) {
  S[j+1]<- S[j]-beta*S[j]*I[j]/N+alpha*I[j]
  I[j+1]<- I[j]+beta*S[j]*I[j]/N-gamma*I[j]-alpha*I[j]-omega*I[j]
  R[j+1]<- R[j]+gamma*I[j]
  M[j+1]<- M[j]+omega*I[j]
}
time<- 0:40
plot(time,S, type="l", lty=1, lwd=3, main="The Deterministic SIRM Epidemic
Model", ylim=c(0,N), col="green")
lines(time,I,lwd=3, col="red")
lines(time,R,lwd=3, col="blue")
lines(time,M,lwd=3, col= "purple")
legend("left", c("S","I","R","M"), lty=1, col=c("green","red","blue","purple"))


Day <- 0:40

sir_df <- data.frame(Day, S, I, R, M)
write_xlsx(sir_df, "C:/data_SIRS.xlsx")


# simulating a dataset for SVIR model (full immunity)
N<- 350
beta<- 0.7
nu<- 0.05 #rate of vaccination
gamma<- 0.2
S<- c()
V<- c()
I<- c()
R<- c()

S[1]<- N-1
V[1]<- 0
I[1]<- 1
R[1]<- 0

for (j in 1:40) {
  S[j+1]<- S[j]-beta*S[j]*I[j]/N-nu*S[j]
  V[j+1]<- V[j]+nu*S[j]
  I[j+1]<- I[j]+beta*S[j]*I[j]/N-gamma*I[j]
  R[j+1]<- R[j]+gamma*I[j]
}

time<- 0:40
plot(time,S, type="l", lty=1, lwd=3, main="The Deterministic 
SVIR Epidemic Model", ylim=c(0,N), col="green",ylab="")
lines(time,V,lwd=3, col="black")
lines(time,I,lwd=3, col="red")
lines(time,R,lwd=3, col="blue")
legend("left", c("S","V","I","R"), lty=1, col=c("green","black","red","blue"))

Day <- 0:40

sir_df <- data.frame(Day, S, V, I, R)
write_xlsx(sir_df, "C:/data_SVIR.xlsx")


# simulating SVIR model (partial immunity)
N<- 350
beta<- 0.7
nu<- 0.05 #rate of vaccination
gamma<- 0.2
epsilon<- 0.15 #vaccinated people's rate of infection
S<- c()
V<- c()
I<- c()
R<- c()

S[1]<- N-1
V[1]<- 0
I[1]<- 1
R[1]<- 0

for (j in 1:40) {
  S[j+1]<- S[j]-beta*S[j]*I[j]/N-nu*S[j]
  V[j+1]<- V[j]+nu*S[j]-epsilon*V[j]*I[j]/N
  I[j+1]<- I[j]+beta*S[j]*I[j]/N-gamma*I[j]+epsilon*V[j]*I[j]/N
  R[j+1]<- R[j]+gamma*I[j]
}

time<- 0:40
plot(time,S, type="l", lty=1, lwd=3, main="The Deterministic 
SVIR Epidemic Model", ylim=c(0,N), col="green",ylab="")
lines(time,V,lwd=3, col="black")
lines(time,I,lwd=3, col="red")
lines(time,R,lwd=3, col="blue")
legend("left", c("S","V","I","R"), lty=1, col=c("green","black","red","blue"))

Day <- 0:40

sir_df <- data.frame(Day, S, V, I, R)
write_xlsx(sir_df, "C:/data_SVIRpartial.xlsx")
