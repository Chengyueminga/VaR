rm(list=ls())
####Historical simulation  
install.packages("quantmod")
library("quantmod")
getSymbols("SPY",start = "2008-01-01",end = '2010-10-30')
ret <- diff(log(SPY$SPY.Close))
T <- length(ret)
ret <- ret[2:T]
T <- length(ret)
VaR1_250 <- numeric(T)
VaR1_80 <- numeric(T)
VaR1_1000 <- numeric(T)
VaR5_250 <- numeric(T)

##compare with quantile 5 and 1
for (i in 251:T){
  VaR1_250[i] <- -quantile(ret[(i-250):i], probs = 0.01)
}
for (i in 251:T){
  VaR5_250[i] <- -quantile(ret[(i-250):i], probs = 0.05)
}

for (i in 1001:T){
  VaR1_1000[i] <- -quantile(ret[(i-1000):i], probs = 0.01)
}
for (i in 81:T){
  VaR1_80[i] <- -quantile(ret[(i-80):i], probs = 0.01)
}
plot(VaR1_250, type = "l",col = "red")
points(VaR5_250,type = "l",col = "blue")


####Compare with 80days,1000days, 250 days
plot(VaR1_250,type = "l", col="red",lwd = 3)
points(VaR1_1000,type = "l",col ="black",lwd = 3)

plot(VaR1_250, type = "l", col="red",lwd = 3)
points(VaR1_80,type = "l",col = "blue", lwd = 3)


library('quantmod')
library('fGarch')
#my data BAC, from 2000-01-03 to 2018-03-20
getSymbols('BAC',from = '2000-01-03', to ='2018-03-20')
price <- BAC$BAC.Close
plot(price, type = "l")

#GarchModel(1,1) firslty, we calculate the returns in this time period
ret <- diff(log(BAC$BAC.Close))
ret <- ret[-1,]
T <- length(ret)
var1_fhs  <- numeric(T)
var1_garch <- numeric(T)

#Then we choose 500 days as a return window and estimate GARCH based on data in return windows
#We can get one GARCH for every return window 
#Based on GARCH, we know returns and can get sigmas, we can calculate the standardized returns
#And find the worst standardized return in probability = 0.01
#And we can also estimate the t+1 predictive sigma 
#When we use predictive sigma to mulptily standardized return with probability = 0.01
#So we get the Value at Risk in 0.01
#In Filter Historical Simulation, more extreme situations are included in real world
#It captures the tail risk
#Based on the picture, we can find that the FHS is higher than GARCH model
for (i in 501 : T){
  retwindow <- ret[(i -500) : (i - 1)]
  #GARCH(1,1) IN FHS GARCH
  fit2 <- garchFit(formula =~ garch(1,1),data = retwindow,
                   trace = FALSE)
  omega <- fit2@fit$coef[2]
  alpha <- fit2@fit$coef[3]
  beta  <- fit2@fit$coef[4]
  sigma <- fit2@sigma.t
  sigmapred <- sqrt(omega + alpha*retwindow[500]^2 + beta*sigma[500]^2)
  retstand <- retwindow/sigma
  var1_fhs[i]  <- -sigmapred*quantile(retstand, probs=0.01)
  var1_garch[i] <- -qnorm(0.01, mean=0, sd=sigmapred)
}

plot(var1_fhs, type = "l",col = "blue")
points(var1_garch,type = "l",col = "red")

library('fGarch')
sp500 <- read.csv('P:\\FRM\\Modeling\\SP500.csv')
Ret   <- diff(log(sp500$Close))
result <- garchFit( formula = ~garch(1, 1), data = Ret[1:1000], trace = FALSE)
omega <- result@fit$par[2]
alpha <- result@fit$par[3]
beta  <- result@fit$par[4]
sigma  <- result@sigma.t
Retstand <- Ret[1:1000]/sigma
sigmapred <- sqrt(omega + alpha*Ret[1000]^2 + beta*sigma[1000]^2)

sigmapred/sqrt(omega/(1-alpha-beta))


MC <- 10000
T  <- 500
shock <- matrix(0, MC, T)
for (i in 1:T){
  shock[, i] <- sample(Retstand, MC, replace=T) 
}


ReturnMC <- matrix(NA, MC, T)
for (i in 1:MC){
  sigmapredMC <- sigmapred
  for (j in 1:T){
    ReturnMC[i, j] <- sigmapredMC*shock[i, j]
    sigmapredMC <- sqrt(omega + alpha*ReturnMC[i, j]^2 + beta*sigmapredMC^2)
  }
}


ReturnMCT <- matrix(NA, MC, T)
for (i in 1:MC){
  ReturnMCT[i, ] <- cumsum(ReturnMC[i, ])
}




### Value at Risk
VaRMC     <- numeric(T)
for (i in 1:T){
  VaRMC[i] <- -quantile(ReturnMCT[,i], probs=0.01)
}
#par(mfrow=c(1,2))
plot(VaRMC, type='l', lwd=4)
plot(VaRMC/sqrt(1:T)/VaRMC[1], type='l', lwd=4)




### Expected Shortfall
ESMC     <- numeric(T)
for (i in 1:T){
  indES    <- which(ReturnMCT[,i] <= -VaRMC[i])
  ESMC[i] <- -mean(ReturnMCT[indES,i])
}
#par(mfrow=c(1,2))
plot(ESMC, type='l', lwd=4)
plot(ESMC/sqrt(1:T)/ESMC[1], type='l', lwd=4)