#daily returns have little autocorrelation
#Autocorrelation of daily S&P 500 returns
#Get the data
sp500 = read.csv("C:\\Users\\74596\\Desktop\\easyWay\\SP500.csv",header = TRUE)
price = sp500$Close
plot(price,type = "l")
T = length(price)
return = log(price[2:T]) - log(price[1:(T-1)])
ind = which(return!=0)
return = return[ind]
autocorr = acf(return,100)$acf[2:101]
plot(autocorr,type = "l",col = "blue")
abline(0,0,col = "red")
 
#return sqrt
returnSqrt = return^2
autocorr = acf(returnSqrt,100)$acf[2:101]
plot(autocorr,type = "l",col = "blue")
hist(return)

mean(return)*100
sd(return)*100

#Historical simulation
#get the VaR
T = length(return)
var_250 = numeric(T)
var1_250 = numeric(T)
for(i in 251:T){
  var_250[i] = -quantile(return[(i-250):(i)],probs = 0.01)
  var1_250[i] = -quantile(return[(i-250):(i)],probs = 0.05)
}
plot(var_250,type = "l",col = 'blue')
points(var1_250,type = "l",col = "red")

#How much does the initial value matter in the RiskMetrics model
sigma2_A = numeric(T)
sigma2_B = numeric(T)
sigma2_C = numeric(T)
sigma2_B[1] = -0.0134^2
sigma2_C[1] = -0.05^2

for(i in 2:T){
  sigma2_A[i] = 0.94*sigma2_A[i-1] + 0.06*return[i-1]^2
  sigma2_B[i] = 0.94*sigma2_B[i-1] + 0.06*return[i-1]^2
  sigma2_C[i] = 0.94*sigma2_C[i-1] + 0.06*return[i-1]^2
}
plot(sigma2_A,col = "black", type = "l")
points(sigma2_B,col = "blue", type = "l")
points(sigma2_C,col = "red",type = "l")

#Get the data
data = read.csv("C:\\FRM\\2019.1-2019.5\\Modeling2\\Lecture3\\RegressionData.csv",header = TRUE)
x1 = data$x1
y1 = data$y1
mean(x1)
var(x1)
mean(y1)
var(y1)
cor(x1,y1)
reg = lm(y1~x1)
reg
plot(x1,y1,xlim = c(0,20),ylim = c(0,15))
points(x1,reg$fitted.values,type = "l",col = "red")
x2 = data$x2
y2 = data$y2
mean(x2)
var(x2)
mean(y2)
var(y2)
cor(x2,y2)
reg2=lm(y2~x2)
reg2
plot(x2,y2,xlim = c(0,20),ylim = c(0,15))
points(x2,reg2$fitted.values,type = "l",col = "red")

x3 = data$x3
y3 = data$y3
mean(x3)
var(x3)
mean(y3)
var(y3)
cor(x3,y3)
reg3 = lm(y3~x3)
reg3
plot(x3,y3,xlim = c(0,20),ylim = c(0,15))
points(x3,reg3$fitted.values,type = "l",col = "red")

ar1 = arima.sim(model = list(ar = 0.5),n = 1000,sd = 0.1)
acf(ar1)
pacf(ar1)
arma1 = arima.sim(model=list(ma=c(0.99)),n=1000,sd=0.1)
acf(arma1)
pacf(arma1)
