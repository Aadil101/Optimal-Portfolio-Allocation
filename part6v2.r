library(tseries)
library(vars)

# read data
setwd('/Users/rtribush/Documents/Math70/')
stockData = read.csv('Stocks.csv')
n=nrow(stockData)

# convert prices to log scale
stockData$AAPL = log(stockData$AAPL)
stockData$TSLA = log(stockData$TSLA)
stockData$GE = log(stockData$GE)
stockData$C = log(stockData$C)
stockData$CCL = log(stockData$CCL)

# time series
aapl = ts(stockData$AAPL)
tsla = ts(stockData$TSLA)
ge = ts(stockData$GE)
c = ts(stockData$C)
ccl = ts(stockData$CCL)

# vector autoregressions
v1 = cbind(aapl, tsla, ge, c, ccl)
colnames(v1) = cbind('AAPL', 'TSLA', 'GE', 'C', 'CCL')
Model1 = VAR(v1, p=1, type='const')
print('1 Lag Cross Predictions:')
print(predict(Model1, n.ahead=1))
Model2 = VAR(v1, p=3, type='const')
print('3 Lag Cross Predictions:')
print(predict(Model2, n.ahead=1))
coeffs = Bcoef(Model1)
coeffs3 = Bcoef(Model2)

# Plot AAPL Stock Data
plot(stockData$AAPL, ylab='Log(Price)', xlab='', xaxt='n', type='l')
title('AAPL Stock Price Autoregression')
axis(side=1, at=seq(from=1, to=n, by=3), labels=stockData$Date[seq(from=1, to=n, by=3)], las=2)

# AAPL First Order Autoregression
y=stockData$AAPL[2:n]
x=stockData$AAPL[1:(n-1)]
o1=lm(y~x)
a=coef(o1)
y.pred=a[1]+a[2]*y
lines(3:(n+1),y.pred,col=2)
print(paste("AAPL 1 Lag Predicted value:",y.pred[n-1]))

# AAPL Third Order Autoregression
yml=stockData$AAPL[4:n]
X=matrix(ncol=3,nrow=n-3)
for(j in 1:3) 
  X[,j]=stockData$AAPL[(4-j):(n-j)] 
oml=lm(yml~X)
a=coef(oml)
y.past=stockData$AAPL[seq(from=n,to=n-2,by=-1)]
yml.pred.n1=a[1]+sum(a[2:4]*y.past)	
print(paste("AAPL 3 Lag Predicted value:",yml.pred.n1))
yfv=c(oml$fitted.values,yml.pred.n1)
lines((4):(n+1),yfv,col=3)

# AAPL First Order Cross Autoregression
Z = data.matrix(stockData[1:n, c('AAPL', 'TSLA', 'GE', 'C', 'CCL')])
predictions = rep(NA, n)
for(i in 1:n) {
  predictions[i] = sum(Z[i,]*coeffs[1,1:5]) + coeffs[1,6]
}
lines(2:(n+1),predictions, col=4)

# AAPL Third Order Cross Autoregression
predictions3 = rep(NA, n-2)
for(i in 1:n-2) {
  predictions3[i] = sum(Z[i,]*coeffs3[1,11:15])+sum(Z[i+1,]*coeffs3[1,6:10])+
    sum(Z[i+2,]*coeffs3[1,1:5])+coeffs3[1,16]
}
lines(4:(n+1), predictions3, col=5)
legend('topleft', legend=c('Actual Prices', '1 Lag', '3 Lag', '1 Lag Cross', '3 Lag Cross'),
       col=c(1,2,3,4,5), lty=1, cex=.75)

# Plot TSLA Stock Data
plot(stockData$TSLA, ylab='Log(Price)', xlab='', xaxt='n', type='l')
title('TSLA Stock Price Autoregression')
axis(side=1, at=seq(from=1, to=n, by=3), labels=stockData$Date[seq(from=1, to=n, by=3)], las=2)

# TSLA First Order Autoregression
y=stockData$TSLA[2:n]
x=stockData$TSLA[1:(n-1)]
o1=lm(y~x)
a=coef(o1)
y.pred=a[1]+a[2]*y
lines(3:(n+1),y.pred,col=2)
print(paste("TSLA 1 Lag Predicted value:",y.pred[n-1]))

# TSLA Third Oder Autoregression
yml=stockData$TSLA[4:n]
X=matrix(ncol=3,nrow=n-3)
for(j in 1:3) 
  X[,j]=stockData$TSLA[(4-j):(n-j)] 
oml=lm(yml~X)
a=coef(oml)
y.past=stockData$TSLA[seq(from=n,to=n-2,by=-1)]
yml.pred.n1=a[1]+sum(a[2:4]*y.past)	
print(paste("TSLA 3 Lag Predicted value:",yml.pred.n1))
yfv=c(oml$fitted.values,yml.pred.n1)
lines((4):(n+1),yfv,col=3)

# TSLA First Order Cross Autoregression
predictions = rep(NA, n)
for(i in 1:n) {
  predictions[i] = sum(Z[i,]*coeffs[2,1:5]) + coeffs[2,6]
}
lines(2:(n+1),predictions, col=4)

# TSLA Third Order Cross Autoregression
predictions3 = rep(NA, n-2)
for(i in 1:n-2) {
  predictions3[i] = sum(Z[i,]*coeffs3[2,11:15])+sum(Z[i+1,]*coeffs3[2,6:10])+
    sum(Z[i+2,]*coeffs3[2,1:5])+coeffs3[2,16]
}
lines(4:(n+1), predictions3, col=5)
legend('topright', legend=c('Actual Prices', '1 Lag', '3 Lag', '1 Lag Cross', '3 Lag Cross'),
       col=c(1,2,3,4,5), lty=1, cex=.75)

# Plot GE Stock Data
plot(stockData$GE, ylab='Log(Price)', xlab='', xaxt='n', type='l')
title('GE Stock Price Autoregression')
axis(side=1, at=seq(from=1, to=n, by=3), labels=stockData$Date[seq(from=1, to=n, by=3)], las=2)

# GE First Order Autoregression
y=stockData$GE[2:n]
x=stockData$GE[1:(n-1)]
o1=lm(y~x)
a=coef(o1)
y.pred=a[1]+a[2]*y
lines(3:(n+1),y.pred,col=2)
print(paste("GE 1 Lag Predicted value:",y.pred[n-1]))

# GE Third Oder Autoregression
yml=stockData$GE[4:n]
X=matrix(ncol=3,nrow=n-3)
for(j in 1:3) 
  X[,j]=stockData$GE[(4-j):(n-j)] 
oml=lm(yml~X)
a=coef(oml)
y.past=stockData$GE[seq(from=n,to=n-2,by=-1)]
yml.pred.n1=a[1]+sum(a[2:4]*y.past)	
print(paste("GE 3 Lag Predicted value:",yml.pred.n1))
yfv=c(oml$fitted.values,yml.pred.n1)
lines((4):(n+1),yfv,col=3)

# GE First Order Cross Autoregression
predictions = rep(NA, n)
for(i in 1:n) {
  predictions[i] = sum(Z[i,]*coeffs[3,1:5]) + coeffs[3,6]
}
lines(2:(n+1),predictions, col=4)

# GE Third Order Cross Autoregression
predictions3 = rep(NA, n-2)
for(i in 1:n-2) {
  predictions3[i] = sum(Z[i,]*coeffs3[3,11:15])+sum(Z[i+1,]*coeffs3[3,6:10])+
    sum(Z[i+2,]*coeffs3[3,1:5])+coeffs3[3,16]
}
lines(4:(n+1), predictions3, col=5)
legend('topright', legend=c('Actual Prices', '1 Lag', '3 Lag', '1 Lag Cross', '3 Lag Cross'),
       col=c(1,2,3,4,5), lty=1, cex=.75)

# Plot C Stock Data
plot(stockData$C, ylab='Log(Price)', xlab='', xaxt='n', type='l')
title('C Stock Price Autoregression')
axis(side=1, at=seq(from=1, to=n, by=3), labels=stockData$Date[seq(from=1, to=n, by=3)], las=2)

# C First Order Autoregression
y=stockData$C[2:n]
x=stockData$C[1:(n-1)]
o1=lm(y~x)
a=coef(o1)
y.pred=a[1]+a[2]*y
lines(3:(n+1),y.pred,col=2)
print(paste("C 1 Lag Predicted value:",y.pred[n-1]))

# C Third Oder Autoregression
yml=stockData$C[4:n]
X=matrix(ncol=3,nrow=n-3)
for(j in 1:3) 
  X[,j]=stockData$C[(4-j):(n-j)] 
oml=lm(yml~X)
a=coef(oml)
y.past=stockData$C[seq(from=n,to=n-2,by=-1)]
yml.pred.n1=a[1]+sum(a[2:4]*y.past)	
print(paste("C 3 Lag Predicted value:",yml.pred.n1))
yfv=c(oml$fitted.values,yml.pred.n1)
lines((4):(n+1),yfv,col=3)

# C First Order Cross Autoregression
predictions = rep(NA, n)
for(i in 1:n) {
  predictions[i] = sum(Z[i,]*coeffs[4,1:5]) + coeffs[4,6]
}
lines(2:(n+1),predictions, col=4)

# C Third Order Cross Autoregression
predictions3 = rep(NA, n-2)
for(i in 1:n-2) {
  predictions3[i] = sum(Z[i,]*coeffs3[4,11:15])+sum(Z[i+1,]*coeffs3[4,6:10])+
    sum(Z[i+2,]*coeffs3[4,1:5])+coeffs3[4,16]
}
lines(4:(n+1), predictions3, col=5)
legend('bottomright', legend=c('Actual Prices', '1 Lag', '3 Lag', '1 Lag Cross', '3 Lag Cross'),
       col=c(1,2,3,4,5), lty=1, cex=.75)

# Plot CCL Stock Data
plot(stockData$CCL, ylab='Log(Price)', xlab='', xaxt='n', type='l')
title('CCL Stock Price Autoregression')
axis(side=1, at=seq(from=1, to=n, by=3), labels=stockData$Date[seq(from=1, to=n, by=3)], las=2)

# CCL First Order Autoregression
y=stockData$CCL[2:n]
x=stockData$CCL[1:(n-1)]
o1=lm(y~x)
a=coef(o1)
y.pred=a[1]+a[2]*y
lines(3:(n+1),y.pred,col=2)
print(paste("CCL 1 Lag Predicted value:",y.pred[n-1]))

# CCL Third Oder Autoregression
yml=stockData$CCL[4:n]
X=matrix(ncol=3,nrow=n-3)
for(j in 1:3) 
  X[,j]=stockData$CCL[(4-j):(n-j)] 
oml=lm(yml~X)
a=coef(oml)
y.past=stockData$CCL[seq(from=n,to=n-2,by=-1)]
yml.pred.n1=a[1]+sum(a[2:4]*y.past)	
print(paste("CCL 3 Lag Predicted value:",yml.pred.n1))
yfv=c(oml$fitted.values,yml.pred.n1)
lines((4):(n+1),yfv,col=3)

# CCL First Order Cross Autoregression
predictions = rep(NA, n)
for(i in 1:n) {
  predictions[i] = sum(Z[i,]*coeffs[5,1:5]) + coeffs[5,6]
}
lines(2:(n+1),predictions, col=4)

# CCL Third Order Cross Autoregression
predictions3 = rep(NA, n-2)
for(i in 1:n-2) {
  predictions3[i] = sum(Z[i,]*coeffs3[5,11:15])+sum(Z[i+1,]*coeffs3[5,6:10])+
    sum(Z[i+2,]*coeffs3[5,1:5])+coeffs3[5,16]
}
lines(4:(n+1), predictions3, col=5)
legend('topright', legend=c('Actual Prices', '1 Lag', '3 Lag', '1 Lag Cross', '3 Lag Cross'),
       col=c(1,2,3,4,5), lty=1, cex=.75)

