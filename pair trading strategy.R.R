
#library
library(quantmod)
library(lmtest)
library(PerformanceAnalytics)
library(xts)
library(zoo)
library(TTR)
library(stargazer)
library(tseries)
library(vars)
library(rugarch)
#library(knitr)

#read datas
stock1=getSymbols("600170.SS", auto.assign=F, from="2018-01-01", to="2019-01-01", src = "yahoo")
stock2=getSymbols("601668.SS", auto.assign=F, from="2018-01-01", to="2019-01-01", src = "yahoo")
head(stock1)
stock1$`600170.SS.Close`
#Combine return
return_s1=ROC(stock1$`600170.SS.Close`)
return_s2=ROC(stock2$`601668.SS.Close`)
return_all=cbind(return_s1,return_s2)

#Descriptive statistics
return_all=na.omit(return_all)
f_table=function(x){
  table=matrix(NA,nrow=2,ncol=6)
  for (i in 1:2) {
    series=x[,i]
    table[i,1]=mean(series)
    table[i,2]=min(series)
    table[i,3]=median(series)
    table[i,4]=max(series)
    table[i,5]=sd(series)
    table[i,6]=length(series)
  }
  colnames(table)=c("mean","min","median","max","sd","number")
  row.names(table)=c("TECHM","HCLTECH")
  table=round(table,3)
  return(table)
}


Statistics=f_table(return_all)
write.csv(Statistics,"~/statistics.csv")

#Correlation coefficient
Cor_table=cor(return_all)

Cor_table
write.csv(Cor_table,"~/Cor_table.csv")

#Stationary
adf.test(return_all$`600170.SS.Close`)
adf.test(return_all$`601668.SS.Close`)

#plot the return
par(mfrow=c(1,1))
plot(return_all[,1],main="stock1",ylab="return")
plot(return_all[,2],main="stock2",ylab="return")

#autocorrelation AR model
acf(return_all[,1])
pacf(return_all[,1])
AR_1=ar(return_all[,1])
sum(AR_1$ar)
AR_1$order

#garch model
spec_1=ugarchspec(mean.model = list(armaOrder=c(1,1),distribution="std"))
garchfit_1=ugarchfit(spec_1,return_all[,1])

garchfit_1@fit$robust.matcoef
write.csv(garchfit_1@fit$robust.matcoef,"~/garch_1.csv")

#VAR
var_model=VAR(return_all,ic="AIC",lag.max=5)
causality(var_model,cause =return_all$`601668.SS.Close`)
Acoef(var_model)
Bcoef(var_model)
coefficients=coef(var_model)
coefficients$`600170.SS.Close`

#regression
stock1_ex=cbind(stock1,return=ROC(stock1$`600170.SS.Close`))
stock1_ex=na.omit(stock1_ex)
stock2_ex=cbind(stock2,return=ROC(stock2$`601668.SS.Close`))
stock2_ex=na.omit(stock2_ex)

re_stock1=lm(stock1_ex$`600170.SS.Close`.1~lag(stock1_ex$`600170.SS.Close`.1,1)+stock1_ex$`600170.SS.Volume`
             +lag(stock1_ex$`600170.SS.Volume`,1))
summary(re_stock1)

re_stock2=lm(stock2_ex$`601668.SS.Close`.1~lag(stock2_ex$`601668.SS.Close`.1,1)+stock2_ex$`601668.SS.Volume`
             +lag(stock2_ex$`601668.SS.Volume`,1))
summary(re_stock2)

stargazer(re_stock2,type="html",out="stock1.htm",report="vc*t",align=T)

#trading strategy one: moving average
raw=stock1$`600170.SS.Close`
data=return_all[,1]
sma=SMA(data,n=9)
macd=MACD(raw,12,26,9)
signal=lag(ifelse(macd$macd<macd$signal,-1,1))
return_1=signal*data
return_1=na.omit(return_1)
portfolio=exp(cumsum(return_1))
plot(macd$signal, main="Buy/sell signal");lines(signal,col="red")
charts.PerformanceSummary(return_1,main="perform summary")

#pair trading strategy
return1=return_all[,1]
return2=return_all[,2]
return1=round(return1+1,4)
return2=round(return2+1,4)
return1=cumprod(return1)
return2=cumprod(return2)

diff=return1-return2
mean=rollapply(diff,10,mean)
std=rollapply(diff,10,sd)

up=mean+std
down=mean-std

signal=ifelse(diff>up,-1, ifelse(diff<down,1,0))

spread_return=return_all[,1] - return_all[,2]
trade_return=spread_return*lag(signal)
portfolio=exp(cumsum(trade_return))
portfolio=na.omit(portfolio)
portfolio=portfolio[portfolio!=Inf & portfolio!=-Inf]

charts.PerformanceSummary(trade_return)




















