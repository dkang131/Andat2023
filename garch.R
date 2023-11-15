library(tseries)
library(FinTS)
library(rugarch)
library(e1071)

install.packages('fGarch')
library(fGarch)

setwd('C:/Users/darre/Desktop/SEM 7/AnDat/Post Midterm')
return = read.csv('return.csv')
return = as.data.frame(return)

res1_sq = read.csv('res1_sq.csv')
res1_sq = as.data.frame(res1_sq)

garch1_1 = ugarchspec(variance.model = list(garchOrder=c(1,1)),
                      mean.model = list(armaOrder=c(0,0)))

ret_fit = ugarchfit(garch1_1, data = return)
ret_fit

arch1_1 = ugarchspec(variance.model = list(garchOrder=c(1,0)),
                      mean.model = list(armaOrder=c(0,0)))

ret_fit_arch = ugarchfit(arch1_1, data = return)
ret_fit_arch
