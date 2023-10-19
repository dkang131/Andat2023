data=read.csv("C:/Users/darre/Downloads/insurance.csv", header=T, sep=",")
#install.packages('lmtest')
library('lmtest')
#install.packages('car')
library('car')
x1=data$age
x2=data$bmi
y=data$charges
N=nrow(data)
lm.mod_1=lm(y~x1+x2, data=data)
summary(lm.mod_1)
bptest(lm.mod_1)
ncvTest(lm.mod_1)
#pvalue sangat kecil maka tidak identik
# both bptest and ncvtest

resi_data = data.frame(log_e2=log(lm.mod_1$residuals^2), x1,x2)
resi_mdl = lm(log_e2~x1+x2,data = resi_data)
n_ext = exp(resi_mdl$fitted.values)
mdl_wls = lm(y~x1+x2, data = data, weights = 1/n_ext)
summary(mdl_wls)
print(round(coef(summary(mdl_wls)),5))
ncvTest(mdl_wls)
#asumsi di wls lebih baik menggunakan ncvtest karena
#bisa mengakomodasi pembobotnya
# w = 1/exp(y_hat)
# y_hat dari model log e^2 = b_0 + b_1x_1 + b_2x_2


#### Transformasi -> mengatasi reidual tidak normal ####
library(xlsx)
data_nnorm <- read.xlsx("C:/Users/darre/Downloads/Data tidak normal.xlsx", sheetIndex = 1)
x <- data_nnorm$X
y <- data_nnorm$Y

lm.mod2 <- lm(y~x, data = data_nnorm)
shapiro.test(lm.mod2$residuals)
bc= boxCox(y~x, lambda = seq(-5,5,1/100))
lambda = bc$x[which.max(bc$y)];lambda

g = exp(mean(log(y))) #geometric mean from original y
mdl.trans = update(lm.mod2, ((((.)^lambda)-1)/((lambda*g)^(lambda-1)))~ .)
summary(mdl.trans)
shapiro.test(mdl.trans$residuals)


#### cochran-orcutt -> mengatasi residual tidak independen ####
data_auto <- read.xlsx("C:/Users/darre/Downloads/Data Autokorelasi.xlsx", sheetIndex = 1)
lm.mod_3 <- lm(Y ~ X1+X2, data = data_auto)
dwtest(lm.mod_3)
par(mfrow=c(1,1))
acf(lm.mod_3$residuals, lag.max = 15, plot = T)
res.ts=residuals(lm.mod_3)[2:20] #Create Lag residuals
lag1res=residuals(lm.mod_3)[1:19] #Create Lag residuals
lagdata1=data.frame(res.ts, lag1res) #Bind time series data
acp=coef(lm(res.ts~lag1res -1, data=lagdata1)) #Rho coef (respon=residual, prediktor=lag residual)
acp
lag1y=data_auto$Y[1:19] #lag for x and y
y=data_auto$Y[2:20] #lag for x and y
lag1x_1=data_auto$X1[1:19] #lag for x and y
x1=data_auto$X1[2:20] #lag for x and y
lag1x_2=data_auto$X2[1:19] #lag for x and y
x2=data_auto$X2[2:20] #lag for x and y
y.co=y-(acp*lag1y) #X-(rho*lagX)
x1.co=x1-(acp*lag1x_1) #X-(rho*lagX)
x2.co=x2-(acp*lag1x_2) #X-(rho*lagX)
mdl.co=lm(y.co~x1.co+x2.co) #Regression OLS using transformation data
summary(mdl.co)
dwtest(mdl.co)  
# melebihi alpha memenuhi independen

#### robust regression ####
library(MASS)
model2 <- rlm(y~x1+x2, data = data_auto, method = c("MM"));model2
summary(model2)
