setwd("C:/Users/darre/Desktop/SEM 7/AnDat/Pre Midterm")
library(car)
##Ridge Regression
y=longley[,"Employed"]
x=data.matrix(longley[,c(2:6,1)])
lm.mod4=lm(Employed~.,data=longley)
summary(lm.mod4)
vif(lm.mod4)
#install.packages("glmnet")
library(glmnet)
model=glmnet(x,y,alpha=0)#fit ridge regression model
summary(model) 
cv_model=cv.glmnet(x,y,alpha=0) #find optimal lambda
best_lambda=cv_model$lambda.min
best_lambda
plot(cv_model) 
best_model=glmnet(x,y,alpha=0,lambda=best_lambda)
coef(best_model)
plot(model, xvar = "lambda")
y_predicted=predict(model,s=best_lambda,newx=x)
sst=sum((y-mean(y))^2)
sse=sum((y_predicted-y)^2)
rsq=1-sse/sst
rsq
#################package lain
data=longley
lmod=lm(Employed~GNP+Unemployed+Armed.Forces+Population+Year+GNP.deflator,data=longley)
summary(lmod)
vif(lmod)
longley.y=longley[,"Employed"]
longley.X=data.matrix(longley[,c(2:6,1)])
#install.packages("lmridge")
library(lmridge)
mod=lmridge(longley.y~longley.X, data=data,K=seq(0,1,0.1)) #find optimal lambda (k=lambda)
summary(mod) #optimal lambda by minimum MSE; smaller increment longer running time
kest(mod) #optimal lambda by different refferences (LW (lm.ridge))
best.mod=lmridge(longley.y~longley.X,data=data,K= 0.02907) #use optimal lambda(k) basef on LW (lm.ridge)
summary(best.mod)
vif(best.mod)


