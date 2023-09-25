library(lmtest)
library(car)
library(nortest)
library(MASS)

data <- read.csv("C:/Users/darre/Downloads/World Happiness Report 2020.csv", sep = ",")
head(data)

y <- data$Healthy.life.expectancy
x1 <- data$Perceptions.of.corruption
x2 <- data$Ladder.score.in.Dystopia
x3 <- data$Social.support

data_final <- data.frame(y,x1,x2,x3)

assumption_check <- function(data_input){
  model_reg <- lm(y~x1+x2+x3, data = data_input)
  
  cat('Checking Heteroscedasticity','\n')
  cat('ncvTest','\n')
  ncv_check <- ncvTest(model_reg)
  if(ncv_check$p > 0.05){
    cat('Pvalue: ',ncv_check$p,'\n')
    cat('Residuals is Identical','\n')
  }else{
    cat('Pvalue: ',ncv_check$p,'\n')
    cat('Residuals is not identic, proceed to assumption handling','\n')
  }
  cat('\n')
  cat('bptest','\n')
  bp_check <- bptest(model_reg)
  if(bp_check$p.value > 0.05){
    cat('Pvalue: ',bp_check$p.value,'\n')
    cat('Residuals is Identical','\n')
  }else{
    cat('Pvalue: ',bp_check$p.value,'\n')
    cat('Residuals is not identic, proceed to assumption handling','\n')
  }
  cat('\n')
  cat('Checking normality of residuals','\n')
  norm_check <- ad.test(model_reg$residuals)
  if(norm_check$p.value > 0.05){
    cat('Pvalue: ',norm_check$p.value,'\n')
    cat('Residuals is normally distributed','\n')
  }else{
    cat('Pvalue: ',norm_check$p.value,'\n')
    cat('Residuals is not normally distributed, proceed to assumption handling','\n')
  }
  cat('\n')
  cat('Checking Non Independent Residuals','\n')
  independent_check <- dwtest(model_reg)
  if(independent_check$p.value > 0.05){
    cat('Pvalue: ',independent_check$p.value,'\n')
    cat('Residuals is independent','\n')
  }else{
    cat('Pvalue: ',independent_check$p.value,'\n')
    cat('Residuals is not independent, proceed to assumption handling','\n')
  }
  cat('\n')
  cat('Checking Multicollinearity','\n')
  print(vif(model_reg))
}
assumption_check(data_final)


#### Handling not identical residual ####
identic_handling <- function(data_input){
  model_reg <- lm(y~x1+x2+x3, data = data_input)
  
  resi_data = data.frame(log_e2=log(model_reg$residuals^2), x1,x2,x3)
  resi_mdl = lm(log_e2~x1+x2+x3,data = resi_data)
  h_ext = exp(resi_mdl$fitted.values)
  mdl_wls = lm(y~x1+x2+x3, data = data, weights = 1/h_ext)
  summary(mdl_wls)
  print(round(coef(summary(mdl_wls)),5))
  ncv_res <- ncvTest(mdl_wls)
  if(ncv_res$p > 0.05){
    cat('Pvalue: ',ncv_res$p,'\n')
    cat('Residuals is Identical')
  }else{
    cat('Pvalue: ',ncv_res$p,'\n')
    cat('Residuals is not identic, redo WLS')
  }
}
identic_handling(data_final)
