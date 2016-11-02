
##############AMAN KUMAR########################################


#install.packages("ISLR")
library (ISLR)
library(MASS)
library(car)
library(leaps)
View(College)
coll=Colleg
str(coll)
cor(mtcars[,unlist(lapply(mtcars, is.numeric))])
reg1=lm(coll$Apps~.,data = coll)
summary(reg1)
#from the summary only inp variable are kept for reg2
reg2=lm(coll$Apps~Accept+Enroll+Top10perc+Top25perc+Outstate+Room.Board+Expend+Grad.Rate+Grad.Rate,data = coll)
summary(reg2)
#regbest is made on removing any correlated values from reg 2 which can be seen from line 8
regbest=lm(coll$Apps~+Top10perc+Outstate+Room.Board+Expend+Grad.Rate+Grad.Rate,data = coll)
#removed accept because highly correlated too app and eroll
#removed top 25 becuse it is corelated more to App than top 10 and top 10 and top 25 is highly correlated
summary(regbest)

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -102.23469  611.91621  -0.167   0.8674    
# Top10perc     76.70714   10.12418   7.577 1.02e-13 ***
#   Outstate      -0.43950    0.05243  -8.383 2.44e-16 ***
#   Room.Board     0.70667    0.15288   4.622 4.45e-06 ***
#   Expend         0.15478    0.03717   4.164 3.49e-05 ***
#   Grad.Rate     15.33776    9.31003   1.647   0.0999 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3488 on 771 degrees of freedom
# Multiple R-squared:  0.1928,	Adjusted R-squared:  0.1875 
# F-statistic: 36.83 on 5 and 771 DF,  p-value: < 2.2e-16






#Test for Heteroscedasticity

# The ncvTest() function produces a score test of the hypothesis 
# of constant error variance against the alternative
# that the error variance changes with the level of the fitted values.
ncvTest(reg1)
ncvTest(reg2)
ncvTest(regbest)
#for all the three model heteroscedasticity
#but least for regbest

# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 1449.211    Df = 1     p = 4.254011e-317 
# > ncvTest(reg2)
# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 1429.646    Df = 1     p = 7.590534e-313 
# > ncvTest(regbest)
# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 205.4972    Df = 1     p = 1.319142e-46


#The spreadLevelPlot() function creates a scatter plot of the absolute standardized residuals versus the fitted values, and superimposes a line of best fit.
spreadLevelPlot(reg1)
spreadLevelPlot(reg2)
spreadLevelPlot(regbest)

# `

#Test for multicollinearity
vif(reg1)
vif(reg2)

#Accept     Enroll  Top10perc  Top25perc   Outstate Room.Board     Expend 
#6.868368   7.243114   6.738772   5.313566   3.174555   1.850555   2.519297 
#Grad.Rate 
#1.654478 

vif(regbest)

# Top10perc   Outstate Room.Board     Expend  Grad.Rate 
# 2.033906   2.836734   1.792636   2.402789   1.630907  

#@@@@for regbest and reg2 the vif is best

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#now focusing on just model
#1  reg2
#regbest


#reg2 outliers

z = round(cooks.distance(reg2),4) 
cutoff = 4/nrow(coll)
length(z[z>cutoff])
plot(reg2,which=4,cook.levels = cutoff) 
abline(h=cutoff,col="red")
#length(z[z>cutoff])
#[1] 45

#now giving diffrent weights to the outliers

cutoff = 4/length(coll)
z = cooks.distance(reg2)
w2 = ifelse(z<=cutoff,1,cutoff/z)
reg4 = lm(coll$Apps~Accept+Enroll+Top10perc+Top25perc+Outstate+Room.Board+Expend+Grad.Rate+Grad.Rate,data = coll,weights = w2)
summary(reg4)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -889.99287  214.92800  -4.141 3.84e-05 ***
#  Accept         1.35988    0.04661  29.173  < 2e-16 ***
#  Enroll         0.02206    0.11930   0.185 0.853373    
#Top10perc     44.83743    5.25209   8.537  < 2e-16 ***
#  Top25perc    -14.21731    4.14853  -3.427 0.000643 ***
#  Outstate      -0.11094    0.01590  -6.979 6.45e-12 ***
#  Room.Board     0.17566    0.04435   3.960 8.18e-05 ***
#  Expend         0.06868    0.01087   6.318 4.48e-10 ***
#  Grad.Rate      6.10645    2.66880   2.288 0.022403 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 992.4 on 768 degrees of freedom
#Multiple R-squared:  0.9212,	Adjusted R-squared:  0.9204 
#F-statistic:  1122 on 8 and 768 DF,  p-value: < 2.2e-16


#regbest outliers detection

z = round(cooks.distance(regbest),4) 
cutoff = 4/nrow(coll)
length(z[z>cutoff])
plot(regbest,which=4,cook.levels = cutoff)
# > length(z[z>cutoff])
# [1] 43
abline(h=cutoff,col="red")


#regresion after removing outliers ie putting weights to the outliers

cutoff = 4/length(coll)
z = cooks.distance(regbest)
w3 = ifelse(z<=cutoff,1,cutoff/z)
reg5 = lm(coll$Apps~Top10perc+Outstate+Room.Board+Expend+Grad.Rate+Grad.Rate,data = coll,weights = w3)
summary(reg5)

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -102.23469  611.91621  -0.167   0.8674    
# Top10perc     76.70714   10.12418   7.577 1.02e-13 ***
#   Outstate      -0.43950    0.05243  -8.383 2.44e-16 ***
#   Room.Board     0.70667    0.15288   4.622 4.45e-06 ***
#   Expend         0.15478    0.03717   4.164 3.49e-05 ***
#   Grad.Rate     15.33776    9.31003   1.647   0.0999 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3488 on 771 degrees of freedom
# Multiple R-squared:  0.1928,	Adjusted R-squared:  0.1875 
# F-statistic: 36.83 on 5 and 771 DF,  p-value: < 2.2e-16

#now structure of regression

#now all the variables are significant
#transformed regression 1 after removing outliers




opar = par() 
par(mfrow=c(2,2)) 
plot(reg4) 
par(opar)
#transformed regressionbest after removing outliers


opar = par() 
par(mfrow=c(2,2)) 
plot(reg5) 
par(opar)

# applying bocCox() which states whether we need to use log transformation
#on the dependent variable


boxCox(reg4, family="yjPower", plotit = TRUE)

#lamda not be near to zero so no log transformation

boxCox(reg5, family="yjPower", plotit = TRUE)

##lamda not be near to zero so no log transformation


#now finding power to be used for liner regression

boxTidwell(Apps~Enroll+Top10perc+Outstate+Expend+Grad.Rate+Top25perc,data=coll)

#Score Statistic   p-value MLE of lambda
#Enroll         -4.1427856 0.0000343     0.8641373
#Top10perc       4.4464998 0.0000087     0.1375179
#Outstate       -2.7842988 0.0053644   -25.0697911
#Expend         -0.7459597 0.4556917     0.1474975
#Grad.Rate      -1.1626244 0.2449819    -0.2108239
#Top25perc       0.9737399 0.3301857     8.5580808

#iterations =  15 

#now putting those power to the linear regression

reg4=lm(coll$Apps~Accept+Enroll+I(Enroll^.8)+Top10perc+I(Top10perc^.13)+Top10perc+I(Top25perc^8.5)+Outstate+I(Outstate^-25)+Room.Board+Expend+I(Expend^0.14)+Grad.Rate+I(Grad.Rate^-0.2),data = coll,weights = w3)
summary(reg4)

#r2 increases

reg5=lm(log(coll$Apps)~+Top10perc+I(Top10perc^.13)+Outstate+I(Outstate^-25)+Room.Board+Expend+I(Expend^0.14)+Grad.Rate+I(Grad.Rate^-0.2),data = coll,weights = w3)
summary(reg5)




#now finding variable interaction and puting them into the model
# The function step(regression model, ~.^2) is a powerful one that evaluates all possible 
# interactions that can be added to the model. Use it as follows.
res = step(reg4,~.^2)
res$anova
reg7=lm(coll$Apps~Accept+Enroll+I(Enroll^.8)+Top10perc+I(Top10perc^.13)+Top10perc+I(Top25perc^8.5)+Outstate+I(Outstate^-25)+Room.Board+Expend+I(Expend^0.14)+Grad.Rate+I(Grad.Rate^-0.2)+I(Enroll^0.8):Top10perc+I(Top25perc^8.5):Grad.Rate+Accept:Outstate ,data = coll,weights = w2)
anova(reg4,reg7)
summary(reg7)
# Residual standard error: 789.5 on 760 degrees of freedom
# Multiple R-squared:  0.9506,	Adjusted R-squared:  0.9496 
# F-statistic: 914.9 on 16 and 760 DF,  p-value: < 2.2e-16

res = step(reg5,~.^2)
res$anova
reg8=lm(coll$Apps~coll$Top10perc+I(Top10perc^.13)+Outstate+I(Outstate^-25)+Room.Board+Expend+I(Expend^0.14)+Grad.Rate+I(Grad.Rate^-0.2)+Outstate:Grad.Rate+Room.Board:Grad.Rate,data = coll,weights = w3)
anova(reg5,reg8)
summary(reg8)
# Residual standard error: 3487 on 765 degrees of freedom
# Multiple R-squared:  0.1999,	Adjusted R-squared:  0.1884 
# F-statistic: 17.37 on 11 and 765 DF,  p-value: < 2.2e-16

#Finding out how many variables to keep

regfit = regsubsets(coll$Apps~Accept+Enroll+I(Enroll^.8)+Top10perc+I(Top10perc^.13)+Top10perc+I(Top25perc^8.5)+Outstate+I(Outstate^-25)+Room.Board+Expend+I(Expend^0.14)+Grad.Rate+I(Grad.Rate^-0.2)+I(Enroll^0.8):Top10perc+I(Top25perc^8.5):Grad.Rate+Accept:Outstate,data=coll,nvmax = 16)
summary(regfit)
summary(regfit)$bic 
which.min(summary(regfit)$bic)
#the min is comming as 16 so it is good
#summary(regfit)$bic 
#[1] -1634.102 -1627.632 -1661.001 -1881.559 -1980.758 -2020.875 -2014.759 -2008.647
#[9] -2012.238 -2023.901 -2019.317 -2016.999 -2011.666 -2055.164 -2210.902 -2218.585
#> which.min(summary(regfit)$bic)
#[1] 16

regfit = regsubsets(log(coll$Apps)~coll$Top10perc+I(Top10perc^.13)+Outstate+I(Outstate^-25)+Room.Board+Expend+I(Expend^0.14)+Grad.Rate+I(Grad.Rate^-0.2)+Outstate:Grad.Rate+Room.Board:Grad.Rate,data = coll,weights = w3,nvmax = 13)
summary(regfit)
summary(regfit)$bic 
which.min(summary(regfit)$bic)

# summary(regfit)$bic 
# [1]  -81.44505  -79.52282  -98.13066  -91.50100 -110.33961 -117.38270 -114.54724
# [8] -111.65239 -105.20373  -98.61776  -93.40252
# > which.min(summary(regfit)$bic)
# [1] 6

#now since it came 6 so we will remove I(Expend^0.14) Grad.Rate I(Grad.Rate^-0.2) Outstate:Grad.Rate

newreg8=lm(log(coll$Apps)~coll$Top10perc+I(Top10perc^.13)+Outstate+I(Outstate^-25)+Room.Board+Expend,data = coll,weights = w3)
summary(newreg8)
# Residual standard error: 0.9741 on 770 degrees of freedom
# Multiple R-squared:  0.1832,	Adjusted R-squared:  0.1769 
# F-statistic: 28.79 on 6 and 770 DF,  p-value: < 2.2e-16

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#now sice after testing vif of my best model i am removing the variables whose vif is the most
#my best model based on r2 is reg7
#reg8 even after tuning is not comming good



vif(reg7)

#now at the end i am removing all the vif which is high


bestregmodel=update(reg7,~.-Accept- Enroll-I(Enroll^0.8)-Top10perc -I(Top25perc^8.5)-I(Top25perc^8.5):Grad.Rate - I(Grad.Rate^-0.2)-Grad.Rate-I(Expend^0.14) )
vif(bestregmodel)

# I(Top10perc^0.13)                Outstate         I(Outstate^-25) 
# 2.361749                3.311723                1.070235 
# Room.Board                  Expend I(Enroll^0.8):Top10perc 
# 1.821239                2.233974                3.812534 
# Outstate:Accept 
# 2.789650

summary(bestregmodel)

# Residual standard error: 1154 on 769 degrees of freedom
# Multiple R-squared:  0.8933,	Adjusted R-squared:  0.8923 
# F-statistic: 919.9 on 7 and 769 DF,  p-value: < 2.2e-16




#my best model has these variables

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              3.452e+03  6.768e+02   5.101 4.27e-07 ***
#   I(Top10perc^0.13)       -1.386e+03  4.956e+02  -2.797  0.00529 ** 
#   Outstate                -2.216e-01  1.876e-02 -11.812  < 2e-16 ***
#   I(Outstate^-25)         -4.903e+87  2.022e+87  -2.425  0.01556 *  
#   Room.Board               1.665e-01  5.102e-02   3.264  0.00115 ** 
#   Expend                   3.107e-02  1.193e-02   2.605  0.00936 ** 
#   I(Enroll^0.8):Top10perc  1.843e-01  1.015e-02  18.159  < 2e-16 ***
#   Outstate:Accept          8.736e-05  2.695e-06  32.414  < 2e-16 ***









