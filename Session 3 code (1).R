NumericPred <- read.csv("C:/Users/rgopal/Desktop/Session 3/data/NumericPred.csv")
colnames(NumericPred) = c("target","model1","model2")
View(NumericPred)

a = NumericPred$target
m = NumericPred$model1

nummetrics = function(a,m)
{
  metrics = c(MAD=0,MSE=0,MAPE=0,MPSE=0,R2=0,MAD_Trim=0)
  metrics["MAD"] = mean(abs(a-m))
  metrics["MSE"] = mean((a-m)^2)
  metrics["MAPE"] = mean(abs((a-m)/a))
  metrics["MPSE"]  = mean(((a-m)/a)^2)
  SST = sum((a-mean(a))^2)
  SSE = sum((a-m)^2)
  metrics["R2"] = 1 - (SSE/SST)
  metrics["MAD_Trim"] =mean(abs(a-m),trim = 0.1)
  return(metrics)
}

nummetrics(a=NumericPred$target,m=NumericPred$model1)
nummetrics(a=NumericPred$target,m=NumericPred$model2)

nummetrics
args(nummetrics)

NumericPred$baseline = mean(a)
View(NumericPred)


nummetrics(a=NumericPred$target,m=NumericPred$baseline)
nummetrics(a=NumericPred$target,m=NumericPred$model1)
nummetrics(a=NumericPred$target,m=NumericPred$model2)


ensemble = lm(NumericPred$target~NumericPred$model1+NumericPred$model2)
ensemble
NumericPred$model3 = -1.1136+(0.2994*NumericPred$model1)+(0.7504*NumericPred$model2)



plot(NumericPred$target,NumericPred$model3,pch=19,col="blue")
lines(NumericPred$target,NumericPred$target,lwd=5,col="orange")

a = NumericPred$target
m = NumericPred$model3
cost = ifelse(abs(a-m)<5,0,2*abs(a-m))
sum(cost)

s1 = NumericPred[abs(NumericPred$target-NumericPred$model3)>5,]
points(s1$target,s1$model3,pch=19,col="red")












