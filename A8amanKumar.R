install.packages('moments')
library(moments)

ad=read.csv('C:/Users/aman/Desktop/R/admit.csv')#reading data
y=kurtosis(ad$gre)#kutosis of sample
str(ad)
test=function()
{
  sis=rnorm(100,mean(ad$gre),sd(ad$gre))#defining population as normal distribution and returning its kutosis
   k=kurtosis(sis)#returning kurtosiis of each sample
}
   pop=replicate(1000,test())#creating population by replicating test function
   plot(density(pop))
   abline(v=y,col='red')#line to test the kurtosis of sample
   abline(v=(3+(3-y)),col='green')#line to test the kurtosis of sample
   z=pop[pop<=y|pop>=(3+(3-y))]#the normaal has kurtosis of 3
   pvalue=length(z)/1000#calculating pvalue
   pvalue#based on the p value we cannot reject null hypotheseis ie the population is a nornal distribution with kurtosis of 3