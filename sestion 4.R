x=read.csv('C:/Users/aman/Desktop/R/Session 4/session 4/data/admission.csv')
attach(x)

#mean test

tstat=mean(GMAT)
str(x)

f1= function()
{
s1=rnorm(85,mean=510,sd=sd(GMAT))#we are describing population with sd=samp sd,mean=hypotheseis
stat=mean(s1)#nowwe calculate the mean of the sample

}
sdidt=replicate(10000,f1())# we are taking samples from the hypothesised population
plot(density(sdidt))#plot of all the sample from the hythesised population

abline(v=tstat,col='red')
abline(v=510+510-tstat,col='red')
y=sdidt[sdidt<=tstat|sdidt>=510+510-tstat]#subset of sdidt with both the side of normal 
y
pvalue=length(y)/length(sdidt)#we find the probability of both the side
pvalue
#median test
g=median(GMAT)
#shape =n, mean=500(mean and median are same in normal)sd=sample sd

f1= function()
{
  s1=rnorm(85,mean=500,sd=sd(GMAT))#we are describing population with sd=samp sd,mean=hypotheseis
  stat=median(s1)#nowwe calculate the mean of the sample
  
}
sdidt=replicate(10000,f1())# we are taking samples from the hypothesised population
plot(density(sdidt))#plot of all the sample from the hythesised population

abline(v=g,col='red')
abline(v=500+500-g,col='red')
y=sdidt[sdidt<=g|sdidt>=500+500-g]#subset of sdidt with both the side of normal 
pvalue=length(y)/length(sdidt)
#sd=78

tstat=mean(GMAT)

f1= function()
{
  s1=rnorm(85,mean=tstat,sd=78)#we are describing population with sd=samp sd,mean=hypotheseis
  stat=sd(s1)#nowwe calculate the mean of the sample
  
}
sdidt=replicate(10000,f1())# we are taking samples from the hypothesised population
plot(density(sdidt))#plot of all the sample from the hythesised population

abline(v=sd(GMAT),col='black')
abline(v=78-(sd(GMAT)-78),col='red')
y=sdidt[sdidt>=sd(GMAT)|sdidt<=78-(sd(GMAT)-78)]#subset of sdidt with both the side of normal 
pvalue=length(y)/length(sdidt)#we find the probability of both the side
pvalue

#75th percentile=600

qnorm(.75,mean = 545,sd=sd(GMAT))#keep playing with mean such that the 75th perctile =600

tstat=quantile(GMAT,probs = .75)
tstat

f1= function()
{
  s1=rnorm(85,mean=545,sd=sd(GMAT))#we are describing population with sd=samp sd,mean=hypotheseis
  stat=quantile(s1,probs = .75)#nowwe calculate the mean of the sample
  
}
sdidt=replicate(10000,f1())# we are taking samples from the hypothesised population
plot(density(sdidt))#plot of all the sample from the hythesised population

abline(v=tstat,col='red')
abline(v=510+510-tstat,col='red')
y=sdidt[sdidt<=tstat|sdidt>=510+510-tstat]#subset of sdidt with both the side of normal 
pvalue=length(y)/length(sdidt)#we find the probability of both the side
pvalue

#interquartile range is 120
#correlation between GMAT and GPA

 tstat=cor(GMAT,GPA)
#is it real hypothesis isthata there is no correlation
#gmat=ramdon gmatwith mean = sample mean, sd of sample
#GPA=ramdon gpa with mean = sample mean, sd of sample
 f1= function()
 {
   
   s1=rnorm(85,mean(GMAT),sd(GMAT))
   s2=rnorm(85,mean(GPA),sd(GMAT))
   s=cor(s1,s2)
   return(s)
   
   #population 
 }
 
 sdist=replicate(10000,f1())
 

 plot(density(sdist))#plot of all the sample from the hythesised population
 
 abline(v=tstat,col='red')#s it is near 

 x=read.csv('C:/Users/aman/Desktop/R/Session 4/session 4/data/Sleep and Money.csv')
 
 View(x)
 attach(x)
 cor(sleep,money)
 
 tstat=cor(sleep,money)
 #is it real hypothesis isthata there is no correlation
 #gmat=ramdon gmatwith mean = sample mean, sd of sample
 #GPA=ramdon gpa with mean = sample mean, sd of sample
 f1= function()
 {
   
   s1=rnorm(85,mean(sleep),sd(sleep))
   s2=rnorm(85,mean(money),sd(money))
   s=cor(s1,s2)
   return(s)
   
   #population 
 }
 
 sdist=replicate(10000,f1())
 
 
 plot(density(sdist))#plot of all the sample from the hythesised population
 
 abline(v=tstat,col='red')
 
 
 
 
 #two sample test..............
 
 y= read.csv('C:/Users/aman/Desktop/R/Session 4/session 4/data/twosample.csv')
 sb1=y[y$group=='Treatment',]
 sb2=y[y$group=='Control',]
 tstat=abs(mean(sb1$score)-mean(sb2$score))
 tstat
 pop=y$score
 f1=function()
 {
 
 x=sample(pop)
 y1=mean(x[1:23])
 y2=mean(x[24:44])
 stat=abs(y1-y2)
 return(stat)
 }
 sdist=replicate(10000,f1())
 
 
 plot(density(sdist))#plot of all the sample from the hythesised population
 
 abline(v=tstat,col='red')
 
 s1=sdist[sdist>=tstat]
 pvalue=length(s1)/10000
 pvalue
 
 #we didnt use two tail becase value cannot be less than 0
 #bootstrap
 #we dont make normal distribution in bootstrap
 # we replicate the sample we donot hythesisize anything
 #confidence intervals
 f1=function()
 {
 s1=rnorm(85,mean = mean(GMAT),sd(GMAT))
 x=mean(s1)
 return(x)
 }
 
 sdist=replicate(10000,f1())
 quantile(sdist,probs = c(.025,1-.025))
 
 #bootstrap
 f1=function()
 {
   
   s1=sample(x=GMAT,size = 85,replace=T)
   x=mean(s1)
   return(x)
 }
 sdist=replicate(10000,f1())
 plot(density(sdist))
 quantile(sdist,probs = c(.025,1-.025))
 
 
 #moments
 #read first 8page from session 5
 
 
   
 }
 
 