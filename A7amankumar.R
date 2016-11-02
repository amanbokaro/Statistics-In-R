#ans 1
t1=runif(10000,min = 5,max = 9)#task 1 can be completed in unform from 5 to 9
t2=rnorm(10000,mean = 7,sd = 2)#task 2 can be completed in normal distribution meaan= 5 to sd=9
t3=rexp(n = 10000,rate = 8)#task 3 can be completed in exponential with rate =8
t4=runif(10000,min = 3,max = 10)#task 4 can be completed in unform from 3 to 10
t5=t1+t2#sum of completting t1 and t2
t6=t3+t4#sum of completting t3 and t4
totaltime=ifelse(t5>t6 , t5,t6)#if sum of t1 and t2 completes first then the total task completion will take sum of t3 and t4 and viceversa 
#ans a
m=mean(totaltime)#mean of total time
m

me=median(totaltime)#median of total time
me
#ans b
pvalue12=length(totaltime[totaltime<=12])/10000# p value of the task to complete less then 12 min
pvalue12
#ans c
plot(density(totaltime))#plot of the total time completion density

#ans2

a=95*30#TOTAL AMOUNT into stock a
b=15*100#TOTAL AMOUNT into stock b
c=25*25#TOTAL AMOUNT into stock c
d=50*50#TOTAL AMOUNT into stock d
ra=rnorm(10000,mean = 1.1*a,sd = .05*a)#sum of investment and return from stock a
rb=runif(10000,min = .9*b,max = 1.15*b)#sum of investment and return from stock b
rc=runif(10000,min = .85*c,max = 1.25*c)#sum of investment and return from stock c
rd=rnorm(10000,mean =1.2*d ,sd =.3*d )#sum of investment and return from stock d
rturn=ra+rb+rc+rd-a-b-c-d#return from all the stock we are subtarcting total investment
#ans a
mean(rturn)#total expected profit
#ans b
median(rturn)#median of profit
sd(rturn)#sd of the profit
#ans c
y=rturn[rturn>=100]#the transaction fee for purchasing these stocks is $100 then probability that you don't lose money
pvalue=length(y)/10000
pvalue
z=rturn[(rturn-100)>=1500]
pvalue2=length(z)/10000
pvalue2#the probability you will make at least $1500 profit

#ans 3
va=c(796/7823*100, 541/5611 *100,533/5092 *100,1001/16407 *100,416/4072 *100,268/2802 *100,323/3277 *100,808/8159 *100,517/5331 *100,542/5217 *100,1099/15922 *100,415/4360 *100)
#'conversion rate' - percentage of visits that result in completion of Version a
vb=c(289/2910 *100,262/3049 *100,298/2775*100,191/3266 *100,188/1980 *100,129/1512 *100,134/1408*100,258/2709 *100,258/2802 *100,272/2720 *100,205/3119 *100,182/2091 *100)
#'conversion rate' - percentage of visits that result in completion of Version b b
diff=abs(mean(va)-mean(vb))
#diff between mean of conversion rate of a and b
both=c(796/7823*100, 541/5611 *100,533/5092 *100,1001/16407 *100,416/4072 *100,268/2802 *100,323/3277 *100,808/8159 *100,517/5331 *100,542/5217 *100,1099/15922 *100,415/4360 *100,
289/2910 *100,262/3049 *100,298/2775*100,191/3266 *100,188/1980 *100,129/1512 *100,134/1408*100,258/2709 *100,258/2802 *100,272/2720 *100,205/3119 *100,182/2091 *100)
#'conversion rate' - percentage of visits that result in completion of Version a and b

twotest=function()#we are taking two sample test
{
  s1=sample(both)
  y=mean(s1[1:12])-mean(s1[13:24])# finding the mean difference between samples
  
}
sdid=replicate(10000,twotest())#the function f1 is replicated
plot(density(sdid))#ploting the density of the difference
abline(v=diff,col='red')
abline(v=mean(sdid)-diff-mean(sdid))
p=sdid[sdid>=diff|sdid<=mean(sdid)-diff-mean(sdid)]
pvalue=length(p)/10000# calculating p value 
pvalue#based on the pvlue can be concluded that on the null hypothesis ie there is no difference between version a and version b is true and null hypothesis cannot be rejected


#ans4
heal=read.csv('C:/Users/aman/Desktop/R/Health.csv')#reading the file
tstat=mean(heal$ofp)#mean office visit of sample
tstat
str(heal)
f1=function()
{
  
  s1=rpois(n = 4406,lambda = 5.5)
  y=mean(s1)
  
  
}
stat=replicate(10000,f1())# making the population with mean =5.5

plot(density(stat))
abline(v=tstat,col='red')
st =subset(x = stat,stat>=tstat|stat<=(5.5-(tstat-5.5))) 

pvalue=length(st)/10000#finding the length in the population that has population withbmean office visit =5.5
pvalue# based on the p value which is zero the hypothesis that the mean number of office visits is 5.5 is false 


#ans 5

wal=read.csv('C:/Users/aman/Desktop/R/Walmart.csv')#reading walmart dataset
str(wal)#structure of Walmart adatset
View(wal)
wl= subset(x = wal,wal$Store==1&wal$IsHoliday==FALSE)#subseting data with stire =1 and is not a holiday
#normality for mean

f1=function()
{
  s1=rnorm(48789,mean=mean(wl$Weekly_Sales),sd(wl$Weekly_Sales))#generating population with mean same a sample and sd same as sample
  m=mean(s1)#returning mean of each sample
}
sdid=replicate(10000,f1())#replicating the function 10000 time to make it a population
quantile(sdid,probs = c(.025,1-.025))#confidence interval of mean considerin normality

#normality for median

f2=function()
{
  s1=rnorm(48789,mean=mean(wl$Weekly_Sales),sd(wl$Weekly_Sales)) #generating population with mean same a sample and sd same as sample weekly sales
  m=median(s1)#returning median of each sample
}
sdid=replicate(10000,f2())#replicating the function 10000 time to make it a population
quantile(sdid,probs = c(.025,1-.025))#confidence interval of median considerin normality
#normality for sd

f3=function()
{
  s1=rnorm(48789,mean=mean(wl$Weekly_Sales),sd(wl$Weekly_Sales))#generating population with mean same a sample and sd same as sample weekly sales
  m=sd(s1)#returning sd of each sample
}
sdid=replicate(10000,f3())#replicating the function 10000 time to make it a population
quantile(sdid,probs = c(.025,1-.025))#confidence interval of sd considering normality

#bootstrap for mean

bootstrap1=function()
{
 s1= sample(x = wl$Weekly_Sales,size = 48789,replace = T)#population is made using sample function from the sample we have got
  mean(s1)#return of mean from bootstsrap
  
}
sdid=replicate(10000,bootstrap1())#generating population based on bootstrap
plot(density(sdid))
quantile(sdid,probs = c(.025,1-.025))#confidenece interval of mean considering bootstrap 

bootstrap2=function()
{
  s1=sample(wl$Weekly_Sales,size = 48789,replace = T)#population is made using sample function from the sample we have got
  median(s1)#return of median from bootstsrap
  
}
sdid=replicate(10000,bootstrap2())#generating population based on bootstrap
plot(density(sdid))
quantile(sdid,probs = c(.025,1-.025))#confidenece interval of median considering bootstrap 

bootstrap3=function()
{
  s1=sample(wl$Weekly_Sales,size = 48789,replace = T)#population is made using sample function from the sample we have got
  sd(s1)#return of sd from bootstsrap
  
}
sdid=replicate(10000,bootstrap3())#generating population based on bootstrap
plot(density(sdid))
quantile(sdid,probs = c(.025,1-.025))#confidenece interval of sd considering bootstrap 


#ans 6

lessthan60=c( 75 ,77, 80 ,69 ,73 ,76, 78, 74, 75, 81, 75, 80)#the weight of french fries (grams) for the less than 60 groups of customers
greaterthan60=c(68 ,74 ,77 ,71 ,73, 75 ,80, 77 ,78 ,72, 69 ,71)#the weight of french fries (grams) for the greater than 60 groups of customers
sub=abs(mean(lessthan60)-mean(greaterthan60))#mean difference of wt of both 
both=c(75 ,77, 80 ,69 ,73 ,76, 78, 74, 75, 81, 75, 80,68 ,74 ,77 ,71 ,73, 75 ,80, 77 ,78 ,72, 69 ,71)
#creating sample that is a mixture of both groups
twosample=function()#conducting two sample test
{
  s1=sample(both)#taking samples from both the group at random to create the population
  y=abs(mean(s1[1:12])-mean(s1[13:24]))#finding the diffrence between the both random samples of size 12 by considering first group as less than 60 and other greater than 60 
  
  
}
sdid=replicate(10000,twosample())#replicating twosample to creater population
plot(density(sdid))#plot of the density of population
abline(v=sub,col='red')
z=subset(x = sdid,sdid>sub)
pvalue=length(z)/10000#calculating the pvalue
pvalue#based on the p value we can consider that is there is no discrimation for the elderly and we cannot reject the null hypothesis which consider no diffrence between two population

