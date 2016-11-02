#ans 1

hypothesis=function(b,l)#creating function for hypothesis 
{
  hypo=c("h")#vector for storing hypothesis
  hypo['h']=l^2+b^2#calculation for calculating hypothesis
  return(hypo)#returns hypothesis
  
}
#ans 2

meandiffrence=function(d=c(1,2,3))# function that takes all values below the mean and calculates the median of these, (2) takes all values above the mean and calculates the median of these, and (3) return the difference in the value between the higher and the lower median
{
  b=mean(d)#calculate the mean of vector in the argument
  i=1
  e=c()#initialization a vector e
  f=c()#initialization a vector f
  while (i<=length(d))#loop from i=1 to length of argument vector
  {
  if (d[i]<b)#if the element is less than mean than store the element in e
    e=d[i]
  else
    f=d[i]#if the element is greater than mean than store the element in f
  i=i+1
  }
  
g=abs(median(f)-median(e))#absolute diffrence between the medians of e and f
return(g)#return g ie the absolute diffrence between the medians of e and f
  
  
}
meandiffrence(c(1,2,3,4,5,6))
#ans 3

Runingmedian=function(a=c(1,2,3))#function to create running median with default argument
{
  i=1
  j=1
  e=c()

  while(j<=length(a))#loop run till the length of vector passed
  {
    
    e[j]=median(a[i:j])#calculate median from 1st element to jth element
    j=j+1#increment of j 
  }
  return(e)#retuns vector e
  
}

Runingmedian()
#ans 4

install.packages("schoolmath")#install package school math

library(schoolmath)#loading package

noofprime = function(a=1,b=1)#function to calcualte no of prime no between the arguments
{
  d=0
 if (a<b)#if a is less tan b then wewill increment loop from a to b
 {
   while(a<b){
     if (is.prim(a+1)=='TRUE')#isprime frunction returns true if the no is prime
       d=d+1#d act as a counter
     a=a+1#increment a
   }
 }
  else#else if a>b then we would increment from b to a
  {
    while(a>b)
    {
     
    
      if (is.prim(a)=='TRUE')#isprime frunction returns true if the no is prime
        d=d+1#d act as a counter
      a=a-1#decrement a
      
    }
  }
  
 return(d) #returns counter ir no of prime nos
  
}

noofprime(10,1)

#ans 5

rolldice= function()#create function with rolldice
{
s=c(1,2,3,4,5,6)#vector that contains all thse sidesof dice

x=sample(s, 1, replace=T)#x contains a value if we roll a dice
y=sample(s, 1, replace=T)#y contains a value if we roll another dice
paste('The two values are' ,x ,'and',y,', so the sum of them is',x+y)#gives the output as The two values are 3 and 6, so the sum of them is 9
}
rolldice()

