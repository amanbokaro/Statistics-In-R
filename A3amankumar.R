#Aman Kumar
#Assignment 3
#Ans 1---------------------------------------------
library(MASS)#load the mass

View(airquality)#view dataset airquality

mean(airquality$Temp)#to find the mean of temp in airquality dataset

median(airquality$Temp)#to find the median of temp in airquality dataset

quantile(airquality$Temp,probs = .75)#to find the 75th quartile of Temp in airquality dataset
#Ans 2---------------------------------------------
s1=airquality[airquality$Month==6,]#to find the subset of airquality dataset with month=6 ie june
#ans 3---------------------------------------------
plot(x = s1$Temp,y = s1$Solar.R,pch=19,col='red',cex=3)#plot month =6 subset with x = temp,y = s3$ solar r
#cex makes the bubbles bigger,pch=19 makes it filled up bubble,col makes it red
#ans 4--------------------------------------------
median(s1$Solar.R)#to find the median of solar r in s1 dataset