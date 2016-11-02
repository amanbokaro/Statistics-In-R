#Aman Kumar
#Assignment 2
#Ans 1---------------------------------------------
library(MASS)#load the mass

View(survey)#view dataset survey 

mean(survey$Age)#to find the mean of age in survey dataset

median(survey$Age)#to find the median of age in survey dataset

quantile(survey$Age,probs = .75)#to find the 75th quartile of age in survey dataset
#Ans 2---------------------------------------------
s1=survey[survey$Exer=='Freq',]#to find the subset of people with excerise =frequently
#Ans 3---------------------------------------------
median(s1$Age)#to find the median of age in s1 dataset
#ans 4---------------------------------------------
s2=survey[survey$Sex=='Female'& is.na(survey$Sex)== FALSE,]#to create dataset with sex=female and there is a missing value in sex so used is.na=false because it was being included otherwise
table(survey$Sex)
s3=survey[survey$Sex=='Male'& is.na(survey$Sex)== FALSE,]#to create dataset with sex=male and there is a missing value in sex so used is.na=false because it was being included otherwise

plot(x = s2$Wr.Hnd,y = s2$NW.Hnd,pch=19,col='orange',cex=3)#plot female subset x = s2$Wr.Hnd,y = s2$NW.Hnd
#cex makes the bubbles bigger,pch=19 makes it filled up bubble,col makes it orange

plot(x = s3$Wr.Hnd,y = s3$NW.Hnd,pch=19,col='red',cex=3)#plot male subset with x = s3$Wr.Hnd,y = s3$NW.Hnd
#cex makes the bubbles bigger,pch=19 makes it filled up bubble,col makes it red
#----------------------------------------------------------------___________________________-
