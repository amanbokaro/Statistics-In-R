car=read.csv("C:/Users/aman/Downloads/usedcars.csv")#to read thefile used file
View(car)#to view data set
#ans 1
table(car$year,car$model)#contingency table of year vs model
#In year 2010 Se has the most no of cars
#ans 2
subx=car[car$price>12960 &car$price <21900,]#to make sunset of car with following consition
plot(subx$mileage,subx$price,xlab = 'mileage',ylab = 'price',col='pink',pch=19)#to plot data of that subset with mileage and price
abline(v=mean(subx$mileage),col='red',lwd=5)#to draw a line at the mean
abline(v=quantile(x = subx$mileage,probs = .25),col='green',lwd=5)#to draw a line at the 25 percentile
abline(v=quantile(x = subx$mileage,probs = .8),col='orange',lwd=5)#to draw a line at the 80 percentile
text(30597,18000,labels = "mean",srt=90,col = "black")#to put text for mean
text(21270,18000,labels = "25th percentile",srt=90,col = "black")#to put text for 25th percentile
text(38201,18000,labels = "80 th percentile",srt=90,col = "black")#to put text for 80th percentile


#ans 3
install.packages("dplyr")#install dplyr package
library(dplyr)#to load library
help("dplyr")
arrange(car,desc(year),desc(model),desc(price))#sort the usedcars data frame based on year, model and price (in the descending order).
#ans 4
newcar<-group_by(car,model,transmission)
summarise(newcar,meanprice=mean(price),medianprice=median(price))
#ans 5
car["conservative"]=NA#    Create a column called "conservative"

car$conservative<- ifelse(car$color %in% c('Black','Gray','Silver','White'), TRUE, FALSE)#to make conservative to true for black,gray,white,silver
#%n%is used to compare color
#ans 6
symbols(car$year,car$price,circles =car$mileage)# a symbols plot of year (x-axis) and price (y-axis). The size of the symbol should be determined based on mileage
#cex defines size