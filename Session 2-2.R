library(MASS)
attach(whiteside)
stripchart(Gas=Insul,col="orange",pch=19,method = stack)
x=C('USA','UK','Germany','France')
hist(Gas)
hist(Gas,probability = T,breaks=10)
plot(density(Temp))
polygon(density(Temp),col = 'yellow')
data()
library(car)
attach()
x1=rnorm(10000)
y1=rnorm(10000)
plot(x1,y1,pch=19)#lots ofoverlaps
smoothScatter(x1,y1)#it sis better to use smoothscatter
x1=runif(10000)
y1=runif(10000)
plot(x1,y1,pch=19)
smoothScatter(x1,y1)
View(Boston)
plot(Boston$ptratio,Boston$tax)
sunflowerplot(Boston$ptratio,Boston$tax)#thisgive the concentation of ponts the more the flower leaves means more conc if 3 then 3 points are there
install.packages("rgl")
library(rgl)
plot3d(Boston$ptratio,Boston$tax,Boston$medv,col = 'orange',size = 10)
install.packages("plotrix")
library(plotrix)
x=c("USA",'UK','')
fan.plot(Boston$ptratio,labels = Boston$tax)#alternative to piechart
install.packages("vioplot")#alternative to boxplot
library(vioplot)
vioplot(whiteside$Temp)#DENSITY + MEDIANinst
install.packages("corrgram")#to get correlation between diffrent columns

library(corrgram)
cor(Boston)
corrgram(Boston,order = T)#blue positive and red is negative correlation darker stronger the correlation
#order orders it and makes lot of sence
corrgram(Boston,order = T,upper.panel = panel.conf)#used to get correlation numbers


