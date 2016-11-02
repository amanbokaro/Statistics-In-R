7+89
log(897)
exp()
rnorm(10)# random 10 numbers
rnorm(100)
weight=c(60,72,57,90,95,72)#c stands for concatenate or combine
mean(weight)
var(weight)#variance
sd(weight)#standard deviation
t.test(weight,mu=80)#population mean to be 80 p value is .4 so we cannot reject
height=c(1.75,1.8,1.65,1.9,1.74,1.91)# vector height
mean(height)
length(height)#no of elements
range(height)#smallest and the largest no
median(weight)#median of weight
quantile(height,probs = .75)#gives third quartile
quantile(height,probs = c(.9,.1,.5,.8,.9))#list of quantiles at different levels
plot(x = height,y = weight)#plot height and weight
plot(x = height,y = weight,pch=19,cex=3,col="Orange")#pch 19=dot ,11 gives star,cex increases size of dots,col=in semicolon col name give the color to dot

colors()#list of diffrent colors
bmi=weight/height^2 # abmi vector is formed which is the bmi
lines(height,22.5*height^2,lwd=3,col="red")#line are low level which put into the graph which is highlevel ie plot,lwd for line width
#r missing value are denotedas na
#add more elements to weight vector
weight=c(weight,86)
height=c(height,NA)#adding missing value to height
mean(weight)
mean(height,na.rm = TRUE)#if there is a na then mean comes na if there are missing values then rwont work (na.rn means remove na )
genders=c('M','F','M','F','F','M','F')#put cotes for the single colon
plot(factor(genders),weight)#r doesnot know gender has categorical data so we had to specify it recognizes it as factor
#seq for sequence
#rep for repitition
x=seq(1,100,by = 7)#increment by 7 but by default it increment by 1

x
y=seq(4,840,length.out = 10)#give 10 numbers which are equally spaced
y
x=rep(4,15)#repeate 4 15times
x=rep(c(4,5),15)#repeate the first argument 15 times
x
x=rep(c(4,5),c(15,25))#we repeate 4 15 tiems and 5 25 times
#c bind and r bind
#how to create datafrae
GHW=data.frame(genders,height,weight)
GHW
edit(GHW)# lookat the data in spreadsheet like formatit is readonly on editing
fix(GHW)#here wecan edit the values ie na to 1.8
View(GHW)#in veiw v is capital
class(GHW)#what type of dat it is
dim(GHW)#no of rows and columns


str(GHW)#column name ,type,value
summary(GHW)#in dataframe the categories are regarded are factors
attributes(GHW)
plot(GHW$height,GHW$weight)
library(MASS)#loads the package
library(help= MASS)#give dribstion of packages
data()#list of all the data set that R has
summary(whiteside)
str(whiteside)
dim(whiteside)
help("whiteside")#what is dataset about a complete description
plot(whiteside$Insul,whiteside$Temp)
plot(whiteside$Gas, whiteside$Temp)
View(AirPassengers)
mean(AirPassengers)
var(AirPassengers)