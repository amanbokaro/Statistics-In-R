#Question 1
#Create a vector called x of 1000 random normal values (rnorm() function) and another vector called y with 1000 random uniform values (runif()). 
#Create a plot of x and y. Use pch value of 7, add labels to the x and y axis - "normal" and "uniform". Plot the points in color blue.

x= rnorm(1000)# Aman Kumar x contains 1000 random normal numbers for which we use rnorm(no of number we wnat) 

y=runif(1000)# y contains 1000 random uniform numbers for which we use runif(no of number we wnat)

plot(x,y,col="blue",pch=7,xlab = "normal",ylab = "uniform")#plot function plot the x and y which contains normal and uniform numbers
#in addition to it we use 'pch' to giving shape to symbols and 'col' to give colour to symbols
#xlab is used to give labels to x axis
#ylab to give labels to y axais

#Question 2(a)
#Write the expression in R to repeat x, y, and z as follows Repeat 6 times as follows
#"x" "y" "z" "x" "y" "z" "x" "y" "z" "x" "y" "z" "x" "y" "z" "x" "y" "z"

#ans:
repeat1= rep(factor(c('x','y','z')),6)#vector repeat1 keeps the following ouput;We use function rep for repition;inside rep function we use c() or combine function to combine xy and z;
#the rep function takes factor input so we use factor function on C(x,y,z) vector ;the second argument of rep is number of time to repeat.

repeat1#this gives the ouput or the elements in repeat1 function

# Question 2(b)  "x" "x" "x" "x" "x" "y" "y" "z" "z" "z"

repeat2= rep(factor(c('x','y','z')),c(5,2,3))#vector repeat2 keeps the following ouput;We use function rep for repition;inside rep function we use c() or combine function to combine xy and z;
#the rep function takes factor input so we use factor function on C(x,y,z) vector ;the second argument of rep is number of time to repeat for that we use c() or combine function with argumens as 5,2,3beacuse we want different output of each x,y,z.

repeat2#this gives the ouput or the elements in repeat1 function

