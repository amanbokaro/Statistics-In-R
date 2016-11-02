#ASSIGNMENT 5


x=seq.int(from = 1,to = 100,by = 1)#seqint gives sequebce from 1 to 100 by an increment of 1
y=sqrt(x)#sqrt keeps the square root of all the elements in x
x#to view vales in x
y#to view vales in x
plot(x, y, type="l")#plots x versus y type = l draws line fraph
help("expression")#to see how expression function works
xlabel=expression(plain(E)==m *c^2 )#xlabel contans expression E=mc^2 whre plain E use character E
#expression  plain(E)==m *c^2 is used to produce text labelE=mc^2

text(40,2,xlabel)#text function takes argument asx ,y which is position where we want to 
#print the expression and the third argument is the expression which we want to print

ylabel=expression(plain(y)==alpha[0]+alpha[1]*x+alpha[2]*x^2)#ylabel contans expression E=mc^2 whre plain y use character y
#expression alpha[0]+alpha[1]*x+alpha[2]*x^2  used to produce text label

text(15,8,ylabel)#text function takes argument as x ,y which is position where we want to 
#print the expression and the third argument is the expression which we want to print