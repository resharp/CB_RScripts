# Cytokine regulation
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), { 
    
    dX = a1*X-b1*X^2-c1*X*Y
    dY = a2*Y-b2*Y^2-c2*X*Y

    return(list(c(dX, dY)))  
  }) 
}  


#The smaller a the larger the cycle (but towards stable point)
p = c(a1=0.2, a2=0.2, b1=0.3, b2=0.3, c1=0.2, c2=0.22)
s = c(X = 1, Y = 2)

s = run(1000)
newton(s=c(X = 2, Y =2 ), plot=T)
plane(xmax=2, ymax=2, traject=T, vector=T)
s = c(X = 2, Y = 2)
f = run(traject=T)
s= c(X = 2, Y = 3 )
f = run(traject=T)
s= c(X = 0.5, Y = 5 )
f = run(traject=T)




continue(state = f, parms = p, x="a", y="X", xmin=-0.01, step = 0.0001, positive = T)
