# Cytokine regulation
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), { 
    
    dX = k3*(Y-X)*(a+X^2) - k2*X
    dY = eps*(k1-k2*X)
    
    return(list(c(dX, dY)))  
  }) 
}  


#The smaller a the larger the cycle (but towards stable point)
p = c(k1=1, k2 =1, k3=1, eps=0.1, a = 0.01)
s = c(X = 2, Y = 2)

run(1000)
newton(s=c(X = 2, Y =2 ), plot=T)
plane(xmax=100, ymax=500, traject=T, vector=T)
s = c(X = 2, Y = 2)
f = run(traject=T)
s= c(X = 2, Y = 3 )
f = run(traject=T)
s= c(X = 0.5, Y = 5 )
f = run(traject=T)




continue(state = f, parms = p, x="a", y="X", xmin=-0.01, step = 0.0001, positive = T)
