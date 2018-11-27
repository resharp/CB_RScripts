# Classical theory of prebiotic evolution

# dX/dt = a1*X*(1-X-Y)*Q - d*X
# 
# dY/dt = a2*Y*(1-X-Y) + a1*(1-Q)*X*(1-X-Y) - d2 * Y


model <- function(t, state, parms) {
  with(as.list(c(state,parms)), { 
    
    dX = a1*X*(1-X-Y)*q - d*X
    dY = a2*Y*(1-X-Y) + a1*(1-q)*X*(1-X-Y) - d2*Y

    return(list(c(dX, dY)))  
  }) 
}  


#The smaller a the larger the cycle (but towards stable point)
p = c(a1=1, a2=0.9, q = 0.92, d=0.1, d2=0.1)
s = c(X=10, Y=2)

s = run(5000)
newton(c(X = 10, Y=0), plot=T)
plane(xmax=2, ymax=2, traject=T, vector=T)

s = c(X=1, Y=1)
f = run(1000, traject=T)

continue(state = f, parms = p, x="q", y="X", xmin=-0.01, step = 0.0001, positive = T)
