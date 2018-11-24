# Model for prebiotic evolution
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), { 
    
    #now make some change in the model
    dM = a*M - b*S*M -eps*M^2
    dS = b*S*M - d*S
    
    return(list(c(dM, dS)))  
  }) 
}  


#The smaller a the larger the cycle (but towards stable point)
p = c(a=0.1, b=0.01, d=0.1, eps=0.001)
s = c(M = 15, S = 15)

s = run(500)
newton(c(M = 8, S = 8), plot=T)
plane(xmax=25, ymax=25, traject=T, vector=T)
s = c(M = 8, S = 12)
f = run(1000, traject=T)

continue(state = f, parms = p, x="eps", y="M", xmin=-0.01, step = 0.0001, positive = T)
