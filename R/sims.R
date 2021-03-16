

#' Simulation for a linear model
#' Arguments: n=sample size, and f=effect size between a control and experiment 
#' group
#' @export

lmSim=function(n,f){
y=c(rnorm(n=n/2, mean=0, sd=1), rnorm(n=n/2,mean=f,sd=1))
condition=c(rep("control",times=n/2),rep("experimental",times=n/2))
m=lm(y~condition)
return(summary(m))

}








