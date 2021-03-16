

#' Simulation for a linear model
#' @param n sample size
#' @param f effect size
#' @return A summary table of the model results
#' @export

lmSim=function(n,f){
y=c(rnorm(n=n/2, mean=0, sd=1), rnorm(n=n/2,mean=f,sd=1))
condition=c(rep("control",times=n/2),rep("experimental",times=n/2))
m=lm(y~condition)
return(summary(m))

}


#' Power simulation for a model function
#' @param mod A model simulation
#' @return Value of statistical power
#' @export


powSim=function(mod){
output=NULL
for (i in 1:100){
output[i]=mod
return(output)
}
}





