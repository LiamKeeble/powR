

#' Simulation for a linear model
#' @param n sample size
#' @param f effect size
#' @return A summary table of the model results
#' @export

lmSim=function(n,f){
options(scipen=999)
y=c(rnorm(n=n/2, mean=0, sd=1), rnorm(n=n/2,mean=f,sd=1))
condition=c(rep("control",times=n/2),rep("experimental",times=n/2))
m=lm(y~condition)
return(summary(m))

}


#'Simulation for a linear model that returns only p values
#' @param n sample size
#' @param f effect size
#' @return p-value for effect
#' @export

lmP=function(n,f){
options(scipen=999)
y=c(rnorm(n=n/2, mean=0, sd=1), rnorm(n=n/2, mean=f, sd=1))
condition=c(rep("control",times=n/2),rep("experimental",times=n/2))
m=lm(y~condition)
return(summary(m)$coefficients[2,4])
}



#' Average p value simulation for a model function
#' @param mod A model simulation
#' @return P-values across simulations
#' @export


pSim=function(mod){
output=NULL
for (i in 1:100){
output[i]=mod
return(output)
}
}



#' Power simulation
#' @param pSim p value simulation
#' @return Statistical power value for model
#' @export

power=function(pSim){
p=pSim
power=mean(p<0.05)
power
}







