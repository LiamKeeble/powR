#' The powR R package is currently still under development.
#' Functions for different model simulations are being created.
#' 

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




#' Simulation for binomial generalised linear model



#' Simulation for binomial generalised linear model that only returns p values



#' Simulation of poisson generalised linear model


#' Simulation os Poissons generalised linear model that only returns p values


#' Simulation of lm with random effects variable

#' Simulation of lm with random effects returning only p values


#' Simulation of binomial glm with random effects variable

#' Simulation of binomial glm with random effects variable returning only p values

#' Simulation of poisson glm with random effects

#' Simulation of poisson glm with random effects returning only p values


#' Power simulation from model simulation
#' @param Model simulation
#' @return Statistical power value for model
#' @export

power2=function(mod){
output=NULL
for(i in 1:100){
output[i]=mod
}
p=output[i]
power=mean(p<0.05)
power
}











