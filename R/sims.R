
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
#' @param mod Model simulation
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



#' Simulation of p values from analysis
#' @param mod model simulation
#' @return vector of p values from simulation
#' @export

pSim=function(mod){
output=NULL
for (i in 1:100){
output[i]=mod
}
}



#' Plot p-values and average p-value
#' @param pSim p value simulation
#' @return plot of p values
#' @export

powPlot=function(pSim){
	
ggplot2::ggplot(pSim, aes(pSim))+
geom_density(kernel="gaussian")

abline(v=0.05,col="red")

}












