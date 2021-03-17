
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


#' Power simulation for basic linear model with a two level categorical predictor
#' @param n sample size
#' @param f effect size
#' @param iter number of iterations of simulation
#' @return Statistical power value for model
#' @export

powerlm=function(n,f,iter){

output=NULL
for(i in 1:iter){
output[i]=lmP(n=n, f=f)
}

p=output
power=mean(p<0.05)
power
}


#' Model simulation for binomial generalised linear model with two level categorical predictor
#' @param n sample size
#' @param f effect size (probability)
#' @return p value for model simulation
#' @export

binGlmP=function(n,f){
options(scipen=999)
y=c(rbinom(n/2, 1, 0.5),rbinom(n/2,1,f))
condition=c(rep("control",times=n/2),rep("experimental",times=n/2))
m=glm(y~condition, family=binomial)
return(summary(m)$coefficients[2,4])
}


#' Power simulation for binomial generalised linear model with two level categorical predictor
#' @param n sample size
#' @param f effect size
#' @param iter number of iterations of simulation
#' @return statistical power value for model
#' @export

powerBinGlm=function(n,f,iter){
output=NULL
for (i in 1:iter){
output[i]=binGlmP(n=n,f=f)
}

p=output
power=mean(p<0.05)
power
}

#' Plot p-values from basic linear model simulations
#' @param n sample size
#' @param f effect size
#' @return plot of p values
#' @export

powPlot=function(n,f,iter){

	output=NULL
	for (i in 1:iter){
	output[i]=lmP(n=n,f=f)
	}
	
	output=data.frame(output)	

	plot=ggplot2::ggplot(output,aes(output))+
		geom_density()+
		ggtitle("Density plot of p values")+
		xlab("P values")+
		ylab("Frequency of values")+
		geom_vline(aes(xintercept=0.05))
	plot

}












