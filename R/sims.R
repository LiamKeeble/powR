




#' Simulate experimental data with two conditions and a continuous outcome
#' @param n sample size
#' @param mean1 mean of dependent variable for control group
#' @param var1 standard deviation of dependent variable for experimental group
#' @param var2 standard deviation of dependent variable for experimental group
#' @param mean2 mean of dependent variable for experomental group
#' @export


expCont2=function(n,mean1,var1,mean2,var2){
out1=c(rnorm(n/2,mean1,var1),rnorm(n/2,mean2,var2))
conditions=c(rep("control",n/2),rep("experimental",n/2))
return(data.frame(out1,conditions))
}

#' Simulate experimental data with two conditions, a continuous outcome varible and a participant index
#' @param n sample size
#' @param mean1 mean of dependent variable for control group
#' @param var1 standard deviation of dependent variable for control group
#' @param mean2 mean of dependent variable for experimental group
#' @param var2 standard deviation of dependent variable for experimental group
#' @param ind number of participants
#' @export

expCont2Rand=function(n,mean1,var1,mean2,var2,ind){
out1=c(rnorm(n/2,mean1,var1),rnorm(n/2,mean2,var2))
conditions=c(rep("control",n/2),rep("experimental",n/2))
x=n/ind
index=rep(1:ind,x)
return(data.frame(out1,conditions,index))
}

#' Simulate experimental data with two conditions and a binomial outcome
#' @param n sample size
#' @param prob1 Probability of success for control group
#' @param prob2 Probability of success for experimental group
#' @export

expBin2=function(n,prob1,prob2){
out1=c(rbinom(n/2,1,prob1),rbinom(n/2,1,prob2))
conditions=c(rep("control",n/2),rep("experimental",n/2))
return(data.frame(out1,conditions))
}


#' Simulate experimental data with two conditions and a binomial outcome
#' @param n sample size
#' @param prob1 Probability of success for control group
#' @param prob2 Probability of success for experimental group
#' @param ind number of participants
#' @export

expBin2Rand=function(n,prob1,prob2,ind){
out1=c(rbinom(n/2,1,prob1),rbinom(n/2,1,prob2))
conditions=c(rep("control",n/2),rep("experimental",n/2))
x=n/ind
index=rep(1:ind,x)
return(data.frame(out1,conditions,index))
}


#' Simulate experimental data with three conditions and a continuous outcome
#' @param n sample size
#' @param mean1 mean of dependent variable for experimental group 1
#' @param var1 standard deviation for experimental group2
#' @param mean2 mean of dependent variable for experimental group 2
#' @param var2 standard deviation for experimental group 2
#' @param mean3 mean of dependent variable for experimental group 3
#' @param var3 standard deviation for experimental group 3
#' @export

expCont3=function(n,mean1,var1,mean2,var2,mean3,var3){

out1=c(rnorm(n/3,mean1,var1),rnorm(n/3,mean2,var2),rnorm(n/3,mean3,var3))
conditions=c(rep("group1",n/3),rep("group2",n/3),rep("group3",n/3))
return(data.frame(out1,conditions))
}


#' Simulate experimental data with three conditions, a continuous outcome and a participant index
#' @param n sample size
#' @param mean1 mean of dependent variable for experimental group1
#' @param var1 standard deviation of dependent variable for experimental group1
#' @param mean2 mean of dependent variable for experimental group2
#' @param var2 standard deviation of dependent variable for experimental group2
#' @param mean3 mean of dependent variable for experimental group3
#' @param var3 standard deviation of dependent variable for experimental group 3
#' @param ind number of participants
#' @export

expCont3Rand=function(n,mean1,var1,mean2,var2,mean3,var3,ind){
out1=c(rnorm(n/3,mean1,var1),rnorm(n/3,mean2,var2),rnorm(n/3,mean3,var3))
conditions=c(rep("group1",n/3),rep("group2",n/3),rep("group3",n/3))
x=n/ind
index=rep(1:ind,x)
return(data.frame(out1,conditions,index))
}



#' Simulate experimental data with three conditions and a binomial outcome
#' @param n sample size
#' @param prob1 Probability of success for group 1
#' @param prob2 Probability of success for group 2
#' @param prob3 Probability of success for group 3
#' @export

expBin3=function(n,prob1,prob2,prob3){
out1=c(rbinom(n/3,1,prob1),rbinom(n/3,1,prob2),rbinom(n/3,1,prob3))
conditions=c(rep("group1",n/3),rep("group2",n/3),rep("group3",n/3))
return(data.frame(out1,conditions))
}


#' Simulate experimental data with three conditions and a binomial outcome
#' @param n sample size
#' @param prob1 Probability of success for group 1
#' @param prob2 Probability of success for group 2
#' @param prob3 Probability of success for group 3
#' @param ind number of participants
#' @export

expBin3Rand=function(n,prob1,prob2,prob3,ind){
out1=c(rbinom(n/3,1,prob1),rbinom(n/3,1,prob2),rbinom(n/3,1,prob3))
conditions=c(rep("group1",n/3),rep("group2",n/3),rep("group3",n/3))
x=n/ind
index=rep(1:ind, x)
return(data.frame(out1,conditions,index))
}


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


#' Simulation of margin of error for basic linear model
#' @param n sample size
#' @param f effect size
#' @return margin of error
#' @export

lmME=function(n,f){
options(scipen=999)
y=c(rnorm(n=n/2,mean=0,sd=1), rnorm(n=n/2, mean=f, sd=1))
condition=c(rep("control",times=n/2),rep("experiments",times=n/2))
m=lm(y~condition)
return((confint(m)[2,2]-confint(m)[2,1])/2)
}

#' Simulation of margin of error for binomial generalised linear model
#' @param n sample size
#' @param f effect size
#' @return margin of error
#' @export

glmME=function(n,f){
options(scipen=999)
y=c(rbinom(n/2,1,0.5),rbinom(n/2,1,f))
condition=c(rep("control",times=n/2),rep("experimental",times=n/2))
m=glm(y~condition, family=binomial)
return((confint(m)[2,2]-confint(m)[2,1])/2)
}


#' Precision simulation for basic linear model with a two level categorical predictor
#' @param n sample size
#' @param f effect size
#' @param iter number of iterations of simulation
#' @return Statistical precision of a model
#' @export

precisionlm=function(n,f,iter=100){
output=NULL
for(i in 1:iter){
output[i]=lmME(n=n,f=f)
}
p=output
precision=mean(p)
precision
}


#' Precision simulation for binomial generalised linear model with a two level categorical predictor
#' @param n sample size
#' @param f effect size
#' @param iter number of iterations of simulation
#' @return Statistical precision of model
#' @export

precisionglm=function(n,f,iter=100){
output=NULL
for(i in 1:iter){
output[i]=glmME(n=n,f=f)
}
p=output
precision=mean(p)
precision
}



#' Power simulation for basic linear model with a two level categorical predictor
#' @param n sample size
#' @param f effect size
#' @param iter number of iterations of simulation
#' @return Statistical power value for model
#' @export

powerlm=function(n,f,iter=100){

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


#' Model simulation for binomial generalised linear model with two level categorical predictor and a random intercept fitted
#' @param n total number of trials
#' @param f effect size
#' @param ind number of participants
#' @return p value for model simulation
#' @export

binGlmPRand=function(n,f,ind){
options(scipen=999)
y=c(rbinom(n/2,1,0.5),rbinom(n/2,1,f))
condition=c(rep("control",n/2),rep("experimental",n/2))
x=n/ind
index=rep(1:ind,x)
m=lme4::glmer(y~condition+(1|index), family=binomial)
return(summary(m)$coefficients[2,4])
}


#' Model simulation for meta analysis random effects model
#' @param n sample size
#' @param f effect size
#' @return p value for model simulation
#' @export

metaRsim=function(n,f,var){
options(scipen=999)
y=rnorm(n,f,1)
var=rexp(n,var)
m=metafor::rma.mv(y,var,method="REML")
return(summary(m)$pval)
}


#' Power simulation for binomial generalised linear model with two level categorical predictor
#' @param n sample size
#' @param f effect size
#' @param iter number of iterations of simulation
#' @return statistical power value for model
#' @export

powerBinGlm=function(n,f,iter=100){
output=NULL
for (i in 1:iter){
output[i]=binGlmP(n=n,f=f)
}

p=output
power=mean(p<0.05)
power
}

#' Power analysis for binomial generalised linear model with two level categorical predictor and random intercept
#' @param n total number of trials
#' @param f effect size
#' @param ind number of participants
#' @param iter number of iterations of model simulation
#' @return statistical power value for model
#' @export

powerBinGlmRand=function(n,f,ind,iter=100){
output=NULL
for (i in 1:iter){
output[i]=binGlmPRand(n=n,f=f,ind=ind)
}
p=output
power=mean(p<0.05)
power
}


#' Power simulation for random effects meta analysis model
#' @param n sample size
#' @param f effect size
#' @param var variance for meta analysis model
#' @param iter number of iterations of simulation
#' @return statistical power for meta analysis
#' @export

powerMetaR=function(n,f,var,iter=100){
output=NULL
for (i in 1:iter){
output[i]=metaRsim(n=n,f=f,var=var)
}
p=output
power=mean(p<0.05)
power
}



#' Plot distribution of p-values from binomial generalised linear model
#' @param n sample size
#' @param f effect size
#' @param iter number of iterations of simulation
#' @return plot of distribution of p-values
#' @export

powGlmPlot=function(n,f,iter=100){
output=NULL
for (i in 1:iter){
output[i]=binGlmP(n=n,f=f)
}
output=data.frame(output)

plot=ggplot2::ggplot(output, aes(output))+
	geom_density()+
	ggtitle("Density plot of p-values")+
	xlab("P values")+
	ylab("Frequency of values")+
	geom_vline(aes(xintercept=0.05))+
	xlim(0,1)

plot
}




#' Plot distribution of p values from meta analysis random effects model 
#' @param n sample size
#' @param f effect size
#' @param var variance for meta analysis model
#' @param iter number of iterations of simulation
#' @return plot of distribution of p values
#' @export

powMetaPlot=function(n,f,var,iter=100){
output=NULL
for (i in 1:iter){
output[i]=metaRsim(n=n,f=f,var=var)
}
output=data.frame(output)
plot=ggplot2::ggplot(output, aes(output))+
	geom_density()+
	ggtitle("Density plot of p-values")+
	xlab("P values")+
	ylab("Frequency of values")+
	geom_vline(aes(xintercept=0.05))+
	xlim(0,1)
	
plot
}



#' Plot p-values from basic linear model simulations
#' @param n sample size
#' @param f effect size
#' @param iter number of iterations of simulation
#' @return plot of p values
#' @export

powLmPlot=function(n,f,iter=100){

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
		geom_vline(aes(xintercept=0.05))+
		xlim(0,1)
	plot

}




#' Plot p-values for generalised binomial linear model with a single binomial predictor and a random intercept
#' @param n total number of trials
#' @param f effect size
#' @param ind number of participants
#' @param iter number of iterations of simulation
#' @return distribution plot of p-values
#' @export

powGlmRandPlot=function(n,f,ind,iter=100){
output=NULL
for (i in 1:iter){
output[i]=binGlmPRand(n=n,f=f,ind=ind)
}
output=data.frame(output)
plot=ggplot2::ggplot(output,aes(output))+
	geom_density()+
	ggtitle("Density plot of p values")+
	xlab("P values")+
	ylab("Frequency of value")+
	geom_vline(aes(xintercept=0.05))+
	xlim(0,1)
plot
}






