# powR

This is an R package in development.

Its aim is to make simulating data, statistical analyses and study power easy.

The package allows users to simulate their statistical analyses and the power of their proposed statistical analyses.

Currently, it is only possible to simulate a limited number of different types of analyses, although I am working fast to make more possible.


# Installation

The package can be installed in the usual way for an R package in developments:

```
devtools::install_github("LiamKeeble/powR")

```

# Dependencies

Current dependencies are the ggplot2, lme4 and metafor r packages.


# Usage 

See the 'man' folder for individual functions and their usage. 

# Data simulation

powR can be used to simulate data. The below function simulates a dataset with a sample size of 200, split evenly across 2 conditions. Measuring a standardised continuous outcome variable from the two groups, the 0 represents the average of the first group, and the first 1 indicates the standard deviation of the measures of that variable for group one. The 0.3 and second 1 are the same measures in the same order but for the second group of participants in the dataset.

```
expCont2(200,0,1,0.3,1)
```

For a repeated measures study, an index of participant IDs can be added using the following function, with the number of participants added as an argument alongside the mesures used for the above function.

```
expCont2Rand(200,0,1,0.3,1,20)
```

To simulate data for a study with a binomial variable as the dependent variable of interest, you can use the following function with the sample size, probability of success of the first group and probability of success of the second group as arguments.

```
expBin2(200,0.5,0.8)
```

A participant index can also be added to this function in order to simulate repeated measures data.

```
expBin2Rand(200,0.5,0.8,20)
```



# Model simulation

The following code returns a p value from a simulated linear model with a binary predictor, a sample size of 200 and an effect size of 0.3.

```
lmP(200,0.3)
```

The same can be done for a generalised linear model with a binomial outcome, a sample size of 200 and where the control group has a probability of success of 0.5 and the probability of success of the experiment group is 0.8.

```
binGlmP(200,0.8)
```

A random effects intercept for repeated measures data can be added to the above simulation using the following code.

```
binGlmPRand(200,0.8,20)
```

A p value for a random effects meta analysis model assessing the overall effect sizes of studies with an average effect size of 0.3 and an average standard deviation of 1 can be simulated using the following function.

```
metaRsim(20,0.3,1)
```

# Power simulation


Power analysis can be conducted for each of the above model simulations. To simulate the power of the afforementioned linear model can be assessed with the following code, using 100 iterations of a linear model with a binomial predictor and an effect size of 0.3 and a sample size of 200.

```
powerlm(200,0.3,100)
```

For a binomial generalised linear model the code is:

```
powerBinGlm(200,0.8,100)
```

And for a meta analysis random effects model:

```
powerMetaR(20,0.3,1,100)
```

# Plotting power


