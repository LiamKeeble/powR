# powR

This is an R package in development.

Its aim is to make simulating data, statistical analyses and study power easy.

The package allows users to simulate their statistical analyses and pass 
these simulations through the power2() function, which will provide the 
power of the analysis.

Currently, the only analysis that can be simulated is a basic linear model with a single predictor. Other simulations are currently in development.

Eventually, I hope to allow the user to simulate essentially any analysis and pass it through the power2() function, so that more scientists will be encouraged to simulate their analyses in the study planning stage, before collecting data.

# Installation

The package can be installed in the usual way for an R package in developments:

```
devtools::install_github("LiamKeeble/powR")

```

# Dependencies

At the moment, there is only one dependency necessary for this package: ggplot2 for plotting p values distributions.

However, the package will likely require the packages 'lme4' and 'metafor' in the future.




# Usage 

All functions currently take only two arguments: n= sample size of a proposed study, f = effect size for a proposed study, and iter = number of iterations of model simulations for functions where this is necessary.

Functions currently allow simulation of a linear model with a single predictor that returns a summary table for that analysis, a simulation of a linear model with a single predictor that returns a p-value only for that analysis, power analysis for a analysis involving a linear model with a single predictor, and a density plot for p values of a proposed analysis.





