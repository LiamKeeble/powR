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

At the moment, there is only one dependency necessary for this package: ggplot2 for plotting p values distributions.

However, the package will likely require the packages 'lme4' and 'metafor' in the future.




# Usage 

All functions currently take only two to three arguments: n= sample size of a proposed study, f = effect size for a proposed study, and iter = number of iterations of model simulations for functions where this is necessary.

See the 'man' folder for individual functions and their usage.



