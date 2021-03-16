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

'''
devtools::install_github("LiamKeeble/powR")

'''


# Dependencies

At the moment, there are no dependencies for this package.

However, the package will likely require the packages 'lme4' and 'metafor' in the future.


