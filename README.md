CPOP
================

[![Travis build
status](https://travis-ci.org/kevinwang09/top.svg?branch=master)](https://travis-ci.org/kevinwang09/top)
[![Codecov test
coverage](https://codecov.io/gh/kevinwang09/CPOP/branch/master/graph/badge.svg)](https://codecov.io/gh/kevinwang09/CPOP?branch=master)

# CPOP - Cross-Platform Omics Prediction

Due to data scale differences between multiple omics data, a model
constructed from a training data tends to have poor prediction power on
a validation data. While the usual bioinfomatics approach is to
re-normalise both the training and the validation data, this step may
not be possible due to ethics constrains. CPOP avoids re-normalisation
of additional data through the use of log-ratio features and thus also
enable prediction for single omics samples.

The novelty of the CPOP procedure lies in its ability to construct a
**transferable** model across gene expression platforms and for
prospective experiments. Such a transferable model can be trained to
make predictions on independent validation data with an accuracy that is
similar to a re-substituted model. The CPOP procedure also has the
flexibility to be adapted to suit the most common clinical response
variables, including linear response, binomial and multinomial response
and Cox PH models.

# Vignette

# Installation

``` r
devtools::install_github("kevinwang09/CPOP")
```

# References

This is unpublished work, please do not distribute.
