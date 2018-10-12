# statmodels

Functions I developed as part of my MSc. thesis 'Modelling Pesticide Biodegradation Using Regulatory Data'. Most are simply  wrappers to make model fitting more efficient. Dataset is from the EAWAG-Soil package of envipath, see https://envipath.org/wiki/index.php/EAWAG-SOIL_package

## crossvalidation
Functions for internal cross-validation (10-fold and Leave-One-Out) of lm model objects. Output R2, adjR2 (where possible), RMSE, and MAE.

## myplsr
Running partial least squares regression using {pls} by Bjorn Mevik. Wrapper functions to group together some modelling steps for quicker pls regression model fitting.

## marginals
Based on {ggplot}, plot and facets the marginal distributions of the enumerated variables of a dataset.

## unadjR2
Calculate the unadjusted R2 value. Gam objects derived using {mgcv} give adjusted R2 by default in model outputs.
