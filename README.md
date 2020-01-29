# Health and Wealth
<p align="center">
  <img src="./Images/Figure.png" height="400">
</p>

# Overview
This repository was written in 2018-2020. It examines the relationship between health and wealth using the Panel Study of Income Dynamics. It contributes to three interrelated projects, all of which examine how the health wealth relationship is mediated by one's race, asset allocation, and socio-economic strata with longitudinal modeling approaches (e.g. fixed effects models, mixed effects/growth models).

# Usage
This repository was built only for personal use, so it requires a few steps for migrating to a new computer. New users must first:
- Download all years of the PSID family files, cross sectional individual files, and auxiliary wealth files.
- Change all directories to those on the user's disk.
- Install all required libraries used in the repository.
- Run all scripts within the "Code" directory in the order of their numerical prefixes. Scripts without a numerical prefix should be run after scripts with numerical prefixes are run. 

# Details
Below is a brief overview of what each script in the `Code` folder.

- **01 - Prep data.R**
    - Unzips raw PSID data.
    - Imports data into R format.
    - Gives longitudinal variables consistent names.
    - Merges wealth files into family files.
- **02 - Prepare family variables.R**
    - Creates longitudinal dataset with select variables from each wave of the family files.
    - Reformats changes in variables over time.
    - Renames variables.
- **03 - Prepare individual variables.R**
    - Cleans and accounts for changes in individual-level variables over time.
- **04 - Merge individual and family vars.R**
    - Merges individual and family-level datasets.
    - Changes spouse-level variables to consistent names that correspond to individual-level data.
- **05 - Reduce and rename variables.R**
    - Reduce sample to key variables.
    - Give variables more intuitive names.
- **06 - Inflate financial variables.R**
    - Inflate financial variables to 2015 USD with Consumer Price Index
- **07 - Prep Final variables for models.R**
    - Create lagged variables.
    - Account for censorship of deceased.
    - Transform wealth variables.
    - Make final tweaks to clean data.
- **08 - Subset data.R**
    - Create 1984-2015 and 2005-2012 versions of dataset.
    - Remove rows based on race, age, and number of person-observations.
- **09 - Run models.R**
    - Run 7 nested mixed effects models (poisson, cumulative-link, and gamma), for predicting 6 dependent variables 
    - Format results for publication
- **10 - Run model with standardized variables**
    - Run models with standardized versions of wealth variables
- **Ap1 - Dropped cases by subset condition**
    - Produce table indicating sample size by sample inclusion criteria.
- **Ap2 - Descriptives by missing health**
    - Produce table indicating descriptive statistics for individuals with missing data on health.
- **Ap3 - BW health gaps by age**
    - Show predicted health for blacks and whites by whether model controls for wealth components.
- **Ap4 - Models - wealth variants**
    - Run models with different wealth transformations
- **Ap5 - Models - negative wealth variants**
    - Run models by whether total wealth is positive or negative.
- **Ap6 - Models - distress**
    - Run models for distress with different link functions (poisson, zero-inflated negative binomial, and gamma).
- **Ap7 - Models - 1984**
    - Run models for all years since 1984
- **Ap8 - Parameter estimates of all IVs**
    - Visualize parameter estimates for all independent variables
- **Fig 1 - Racial disparities of wealth**
    - Generate density plot showing racial disparites by wealth.
- **Fig 2 - Parameter estimates of key IVs**
    - Visualize parameter estimates for wealth components by dependent variable.
- **Fig 3 - Predicted health by savings**
    - Visualize predicted health by race and savings.
- **Tab1 - Descriptive statistics**
    - Produce table showing descriptive statistics for all modeled variables.
