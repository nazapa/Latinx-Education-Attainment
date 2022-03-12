################################################################################
##
## [ PROJ ] < Latinx Education Attainment >
## [ FILE ] < R Script >
## [ AUTH ] < Paula Nazario / @nazapa >
## [ INIT ] < February 27, 2022 >
## 
################################################################################

## ---------------------------
## libraries
install.packages("tidyverse")
library("tidyverse")
## ---------------------------

## ---------------------------
## directory paths
data_dir <- file.path('./data_nazario_paula') 
data_dir
## ---------------------------

## -----------------------------------------------------------------------------
## load data sets
census_2014 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2014.csv", skip =4)
census_2015 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2015.csv", skip =4)
census_2016 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2016.csv", skip =4)
census_2017 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2017.csv", skip =4)
census_2018 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2018.csv", skip =4)
census_2019 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2019.csv", skip =4)
census_2020 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2020.csv", skip =4)
census_2021 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2021.csv", skip =4)

## refer to lecture information: 
https://anyone-can-cook.github.io/rclass2/lectures/organizing_and_io/organizing_and_io.html#5_readr_package
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
