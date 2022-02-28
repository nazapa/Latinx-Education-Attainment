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
## load datasets
census_2014 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2014.csv")
census_2015 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2015.csv")
census_2016 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2016.csv")
census_2017 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2017.csv")
census_2018 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2018.csv")
census_2019 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2019.csv")
census_2020 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2020.csv")
census_2021 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2021.csv")

## creating pivot table
mrc_subset <- mrc %>% select(name, par_q1, par_q2, par_q3, par_q4, par_q5)
mrc_pivot <- mrc_subset %>% 
  pivot_longer(cols = starts_with("par_"),
               names_to = "quintile",
               names_prefix = "par_",
               values_to = "fraction",
)
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
