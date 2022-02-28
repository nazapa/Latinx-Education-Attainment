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
census_2014 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2014.csv")
census_2015 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2015.csv")
census_2016 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2016.csv")
census_2017 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2017.csv")
census_2018 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2018.csv")
census_2019 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2019.csv")
census_2020 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2020.csv")
census_2021 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2021.csv")

## arrange rows 
View(census_2014)
census_2014 <- census_2014[-c(1,2,3), ] 
census_2015 <- census_2015[-c(1,2,3), ] 
census_2016 <- census_2016[-c(1,2,3), ] 
census_2017 <- census_2017[-c(1,2,3), ] 
census_2018 <- census_2018[-c(1,2,3), ] 
census_2019 <- census_2019[-c(1,2,3), ] 
census_2020 <- census_2020[-c(1,2,3), ] 
census_2021 <- census_2021[-c(1,2,3), ] 

## rename columns
census_2014 = rename(census_2014,
                     'Demographics & Educational Attainment' = Source..CPS.Annual.Social.and.Economic..March..Supplement.201403,
                     'Total' = X,
                     'Alabama' = X.1,
                     ) 

## creating pivot table - census 2014
census_2014_subset <- census_2014 %>% select()
census_2014_pivot <- census_2014 %>% 
  pivot_longer(cols = starts_with(""),
               names_to = "",
               names_prefix = "",
               values_to = "",
)
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
