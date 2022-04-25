################################################################################
##
## [ PROJ ] < Latinx Education Attainment >
## [ FILE ] < R Script >
## [ AUTH ] < Paula Nazario / @nazapa >
## [ INIT ] < April 4, 2022 >
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
## load data sets & organizing
census_2014 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2014.csv", skip =4)
census_2014 <- rename(census_2014, c(Demographics = Demographics..Educational.attainment..A_HGA., 
                                     D.C. = District.of.Columbia,
                                     New_Hampshire = New.Hampshire,
                                     New_Jersey = New.Jersey,
                                     New_Mexico = New.Mexico,
                                     New_York = New.York,
                                     North_Carolina = North.Carolina,
                                     North_Dakota = North.Dakota,
                                     Rhode_Island = Rhode.Island,
                                     South_Carolina = South.Carolina,
                                     South_Dakota = South.Dakota))
census_2015 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2015.csv", skip =4)
census_2015 <- rename(census_2015, c(Demographics = Demographics..Educational.attainment..A_HGA., 
                                     D.C. = District.of.Columbia,
                                     New_Hampshire = New.Hampshire,
                                     New_Jersey = New.Jersey,
                                     New_Mexico = New.Mexico,
                                     New_York = New.York,
                                     North_Carolina = North.Carolina,
                                     North_Dakota = North.Dakota,
                                     Rhode_Island = Rhode.Island,
                                     South_Carolina = South.Carolina,
                                     South_Dakota = South.Dakota))
census_2016 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2016.csv", skip =4)
census_2016 <- rename(census_2016, c(Demographics = Demographics..Educational.attainment..A_HGA., 
                                     D.C. = District.of.Columbia,
                                     New_Hampshire = New.Hampshire,
                                     New_Jersey = New.Jersey,
                                     New_Mexico = New.Mexico,   
                                     New_York = New.York,
                                     North_Carolina = North.Carolina,
                                     North_Dakota = North.Dakota,
                                     Rhode_Island = Rhode.Island,
                                     South_Carolina = South.Carolina,
                                     South_Dakota = South.Dakota))
census_2017 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2017.csv", skip =4)
census_2017 <- rename(census_2017, c(Demographics = Demographics..Educational.attainment..A_HGA., 
                                     D.C. = District.of.Columbia,
                                     New_Hampshire = New.Hampshire,
                                     New_Jersey = New.Jersey,
                                     New_Mexico = New.Mexico,
                                     New_York = New.York,
                                     North_Carolina = North.Carolina,
                                     North_Dakota = North.Dakota,
                                     Rhode_Island = Rhode.Island,
                                     South_Carolina = South.Carolina,
                                     South_Dakota = South.Dakota))
census_2018 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2018.csv", skip =4)
census_2018 <- rename(census_2018, c(Demographics = Demographics..Educational.attainment..A_HGA., 
                                     D.C. = District.of.Columbia,
                                     New_Hampshire = New.Hampshire,
                                     New_Jersey = New.Jersey,
                                     New_Mexico = New.Mexico,
                                     New_York = New.York,
                                     North_Carolina = North.Carolina,
                                     North_Dakota = North.Dakota,
                                     Rhode_Island = Rhode.Island,
                                     South_Carolina = South.Carolina,
                                     South_Dakota = South.Dakota))
census_2019 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2019.csv", skip =4)
census_2019 <- rename(census_2019, c(Demographics = Demographics..Educational.attainment..A_HGA., 
                                     D.C. = District.of.Columbia,
                                     New_Hampshire = New.Hampshire,
                                     New_Jersey = New.Jersey,
                                     New_Mexico = New.Mexico,
                                     New_York = New.York,
                                     North_Carolina = North.Carolina,
                                     North_Dakota = North.Dakota,
                                     Rhode_Island = Rhode.Island,
                                     South_Carolina = South.Carolina,
                                     South_Dakota = South.Dakota))
census_2020 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2020.csv", skip =4)
census_2020 <- rename(census_2020, c(Demographics = Demographics..Educational.attainment..A_HGA., 
                                     D.C. = District.of.Columbia,
                                     New_Hampshire = New.Hampshire,
                                     New_Jersey = New.Jersey,
                                     New_Mexico = New.Mexico,
                                     New_York = New.York,
                                     North_Carolina = North.Carolina,
                                     North_Dakota = North.Dakota,
                                     Rhode_Island = Rhode.Island,
                                     South_Carolina = South.Carolina,
                                     South_Dakota = South.Dakota))
census_2021 <- read.csv("2014-21 CPS Annual Social and Economic/Census-2021.csv", skip =4)
census_2021 <- rename(census_2021, c(Demographics = Demographics..Educational.attainment..A_HGA., 
                                     D.C. = District.of.Columbia,
                                     New_Hampshire = New.Hampshire,
                                     New_Jersey = New.Jersey,
                                     New_Mexico = New.Mexico,
                                     New_York = New.York,
                                     North_Carolina = North.Carolina,
                                     North_Dakota = North.Dakota,
                                     Rhode_Island = Rhode.Island,
                                     South_Carolina = South.Carolina,
                                     South_Dakota = South.Dakota))
## see lecture: 
## https://anyone-can-cook.github.io/rclass2/lectures/organizing_and_io/organizing_and_io.html#5_readr_package
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Census 2014 data
## Variables:
## less than a high school diploma, associate's degree (occupation/vocation/academic program), 
## some college but didn't complete, Bachelor's degree, Master's degree, Professional degree, 
## Doctorate degree (PhD, EDD), Total, Sum of education degrees 

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Census 2015 data
## Variables:
## less than a high school diploma, associate's degree (occupation/vocation/academic program), 
## some college but didn't complete, Bachelor's degree, Master's degree, Professional degree, 
## Doctorate degree (PhD, EDD), Total, Sum of education degrees 
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Census 2016 data
## Variables:
## less than a high school diploma, associate's degree (occupation/vocation/academic program), 
## some college but didn't complete, Bachelor's degree, Master's degree, Professional degree, 
## Doctorate degree (PhD, EDD), Total, Sum of education degrees 
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Census 2017 data
## Variables:
## less than a high school diploma, associate's degree (occupation/vocation/academic program), 
## some college but didn't complete, Bachelor's degree, Master's degree, Professional degree, 
## Doctorate degree (PhD, EDD), Total, Sum of education degrees 
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Census 2018 data
## Variables:
## less than a high school diploma, associate's degree (occupation/vocation/academic program), 
## some college but didn't complete, Bachelor's degree, Master's degree, Professional degree, 
## Doctorate degree (PhD, EDD), Total, Sum of education degrees 
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Census 2019 data
## Variables:
## less than a high school diploma, associate's degree (occupation/vocation/academic program), 
## some college but didn't complete, Bachelor's degree, Master's degree, Professional degree, 
## Doctorate degree (PhD, EDD), Total, Sum of education degrees 
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Census 2020 data
## Variables:
## less than a high school diploma, associate's degree (occupation/vocation/academic program), 
## some college but didn't complete, Bachelor's degree, Master's degree, Professional degree, 
## Doctorate degree (PhD, EDD), Total, Sum of education degrees 

## Variables within the demographics column:
## latino males, total(minus children), less than a high school diploma, high school graduate-high school diploma,
## associates degree, bachelor's degree, master's degree, professional degree, doctorate degree, 


## need help here; how do i organize these dataframes with the info i want?
latino_males_2020 <- census_2020 %>% 
  select(Alabama, Alaska, Arizona, Arkansas, California, Colorado, 
         Delaware, D.C., Florida, Georgia, Hawaii,
         Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana,
         Missouri, Montana, Nebraska, Nevada, New_Hampshire, New_Jersey, 
         New_Mexico, New_York, North_Carolina, North_Dakota, Ohio,
         Oklahoma, Oregon, Pennsylvania, Rhode_Island, South_Carolina,
         South_Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington) 
white_males_2020 <- census_2020 %>% 
  select(Alabama, Alaska, Arizona, Arkansas, California, Colorado, 
         Delaware, D.C., Florida, Georgia, Hawaii,
         Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana,
         Missouri, Montana, Nebraska, Nevada, New_Hampshire, New_Jersey, 
         New_Mexico, New_York, North_Carolina, North_Dakota, Ohio,
         Oklahoma, Oregon, Pennsylvania, Rhode_Island, South_Carolina,
         South_Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington) 
black_males_2020 <- census_2020 %>% 
  select(Alabama, Alaska, Arizona, Arkansas, California, Colorado, 
         Delaware, D.C., Florida, Georgia, Hawaii,
         Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana,
         Missouri, Montana, Nebraska, Nevada, New_Hampshire, New_Jersey, 
         New_Mexico, New_York, North_Carolina, North_Dakota, Ohio,
         Oklahoma, Oregon, Pennsylvania, Rhode_Island, South_Carolina,
         South_Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington) 
asian_males_2020 <- census_2020 %>% 
  select(Alabama, Alaska, Arizona, Arkansas, California, Colorado, 
         Delaware, D.C., Florida, Georgia, Hawaii,
         Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana,
         Missouri, Montana, Nebraska, Nevada, New_Hampshire, New_Jersey, 
         New_Mexico, New_York, North_Carolina, North_Dakota, Ohio,
         Oklahoma, Oregon, Pennsylvania, Rhode_Island, South_Carolina,
         South_Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington) 
latina_females_2020 <- census_2020 %>% 
  select(Alabama, Alaska, Arizona, Arkansas, California, Colorado, 
         Delaware, D.C., Florida, Georgia, Hawaii,
         Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana,
         Missouri, Montana, Nebraska, Nevada, New_Hampshire, New_Jersey, 
         New_Mexico, New_York, North_Carolina, North_Dakota, Ohio,
         Oklahoma, Oregon, Pennsylvania, Rhode_Island, South_Carolina,
         South_Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington) 
white_females_2020 <- census_2020 %>% 
  select(Alabama, Alaska, Arizona, Arkansas, California, Colorado, 
         Delaware, D.C., Florida, Georgia, Hawaii,
         Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana,
         Missouri, Montana, Nebraska, Nevada, New_Hampshire, New_Jersey, 
         New_Mexico, New_York, North_Carolina, North_Dakota, Ohio,
         Oklahoma, Oregon, Pennsylvania, Rhode_Island, South_Carolina,
         South_Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington) 
black_females_2020 <- census_2020 %>% 
  select(Alabama, Alaska, Arizona, Arkansas, California, Colorado, 
         Delaware, D.C., Florida, Georgia, Hawaii,
         Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana,
         Missouri, Montana, Nebraska, Nevada, New_Hampshire, New_Jersey, 
         New_Mexico, New_York, North_Carolina, North_Dakota, Ohio,
         Oklahoma, Oregon, Pennsylvania, Rhode_Island, South_Carolina,
         South_Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington)
asian_females_2020 <- census_2020 %>% 
  select(Alabama, Alaska, Arizona, Arkansas, California, Colorado, 
         Delaware, D.C., Florida, Georgia, Hawaii,
         Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana,
         Missouri, Montana, Nebraska, Nevada, New_Hampshire, New_Jersey, 
         New_Mexico, New_York, North_Carolina, North_Dakota, Ohio,
         Oklahoma, Oregon, Pennsylvania, Rhode_Island, South_Carolina,
         South_Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington)
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Census 2021 data
## Variables:
## less than a high school diploma, associate's degree (occupation/vocation/academic program), 
## some college but didn't complete, Bachelor's degree, Master's degree, Professional degree, 
## Doctorate degree (PhD, EDD), Total, Sum of education degrees 
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
