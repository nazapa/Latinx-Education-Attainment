## Latinx educational attainment levels

## libraries
library(tidyverse)
library(tidycensus)

########################################################################################################################
# Resources
# https://anyone-can-cook.github.io/rclass2/lectures/apis_and_json/apis_and_json.html#5_Using_APIs_to_request_data
# https://walker-data.com/tidycensus/articles/basic-usage.html

# Available datasets: https://www.census.gov/data/developers/data-sets.html
# ACS 1-year data: https://www.census.gov/data/developers/data-sets/acs-1year.html
# ACS 1-year supplemental data: https://www.census.gov/data/developers/data-sets/ACS-supplemental-data.html
# ACS 5-year data: https://www.census.gov/data/developers/data-sets/acs-5year.html

# Variable list for each dataset can be found either in above Census link or SocialExplorer (ACS 5-year below)
# https://api.census.gov/data/2020/acs/acs5/variables.html
# https://www.socialexplorer.com/data/ACS2020_5yr/metadata/?ds=ACS20_5yr

# Supported geography levels
# https://api.census.gov/data/2020/acs/acs5/geography.html
# https://api.census.gov/data/2020/acs/acs5/examples.html

# API key
# key activation -> sign up here https://api.census.gov/data/key_signup.html

########################################################################################################################

## API key: 7510042ab364969916f8fcb254e7f2e65e2a134f
## census key
census_api_key('7510042ab364969916f8fcb254e7f2e65e2a134f', install = TRUE)


## ?load_variables
acs5_vars <- load_variables('2020', 'acs5')

## filter for attainment and enrollment
acs5_vars %>% filter(str_detect(concept, 'ATTAINMENT')) %>% View()
acs5_vars %>% filter(str_detect(concept, 'ENROLLMENT')) %>% View()

# C15002I. Sex By Educational Attainment For The Population 25 Years And Over (Hispanic or Latino)
# https://api.census.gov/data/2020/acs/acs5/groups/C15002I.html
# https://www.socialexplorer.com/data/ACS2020_5yr/metadata/?ds=ACS20_5yr&table=C15002I
acs5_vars %>% filter(str_detect(name, 'C15002I')) %>% View()

# B14007I. School Enrollment By Detailed Level Of School For The Population 3 Years And Over (Hispanic or Latino)
# https://api.census.gov/data/2020/acs/acs5/groups/B14007I.html
# https://www.socialexplorer.com/data/ACS2020_5yr/metadata/?ds=ACS20_5yr&table=B14007I
acs5_vars %>% filter(str_detect(name, 'B14007I')) %>% View()

# ?get_acs 
## create varible list
var_list <- str_subset(acs5_vars$name, 'C15002I|B14007I')
var_list

## Request variables for 2020 based on zip code level
acs5_data <- get_acs(
  geography = 'zip code tabulation area',  ##taken from this website: https://api.census.gov/data/2020/acs/acs5/examples.html
  variables = var_list,
  year = 2020,
  output = 'wide'
)
acs5_data$YEAR <- 2020

## Append data from previous years based on zip code level
? get_acs
for (year in 2016:2019) {
  temp_df <- get_acs(
    geography = 'zip code tabulation area',
    variables = var_list,
    year = year,
    output = 'wide'
  )
  temp_df$YEAR <- year
  
  acs5_data <- dplyr::bind_rows(acs5_data, temp_df)
}


## create data frame for zip code data
? str_pad
df <- read_csv('zip_code_states.csv', col_types = c('zip_code' ='c')) %>%
  mutate(zip_code = str_pad(zip_code, 5, "left", "0"))

## extracting and replacing sub strings for acs 5 year data set
?str_sub
acs5_data <- acs5_data %>% mutate(NAME = str_sub(NAME, start = 7L, end = -1L))

## left join 
?left_join
acs5_data <- left_join(x= acs5_data, y=df, by = c("NAME" = "zip_code"))




## Meeting w/ Crystal on 04/18/2022
## GEOID is the way that R organizes the acs data set. GEOID, which is an identifier 
## for the geographical unit associated with the row. See: https://walker-data.com/tidycensus/articles/basic-usage.html
## each row is for each zip code area only, not people/individuals.

## To find data at the county and metropolitan area use the following 
## link: https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus 
## and specify the request to county level data. See the following example:

acs5_data <- get_acs(
  geography = 'county',  ##taken from this website: https://api.census.gov/data/2020/acs/acs5/examples.html
  variables = var_list,  ##take from the census website: https://api.census.gov/data/2020/acs/acs5/variables.html
  year = 2020,
  output = 'wide'
)

## In GEOID (in the county data set) the first 2 digits are the numeric code for the state. 
## See this link for the other state codes: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code

acs5_ca_data <- get_acs(
  geography = 'county',  ##taken from this website: https://api.census.gov/data/2020/acs/acs5/examples.html
  variables = var_list,  ##take from the census website: https://api.census.gov/data/2020/acs/acs5/variables.html
  year = 2020,
  state = "CA",
  output = 'wide'
)

## change the variable names. First remove the "M", margin of error, not needed. 
## we can keep "E" for estimate.
acs5_ca_data <- acs5_ca_data %>% select(-ends_with("M")) 

## rename variables using {}
?rename
?str_sub
var_labels_ca <- Vectorize(function(var_name) {
  print(var_name)
  if (var_name == "GEOID" || var_name == "NAME"){
    return(var_name)
  }
  print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
  return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
}) 

acs5_ca_data_renamed_labels <- acs5_ca_data %>% rename_with(var_labels_ca)


## in the metropolitan dataset, there is no need to put state code
acs5_ca_metro_data <- get_acs(
  geography = 'metropolitan statistical area/micropolitan statistical area',  ##taken from this website: https://api.census.gov/data/2020/acs/acs5/examples.html
  variables = var_list,  ##take from the census website: https://api.census.gov/data/2020/acs/acs5/variables.html
  year = 2020,
  output = 'wide'
)

## you can rename variables using this example from this 
## resource: https://walker-data.com/tidycensus/articles/basic-usage.html#working-with-acs-data
vt <- get_acs(geography = "county", 
              variables = c(medincome = "B19013_001"), 
              state = "VT", 
              year = 2018)






############ ACS County Level Data 2020 ############ 
## census key
census_api_key('7510042ab364969916f8fcb254e7f2e65e2a134f', install = TRUE)

## ?load_variables
acs5_vars <- load_variables('2020', 'acs5')

## filter for attainment and enrollment
acs5_vars %>% filter(str_detect(concept, 'ATTAINMENT')) %>% View()
acs5_vars %>% filter(str_detect(concept, 'ENROLLMENT')) %>% View()

# C15002I. Sex By Educational Attainment For The Population 25 Years And Over (Hispanic or Latino)
acs5_vars %>% filter(str_detect(name, 'C15002I')) %>% View()

# B14007I. School Enrollment By Detailed Level Of School For The Population 3 Years And Over (Hispanic or Latino)
acs5_vars %>% filter(str_detect(name, 'B14007I')) %>% View()

## create variable list
var_list <- str_subset(acs5_vars$name, 'C15002I|B14007I')
var_list

## county level 2020 data
acs5_data <- get_acs(
  geography = 'county',  
  variables = var_list, 
  year = 2020,
  output = 'wide'
)

## rename column names
var_labels_county <- Vectorize(function(var_name) {
  print(var_name)
  if (var_name == "GEOID" || var_name == "NAME"){
    return(var_name)
  }
  print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
  return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
}) 

acs5_data <- acs5_data %>% rename_with(var_labels_county)

## CA 2020 DATA
acs5_ca_data <- get_acs(
  geography = 'county',  
  variables = var_list, 
  year = 2020,
  state = "CA", 
  output = 'wide'
)

## rename column names
var_labels_ca <- Vectorize(function(var_name) {
  print(var_name)
  if (var_name == "GEOID" || var_name == "NAME"){
    return(var_name)
  }
  print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
  return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
}) 

acs5_ca_data <- acs5_ca_data %>% rename_with(var_labels_ca)

## AL 2020 DATA
acs5_al_data <- get_acs(
  geography = 'county',  
  variables = var_list, 
  year = 2020,
  state = "AL", 
  output = 'wide'
)

## rename column names
var_labels_al <- Vectorize(function(var_name) {
  print(var_name)
  if (var_name == "GEOID" || var_name == "NAME"){
    return(var_name)
  }
  print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
  return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
}) 

acs5_al_data <- acs5_al_data %>% rename_with(var_labels_al)

## AK 2020 DATA

## AS 2020 DATA

## AZ 2020 DATA 

## AR 2020 DATA 

## CO 2020 DATA

## CT 2020 DATA

## DE 2020 DATA

## DC 2020 DATA

## FL 2020 DATA 

## GA 2020 DATA

## HI 2020 DATA

## ID 2020 DATA

## IL 2020 DATA

## IN 2020 DATA

## IA 2020 DATA 

## KS 2020 DATA

## KY 2020 DATA 

## LA 2020 DATA

## ME 2020 DATA

## MD 2020 DATA

## MA 2020 DATA

## MI 2020 DATA

## MN 2020 DATA

## MS 2020 DATA

## MO 2020 DATA

## MT 2020 DATA

## NI 2020 DATA

## NE 2020 DATA

## NV 2020 DATA

## NH 2020 DATA

## NJ 2020 DATA

## NM 2020 DATA

## NY 2020 DATA

## NC 2020 DATA

## ND 2020 DATA

## OH 2020 DATA

## OK 2020 DATA

## OR 2020 DATA

## PA 2020 DATA

## PR 2020 DATA

## RI 2020 DATA

## SC 2020 DATA

## SD 2020 DATA

## TN 2020 DATA

## TX 2020 DATA

## UT 2020 DATA

## VT 2020 DATA

## VA 2020 DATA

## WA 2020 DATA

## WV 2020 DATA

## WI 2020 DATA

## WY 2020 DATA


############ Metropolitan Data ############

## Metropolitan Data 2020
acs5_metro_2020 <- get_acs(
  geography = 'metropolitan statistical area/micropolitan statistical area',  ## taken from this website: https://api.census.gov/data/2020/acs/acs5/examples.html
  variables = var_list,  ## taken from the census website: https://api.census.gov/data/2020/acs/acs5/variables.html
  year = 2020,
  output = 'wide'
)

## rename column names
var_labels_metro_2020 <- Vectorize(function(var_name) {
  print(var_name)
  if (var_name == "GEOID" || var_name == "NAME"){
    return(var_name)
  }
  print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
  return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
}) 

acs5_metro_2020 <- acs5_metro_2020 %>% rename_with(var_labels_metro_2020)

## Metropolitan Data 2019
acs5_metro_2019 <- get_acs(
  geography = 'metropolitan statistical area/micropolitan statistical area',  ## taken from this website: https://api.census.gov/data/2020/acs/acs5/examples.html
  variables = var_list,  ## taken from the census website: https://api.census.gov/data/2020/acs/acs5/variables.html
  year = 2019,
  output = 'wide'
)
## rename column names
var_labels_metro_2019 <- Vectorize(function(var_name) {
  print(var_name)
  if (var_name == "GEOID" || var_name == "NAME"){
    return(var_name)
  }
  print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
  return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
}) 

acs5_metro_2019 <- acs5_metro_2019 %>% rename_with(var_labels_metro_2019)

## Metropolitan Data 2018
acs5_metro_2018 <- get_acs(
  geography = 'metropolitan statistical area/micropolitan statistical area',  
  variables = var_list, 
  year = 2018,
  output = 'wide'
)
## rename column names
var_labels_metro_2018 <- Vectorize(function(var_name) {
  print(var_name)
  if (var_name == "GEOID" || var_name == "NAME"){
    return(var_name)
  }
  print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
  return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
}) 

acs5_metro_2018 <- acs5_metro_2018 %>% rename_with(var_labels_metro_2018)

## Metropolitan Data 2017
acs5_metro_2017 <- get_acs(
  geography = 'metropolitan statistical area/micropolitan statistical area',  
  variables = var_list, 
  year = 2017,
  output = 'wide'
)

## rename column names
var_labels_metro_2017 <- Vectorize(function(var_name) {
  print(var_name)
  if (var_name == "GEOID" || var_name == "NAME"){
    return(var_name)
  }
  print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
  return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
}) 

acs5_metro_2017 <- acs5_metro_2017 %>% rename_with(var_labels_metro_2017)

## Metropolitan Data 2016
acs5_metro_2016 <- get_acs(
  geography = 'metropolitan statistical area/micropolitan statistical area',  
  variables = var_list, 
  year = 2016,
  output = 'wide'
)

## rename column names
var_labels_metro_2016 <- Vectorize(function(var_name) {
  print(var_name)
  if (var_name == "GEOID" || var_name == "NAME"){
    return(var_name)
  }
  print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
  return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
}) 

acs5_metro_2016 <- acs5_metro_2016 %>% rename_with(var_labels_metro_2016)

## Metropolitan Data 2015
acs5_metro_2015 <- get_acs(
  geography = 'metropolitan statistical area/micropolitan statistical area',  
  variables = var_list, 
  year = 2015,
  output = 'wide'
)

## rename column names
var_labels_metro_2015 <- Vectorize(function(var_name) {
  print(var_name)
  if (var_name == "GEOID" || var_name == "NAME"){
    return(var_name)
  }
  print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
  return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
}) 

acs5_metro_2015 <- acs5_metro_2015 %>% rename_with(var_labels_metro_2015)


## Fix variables
## you can rename variables using this example from this 
## resource: https://walker-data.com/tidycensus/articles/basic-usage.html#working-with-acs-data
acs5_metro_2015 <- get_acs(geography = 'metropolitan statistical area/micropolitan statistical area',  
                           variables = c(Total_Enrollment = 'B14007I_001E', 
                                         Total_Enrolled_In_School = 'B14007I_002E'), 
                           year = 2015,
                           output = 'wide')


############################## Creating Graphs ##############################
library(ggplot2) 
library(scales)
library(haven)
library(labelled)

acs5_metro_2015 %>% 
  ggplot() + geom_bar(aes(y=))
