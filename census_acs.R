## Latinx educational attainment levels

## libraries
library(tidyverse)
library(tidycensus)
library(labelled)

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
## create variable list
var_list <- str_subset(acs5_vars$name, 'C15002|B14007')
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






############ ACS 5-year County Level Data 2012-2020 ############ 
## census key
census_api_key('7510042ab364969916f8fcb254e7f2e65e2a134f', install = TRUE)

## ?load_variables
acs5_vars <- load_variables('2020', 'acs5')

## filter for attainment and enrollment
## acs5_vars %>% filter(str_detect(concept, 'ATTAINMENT')) %>% View()
## acs5_vars %>% filter(str_detect(concept, 'ENROLLMENT')) %>% View()

# C15002I. Sex By Educational Attainment For The Population 25 Years And Over (Hispanic or Latino)
acs5_vars %>% filter(str_detect(name, 'C15002I')) %>% View()

# B14007I. School Enrollment By Detailed Level Of School For The Population 3 Years And Over (Hispanic or Latino)
acs5_vars %>% filter(str_detect(name, 'B14007I')) %>% View()

## create variable list
var_list <- str_subset(acs5_vars$name, 'C15002|B14007')
var_list

# ## County level 2020 data (keep margin of error and estimates)
# acs5_county_data_2020 <- get_acs(
#   geography = 'county',  
#   variables = var_list, 
#   year = 2020,
#   output = 'wide'
# )
# acs5_county_data_2020
# 
# ## County level 2019 data (keep margin of error and estimates)
# acs5_county_data_2019 <- get_acs(
#   geography = 'county',  
#   variables = var_list, 
#   year = 2019,
#   output = 'wide'
# )
# acs5_county_data_2019
# 
# ## County level 2018 data (keep margin of error and estimates)
# acs5_county_data_2018 <- get_acs(
#   geography = 'county',  
#   variables = var_list, 
#   year = 2018,
#   output = 'wide'
# )
# acs5_county_data_2018
# 
# ## County level 2017 data (keep margin of error and estimates)
# acs5_county_data_2017 <- get_acs(
#   geography = 'county',  
#   variables = var_list, 
#   year = 2017,
#   output = 'wide'
# )
# acs5_county_data_2017
# 
# ## County level 2016 data (keep margin of error and estimates)
# acs5_county_data_2016 <- get_acs(
#   geography = 'county',  
#   variables = var_list, 
#   year = 2016,
#   output = 'wide'
# )
# acs5_county_data_2016
# 
# ## County level 2015 data (keep margin of error and estimates)
# acs5_county_data_2015 <- get_acs(
#   geography = 'county',  
#   variables = var_list, 
#   year = 2015,
#   output = 'wide'
# )
# acs5_county_data_2015
# 
# ## County level 2014 data (keep margin of error and estimates)
# acs5_county_data_2014 <- get_acs(
#   geography = 'county',  
#   variables = var_list, 
#   year = 2014,
#   output = 'wide'
# )
# acs5_county_data_2014
# 
# ## County level 2013 data (keep margin of error and estimates)
# acs5_county_data_2013 <- get_acs(
#   geography = 'county',  
#   variables = var_list, 
#   year = 2013,
#   output = 'wide'
# )
# acs5_county_data_2013
# 
# ## County level 2012 data (keep margin of error and estimates)
# acs5_county_data_2012 <- get_acs(
#   geography = 'county',  
#   variables = var_list, 
#   year = 2012,
#   output = 'wide'
# )
# acs5_county_data_2012

############ ACS 5-year Metropolitan Data 2012-2020 ############

# ## Metropolitan Data 2020
# acs5_metro_2020 <- get_acs(
#   geography = 'metropolitan statistical area/micropolitan statistical area',  ## taken from this website: https://api.census.gov/data/2020/acs/acs5/examples.html
#   variables = var_list,  ## taken from the census website: https://api.census.gov/data/2020/acs/acs5/variables.html
#   year = 2020,
#   output = 'wide'
# )
# ## rename column names
# var_labels_metro_2020 <- Vectorize(function(var_name) {
#   print(var_name)
#   if (var_name == "GEOID" || var_name == "NAME"){
#     return(var_name)
#   }
#   print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
#   return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
# }) 
# 
# acs5_metro_2020 <- acs5_metro_2020 %>% rename_with(var_labels_metro_2020)
# 
# ## Metropolitan Data 2019
# acs5_metro_2019 <- get_acs(
#   geography = 'metropolitan statistical area/micropolitan statistical area',  ## taken from this website: https://api.census.gov/data/2020/acs/acs5/examples.html
#   variables = var_list,  ## taken from the census website: https://api.census.gov/data/2020/acs/acs5/variables.html
#   year = 2019,
#   output = 'wide'
# )
# ## rename column names
# var_labels_metro_2019 <- Vectorize(function(var_name) {
#   print(var_name)
#   if (var_name == "GEOID" || var_name == "NAME"){
#     return(var_name)
#   }
#   print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
#   return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
# }) 
# 
# acs5_metro_2019 <- acs5_metro_2019 %>% rename_with(var_labels_metro_2019)
# 
# ## Metropolitan Data 2018
# acs5_metro_2018 <- get_acs(
#   geography = 'metropolitan statistical area/micropolitan statistical area',  
#   variables = var_list, 
#   year = 2018,
#   output = 'wide'
# )
# ## rename column names
# var_labels_metro_2018 <- Vectorize(function(var_name) {
#   print(var_name)
#   if (var_name == "GEOID" || var_name == "NAME"){
#     return(var_name)
#   }
#   print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
#   return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
# }) 
# 
# acs5_metro_2018 <- acs5_metro_2018 %>% rename_with(var_labels_metro_2018)
# 
# ## Metropolitan Data 2017
# acs5_metro_2017 <- get_acs(
#   geography = 'metropolitan statistical area/micropolitan statistical area',  
#   variables = var_list, 
#   year = 2017,
#   output = 'wide'
# )
# ## rename column names
# var_labels_metro_2017 <- Vectorize(function(var_name) {
#   print(var_name)
#   if (var_name == "GEOID" || var_name == "NAME"){
#     return(var_name)
#   }
#   print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
#   return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
# }) 
# 
# acs5_metro_2017 <- acs5_metro_2017 %>% rename_with(var_labels_metro_2017)
# 
# ## Metropolitan Data 2016
# acs5_metro_2016 <- get_acs(
#   geography = 'metropolitan statistical area/micropolitan statistical area',  
#   variables = var_list, 
#   year = 2016,
#   output = 'wide'
# )
# ## rename column names
# var_labels_metro_2016 <- Vectorize(function(var_name) {
#   print(var_name)
#   if (var_name == "GEOID" || var_name == "NAME"){
#     return(var_name)
#   }
#   print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
#   return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
# }) 
# 
# acs5_metro_2016 <- acs5_metro_2016 %>% rename_with(var_labels_metro_2016)
# 
# ## Metropolitan Data 2015
# acs5_metro_2015 <- get_acs(
#   geography = 'metropolitan statistical area/micropolitan statistical area',  
#   variables = var_list, 
#   year = 2015,
#   output = 'wide'
# )
# ## rename column names
# var_labels_metro_2015 <- Vectorize(function(var_name) {
#   print(var_name)
#   if (var_name == "GEOID" || var_name == "NAME"){
#     return(var_name)
#   }
#   print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
#   return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
# }) 
# 
# acs5_metro_2015 <- acs5_metro_2015 %>% rename_with(var_labels_metro_2015)
# 
# ## Metropolitan Data 2014
# acs5_metro_2014 <- get_acs(
#   geography = 'metropolitan statistical area/micropolitan statistical area',  
#   variables = var_list, 
#   year = 2014,
#   output = 'wide'
# )
# ## rename column names
# var_labels_metro_2014 <- Vectorize(function(var_name) {
#   print(var_name)
#   if (var_name == "GEOID" || var_name == "NAME"){
#     return(var_name)
#   }
#   print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
#   return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
# }) 
# 
# acs5_metro_2014 <- acs5_metro_2014 %>% rename_with(var_labels_metro_2014)
# 
# ## Metropolitan Data 2013
# acs5_metro_2013 <- get_acs(
#   geography = 'metropolitan statistical area/micropolitan statistical area',  
#   variables = var_list, 
#   year = 2013,
#   output = 'wide'
# )
# ## rename column names
# var_labels_metro_2013 <- Vectorize(function(var_name) {
#   print(var_name)
#   if (var_name == "GEOID" || var_name == "NAME"){
#     return(var_name)
#   }
#   print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
#   return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
# }) 
# 
# acs5_metro_2013 <- acs5_metro_2013 %>% rename_with(var_labels_metro_2013)
# 
# ## Metropolitan Data 2012
# acs5_metro_2012 <- get_acs(
#   geography = 'metropolitan statistical area/micropolitan statistical area',  
#   variables = var_list, 
#   year = 2012,
#   output = 'wide'
# )
# ## rename column names
# var_labels_metro_2012 <- Vectorize(function(var_name) {
#   print(var_name)
#   if (var_name == "GEOID" || var_name == "NAME"){
#     return(var_name)
#   }
#   print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
#   return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
# }) 
# 
# acs5_metro_2012 <- acs5_metro_2012 %>% rename_with(var_labels_metro_2012)
# 
# ############ ACS 5-year State Level Data 2012-2020 ############
# 
# ## State Data 2020
# acs5_state_data_2020 <- get_acs(
#   geography = 'state',  
#   variables = var_list, 
#   year = 2020,
#   output = 'wide'
# )
# acs5_state_data_2020
# 
# ## State Data 2019
# acs5_state_data_2019 <- get_acs(
#   geography = 'state',  
#   variables = var_list, 
#   year = 2019,
#   output = 'wide'
# )
# acs5_state_data_2019
# 
# ## State Data 2018
# acs5_state_data_2018 <- get_acs(
#   geography = 'state',  
#   variables = var_list, 
#   year = 2018,
#   output = 'wide'
# )
# acs5_state_data_2018
# 
# ## State Data 2017
# acs5_state_data_2017 <- get_acs(
#   geography = 'state',  
#   variables = var_list, 
#   year = 2017,
#   output = 'wide'
# )
# acs5_state_data_2017
# 
# ## State Data 2016
# acs5_state_data_2016 <- get_acs(
#   geography = 'state',  
#   variables = var_list, 
#   year = 2016,
#   output = 'wide'
# )
# acs5_state_data_2016
# 
# ## State Data 2015
# acs5_state_data_2015 <- get_acs(
#   geography = 'state',  
#   variables = var_list, 
#   year = 2015,
#   output = 'wide'
# )
# acs5_state_data_2015
# 
# ## State Data 2014
# acs5_state_data_2014 <- get_acs(
#   geography = 'state',  
#   variables = var_list, 
#   year = 2014,
#   output = 'wide'
# )
# acs5_state_data_2014
# 
# ## State Data 2013
# acs5_state_data_2013 <- get_acs(
#   geography = 'state',  
#   variables = var_list, 
#   year = 2013,
#   output = 'wide'
# )
# acs5_state_data_2013
# 
# ## State Data 2012
# acs5_state_data_2012 <- get_acs(
#   geography = 'state',  
#   variables = var_list, 
#   year = 2012,
#   output = 'wide'
# )
# acs5_state_data_2012










############## Loops for ACS 5-year County, Metropolitan, and State Data 2012-2020 ######################
## ACS 5-year County
acs5_county_data <- get_acs(
  geography = 'county',  
  variables = var_list, 
  year = 2020,
  output = 'wide'
)
acs5_county_data$YEAR <- 2020

for(year in 2012:2019) {
  temp_acs5_county_data <- get_acs(
    geography = 'county',  
    variables = var_list, 
    year = year,
    output = 'wide'
  )
  temp_acs5_county_data$YEAR <- year
  
  acs5_county_data <- dplyr::bind_rows(acs5_county_data, temp_acs5_county_data)
} 
## renaming labels
for (col_name in names(acs5_county_data)) {
  if (col_name == "GEOID" || col_name == "NAME"){
    next
  }
  var_label(acs5_county_data[[col_name]] ) <- (filter(acs5_vars, name == str_sub(col_name, start = 1L, end = -2)))$label
}


## ACS 5-year Metro
metro_data <- function(year) {
  temp_acs5_metro_data <- get_acs(
    geography = 'metropolitan statistical area/micropolitan statistical area',  
    variables = var_list, 
    year = year,
    output = 'wide'
  )
  temp_acs5_metro_data$YEAR <- year
  return(temp_acs5_metro_data)
}
  
acs5_metro_data <- lapply(2012:2020, metro_data) %>% bind_rows()
## renaming labels
for (col_name in names(acs5_metro_data)) {
  if (col_name == "GEOID" || col_name == "NAME"){
    next
  }
  var_label(acs5_metro_data[[col_name]] ) <- (filter(acs5_vars, name == str_sub(col_name, start = 1L, end = -2)))$label
}


## ACS 5-year State
state_data <- function(year) {
  temp_acs5_state_data <- get_acs(
    geography = 'state',  
    variables = var_list, 
    year = year,
    output = 'wide'
  )
  temp_acs5_state_data$YEAR <- year
  return(temp_acs5_state_data)
}

acs5_state_data <- lapply(2012:2020, state_data) %>% bind_rows()
## renaming labels
for (col_name in names(acs5_state_data)) {
  if (col_name == "GEOID" || col_name == "NAME"){
    next
  }
  var_label(acs5_state_data[[col_name]] ) <- (filter(acs5_vars, name == str_sub(col_name, start = 1L, end = -2)))$label
}


############### Loops for ACS 1-year County, Metropolitan, and State Data 2012-2019 ######################
?get_acs
survey = 'acs1'

## County Data
get_acs1_county_data <- function(year) {
  temp_acs1_county_data <- get_acs(
    survey = 'acs1',
    geography = 'county',  
    variables = var_list, 
    year = year,
    output = 'wide'
  )
  temp_acs1_county_data$YEAR <- year
  return(temp_acs1_county_data)
}

acs1_county_data <- lapply(2012:2019, get_acs1_county_data) %>% bind_rows()
## renaming labels
for (col_name in names(acs1_county_data)) {
  if (col_name == "GEOID" || col_name == "NAME"){
    next
  }
  var_label(acs1_county_data[[col_name]] ) <- (filter(acs5_vars, name == str_sub(col_name, start = 1L, end = -2)))$label
}


## Metropolitan Data
get_acs1_metro_data <- function(year) {
  temp_acs1_metro_data <- get_acs(
    survey = 'acs1',
    geography = 'metropolitan statistical area/micropolitan statistical area',  
    variables = var_list, 
    year = year,
    output = 'wide'
  )
  temp_acs1_metro_data$YEAR <- year
  return(temp_acs1_metro_data)
}

acs1_metro_data <- lapply(2012:2019, get_acs1_metro_data) %>% bind_rows()
## renaming labels
for (col_name in names(acs1_metro_data)) {
  if (col_name == "GEOID" || col_name == "NAME"){
    next
  }
  var_label(acs1_metro_data[[col_name]] ) <- (filter(acs5_vars, name == str_sub(col_name, start = 1L, end = -2)))$label
}


## State Data
get_acs1_state_data <- function(year) {
  temp_acs1_state_data <- get_acs(
    survey = 'acs1',
    geography = 'state',  
    variables = var_list, 
    year = year,
    output = 'wide'
  )
  temp_acs1_state_data$YEAR <- year
  return(temp_acs1_state_data)
}

acs1_state_data <- lapply(2012:2019, get_acs1_state_data) %>% bind_rows()
## renaming labels
for (col_name in names(acs1_state_data)) {
  if (col_name == "GEOID" || col_name == "NAME"){
    next
  }
  var_label(acs1_state_data[[col_name]] ) <- (filter(acs5_vars, name == str_sub(col_name, start = 1L, end = -2)))$label
}


## Saving Data to RData Files
## datasets to save:
acs5_vars
acs1_county_data
acs1_metro_data
acs1_state_data
acs5_county_data
acs5_metro_data
acs5_state_data

getwd()
list.files()
dir.create(path = "datasets")
list.files()
data_dir <- "datasets"
save(acs5_vars, acs1_county_data, acs1_metro_data, acs1_state_data, acs5_county_data, acs5_metro_data, acs5_state_data,
     file = file.path(data_dir, 'datasets.RData'))


## Tables and Line Graphs 
## libraries
library(ggplot2) 
library(scales)
library(haven)

## acs1 data
acs1_county_data
acs1_metro_data
acs1_state_data

## acs5 data
acs5_county_data
acs5_metro_data
acs5_state_data

## education attainment 2012-2020
## acs1 - education attainment, county level
temp_edu_attain1 <- acs1_county_data %>% 
  select(GEOID, NAME, YEAR, matches("C15002I_\\d+E")) %>% 
  mutate(C15002I_012E = C15002I_003E + C15002I_008E,
         C15002I_013E = C15002I_004E + C15002I_009E,
         C15002I_014E = C15002I_005E + C15002I_010E,
         C15002I_015E = C15002I_006E + C15002I_011E) %>%
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'attainment',         
    values_to = 'values') %>%
  mutate(gender = case_when(attainment %in%
                              c("C15002I_002E", "C15002I_003E", "C15002I_004E", "C15002I_005E", "C15002I_006E") ~ "latino male",
                            attainment %in%
                              c("C15002I_007E", "C15002I_008E", "C15002I_009E", "C15002I_010E", "C15002I_011E") ~ "latina female",
                            TRUE ~ "total"
  ),
  education_level = case_when(attainment %in%
                                c("C15002I_003E", "C15002I_008E", "C15002I_012E") ~ "less than a high school diploma",
                              attainment %in%
                                c("C15002I_004E", "C15002I_009E", "C15002I_013E") ~ "high school graduate",
                              attainment %in%
                                c("C15002I_005E", "C15002I_010E", "C15002I_014E") ~ "some college or associate’s degree",
                              attainment %in%
                                c("C15002I_006E", "C15002I_011E", "C15002I_015E") ~ "bachelor’s degree or higher",
                              TRUE ~ "total")
  ) %>% 
  filter(education_level != "total", gender != "total") %>%
  group_by(GEOID, NAME, YEAR, gender) %>% 
  mutate(pct = values/sum(values))

# graph, los angeles county
temp_edu_attain1 %>% filter(NAME == "Los Angeles County, California") %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() + 
  ggtitle('Latinx Education Attainment Levels in Los Angeles County, California (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_eduattain_lacounty.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_eduattain_lacounty.png", width = 10,   height = 6)



# graph, counties: Cook County, IL, Maricopa County, AZ, Orange County, CA, Bexar County, TX, Riverside County, CA, San Bernardino County, CA, San Diego County, CA
temp_edu_attain1 %>% filter(NAME %in% c("Los Angeles County, California", "Cook County, Illinois","Maricopa County, Arizona", "Orange County, California", "Bexar County, Texas",
                                        "Riverside County, California", "San Bernardino County, California", "San Diego County, California")) %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Latinx Education Attainment Levels in Counties Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_eduattain_counties.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_eduattain_counties.png", width = 10,   height = 6)




## acs1 - education attainment, metro level 
temp_edu_attain2 <- acs1_metro_data %>% 
  select(GEOID, NAME, YEAR, matches("C15002I_\\d+E")) %>% 
  mutate(C15002I_012E = C15002I_003E + C15002I_008E,
         C15002I_013E = C15002I_004E + C15002I_009E,
         C15002I_014E = C15002I_005E + C15002I_010E,
         C15002I_015E = C15002I_006E + C15002I_011E) %>%
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'attainment',         
    values_to = 'values') %>%
  mutate(gender = case_when(attainment %in%
                              c("C15002I_002E", "C15002I_003E", "C15002I_004E", "C15002I_005E", "C15002I_006E") ~ "male",
                            attainment %in%
                              c("C15002I_007E", "C15002I_008E", "C15002I_009E", "C15002I_010E", "C15002I_011E") ~ "female",
                            TRUE ~ "total"
  ),
  education_level = case_when(attainment %in%
                                c("C15002I_003E", "C15002I_008E", "C15002I_012E") ~ "less than a high school diploma",
                              attainment %in%
                                c("C15002I_004E", "C15002I_009E", "C15002I_013E") ~ "high school graduate",
                              attainment %in%
                                c("C15002I_005E", "C15002I_010E", "C15002I_014E") ~ "some college or associate’s degree",
                              attainment %in%
                                c("C15002I_006E", "C15002I_011E", "C15002I_015E") ~ "bachelor’s degree or higher",
                              TRUE ~ "total")
  ) %>% 
  filter(education_level != "total", gender != "total") %>%
  group_by(GEOID, NAME, YEAR, gender) %>% 
  mutate(pct = values/sum(values))

# graph, metro: Los Angeles-Long Beach-Anaheim, CA Metro Area; New York-Newark-Jersey City, NY-NJ-PA Metro Area;
# Miami-Fort Lauderdale-West Palm Beach, FL Metro Area; Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area; 
# Providence-Warwick, RI-MA Metro Area
temp_edu_attain2 %>% filter(NAME == "Los Angeles-Long Beach-Anaheim, CA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() +
  ggtitle('Latinx Education Attainment Levels in Los Angeles-Long Beach-Anaheim, CA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_eduattain_metro_la.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_eduattain_metro_la.png", width = 10,   height = 6)

temp_edu_attain2 %>% filter(NAME == "New York-Newark-Jersey City, NY-NJ-PA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() +
  ggtitle('Latinx Education Attainment Levels in New York-Newark-Jersey City, NY-NJ-PA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_eduattain_metro_ny.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_eduattain_metro_ny.png", width = 10,   height = 6)

temp_edu_attain2 %>% filter(NAME == "Miami-Fort Lauderdale-West Palm Beach, FL Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() +
  ggtitle('Latinx Education Attainment Levels in Miami-Fort Lauderdale-West Palm Beach, FL Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_eduattain_metro_miami.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_eduattain_metro_miami.png", width = 10,   height = 6)

temp_edu_attain2 %>% filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() +
  ggtitle('Latinx Education Attainment Levels in Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_eduattain_metro_dmv.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_eduattain_metro_dmv.png", width = 10,   height = 6)

temp_edu_attain2 %>% filter(NAME == "Providence-Warwick, RI-MA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() +
  ggtitle('Latinx Education Attainment Levels in Providence-Warwick, RI-MA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_eduattain_metro_providence.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_eduattain_metro_providence.png", width = 10,   height = 6)






## acs1 - education attainment, state level
temp_edu_attain3 <- acs1_state_data %>% 
  select(GEOID, NAME, YEAR, matches("C15002I_\\d+E")) %>% 
  mutate(C15002I_012E = C15002I_003E + C15002I_008E,
         C15002I_013E = C15002I_004E + C15002I_009E,
         C15002I_014E = C15002I_005E + C15002I_010E,
         C15002I_015E = C15002I_006E + C15002I_011E) %>%
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'attainment',         
    values_to = 'values') %>%
  mutate(gender = case_when(attainment %in%
                              c("C15002I_002E", "C15002I_003E", "C15002I_004E", "C15002I_005E", "C15002I_006E") ~ "male",
                            attainment %in%
                              c("C15002I_007E", "C15002I_008E", "C15002I_009E", "C15002I_010E", "C15002I_011E") ~ "female",
                            TRUE ~ "total"
  ),
  education_level = case_when(attainment %in%
                                c("C15002I_003E", "C15002I_008E", "C15002I_012E") ~ "less than a high school diploma",
                              attainment %in%
                                c("C15002I_004E", "C15002I_009E", "C15002I_013E") ~ "high school graduate",
                              attainment %in%
                                c("C15002I_005E", "C15002I_010E", "C15002I_014E") ~ "some college or associate’s degree",
                              attainment %in%
                                c("C15002I_006E", "C15002I_011E", "C15002I_015E") ~ "bachelor’s degree or higher",
                              TRUE ~ "total")
  ) %>% 
  filter(education_level != "total", gender != "total") %>%
  group_by(GEOID, NAME, YEAR, gender) %>% 
  mutate(pct = values/sum(values))
# graph, states: CA, TX, FL, NY
temp_edu_attain3 %>% filter(NAME %in% c("California", "Texas", "Florida", "New York")) %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Latinx Education Attainment Levels in States Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_eduattain_states.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_eduattain_states.png", width = 10,   height = 6)





## acs5 - education attainment, county level
temp_edu_attain4 <- acs5_county_data %>% 
  select(GEOID, NAME, YEAR, matches("C15002I_\\d+E")) %>% 
  mutate(C15002I_012E = C15002I_003E + C15002I_008E,
         C15002I_013E = C15002I_004E + C15002I_009E,
         C15002I_014E = C15002I_005E + C15002I_010E,
         C15002I_015E = C15002I_006E + C15002I_011E) %>%
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'attainment',         
    values_to = 'values') %>%
  mutate(gender = case_when(attainment %in%
                              c("C15002I_002E", "C15002I_003E", "C15002I_004E", "C15002I_005E", "C15002I_006E") ~ "latino male",
                            attainment %in%
                              c("C15002I_007E", "C15002I_008E", "C15002I_009E", "C15002I_010E", "C15002I_011E") ~ "latina female",
                            TRUE ~ "total"
  ),
  education_level = case_when(attainment %in%
                                c("C15002I_003E", "C15002I_008E", "C15002I_012E") ~ "less than a high school diploma",
                              attainment %in%
                                c("C15002I_004E", "C15002I_009E", "C15002I_013E") ~ "high school graduate",
                              attainment %in%
                                c("C15002I_005E", "C15002I_010E", "C15002I_014E") ~ "some college or associate’s degree",
                              attainment %in%
                                c("C15002I_006E", "C15002I_011E", "C15002I_015E") ~ "bachelor’s degree or higher",
                              TRUE ~ "total")
  ) %>% 
  filter(education_level != "total", gender != "total") %>%
  group_by(GEOID, NAME, YEAR, gender) %>% 
  mutate(pct = values/sum(values))

# graph, los angeles county
temp_edu_attain4 %>% filter(NAME == "Los Angeles County, California") %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() + 
  ggtitle('Latinx Education Attainment Levels in Los Angeles County, California (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_eduattain_lacounty.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_eduattain_lacounty.png", width = 10,   height = 6)



# graph, counties: Cook County, IL, Maricopa County, AZ, Orange County, CA, Bexar County, TX, Riverside County, CA, San Bernardino County, CA, San Diego County, CA
temp_edu_attain4 %>% filter(NAME %in% c("Los Angeles County, California", "Cook County, Illinois","Maricopa County, Arizona", "Orange County, California", "Bexar County, Texas",
                                        "Riverside County, California", "San Bernardino County, California", "San Diego County, California")) %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Latinx Education Attainment Levels in Counties Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_eduattain_counties.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_eduattain_counties.png", width = 10,   height = 6)





## acs5 - education attainment, metro level
temp_edu_attain5 <- acs5_metro_data %>% 
  select(GEOID, NAME, YEAR, matches("C15002I_\\d+E")) %>% 
  mutate(C15002I_012E = C15002I_003E + C15002I_008E,
         C15002I_013E = C15002I_004E + C15002I_009E,
         C15002I_014E = C15002I_005E + C15002I_010E,
         C15002I_015E = C15002I_006E + C15002I_011E) %>%
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'attainment',         
    values_to = 'values') %>%
  mutate(gender = case_when(attainment %in%
                              c("C15002I_002E", "C15002I_003E", "C15002I_004E", "C15002I_005E", "C15002I_006E") ~ "male",
                            attainment %in%
                              c("C15002I_007E", "C15002I_008E", "C15002I_009E", "C15002I_010E", "C15002I_011E") ~ "female",
                            TRUE ~ "total"
  ),
  education_level = case_when(attainment %in%
                                c("C15002I_003E", "C15002I_008E", "C15002I_012E") ~ "less than a high school diploma",
                              attainment %in%
                                c("C15002I_004E", "C15002I_009E", "C15002I_013E") ~ "high school graduate",
                              attainment %in%
                                c("C15002I_005E", "C15002I_010E", "C15002I_014E") ~ "some college or associate’s degree",
                              attainment %in%
                                c("C15002I_006E", "C15002I_011E", "C15002I_015E") ~ "bachelor’s degree or higher",
                              TRUE ~ "total")
  ) %>% 
  filter(education_level != "total", gender != "total") %>%
  group_by(GEOID, NAME, YEAR, gender) %>% 
  mutate(pct = values/sum(values))

# graph, metro: Los Angeles-Long Beach-Anaheim, CA Metro Area; New York-Newark-Jersey City, NY-NJ-PA Metro Area;
# Miami-Fort Lauderdale-West Palm Beach, FL Metro Area; Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area; 
# Providence-Warwick, RI-MA Metro Area
temp_edu_attain5 %>% filter(NAME == "Los Angeles-Long Beach-Anaheim, CA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() +
  ggtitle('Latinx Education Attainment Levels in Los Angeles-Long Beach-Anaheim, CA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_eduattain_metro_la.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_eduattain_metro_la.png", width = 10,   height = 6)

temp_edu_attain5 %>% filter(NAME == "New York-Newark-Jersey City, NY-NJ-PA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() +
  ggtitle('Latinx Education Attainment Levels in New York-Newark-Jersey City, NY-NJ-PA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_eduattain_metro_ny.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_eduattain_metro_ny.png", width = 10,   height = 6)

temp_edu_attain5 %>% filter(NAME == "Miami-Fort Lauderdale-West Palm Beach, FL Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() +
  ggtitle('Latinx Education Attainment Levels in Miami-Fort Lauderdale-West Palm Beach, FL Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_eduattain_metro_miami.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_eduattain_metro_miami.png", width = 10,   height = 6)

temp_edu_attain5 %>% filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() +
  ggtitle('Latinx Education Attainment Levels in Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_eduattain_metro_dmv.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_eduattain_metro_dmv.png", width = 10,   height = 6)

temp_edu_attain5 %>% filter(NAME == "Providence-Warwick, RI-MA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() +
  ggtitle('Latinx Education Attainment Levels in Providence-Warwick, RI-MA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_eduattain_metro_providence.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_eduattain_metro_providence.png", width = 10,   height = 6)





## acs5 - education attainment, state level
temp_edu_attain6 <- acs5_state_data %>% 
  select(GEOID, NAME, YEAR, matches("C15002I_\\d+E")) %>% 
  mutate(C15002I_012E = C15002I_003E + C15002I_008E,
         C15002I_013E = C15002I_004E + C15002I_009E,
         C15002I_014E = C15002I_005E + C15002I_010E,
         C15002I_015E = C15002I_006E + C15002I_011E) %>%
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'attainment',         
    values_to = 'values') %>%
  mutate(gender = case_when(attainment %in%
                              c("C15002I_002E", "C15002I_003E", "C15002I_004E", "C15002I_005E", "C15002I_006E") ~ "male",
                            attainment %in%
                              c("C15002I_007E", "C15002I_008E", "C15002I_009E", "C15002I_010E", "C15002I_011E") ~ "female",
                            TRUE ~ "total"
  ),
  education_level = case_when(attainment %in%
                                c("C15002I_003E", "C15002I_008E", "C15002I_012E") ~ "less than a high school diploma",
                              attainment %in%
                                c("C15002I_004E", "C15002I_009E", "C15002I_013E") ~ "high school graduate",
                              attainment %in%
                                c("C15002I_005E", "C15002I_010E", "C15002I_014E") ~ "some college or associate’s degree",
                              attainment %in%
                                c("C15002I_006E", "C15002I_011E", "C15002I_015E") ~ "bachelor’s degree or higher",
                              TRUE ~ "total")
  ) %>% 
  filter(education_level != "total", gender != "total") %>%
  group_by(GEOID, NAME, YEAR, gender) %>% 
  mutate(pct = values/sum(values))
# graph, states: CA, TX, FL, NY
temp_edu_attain6 %>% filter(NAME %in% c("California", "Texas", "Florida", "New York")) %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Latinx Education Attainment Levels in States Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_eduattain_states.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_eduattain_states.png", width = 10,   height = 6)















## education enrollment (2012-2020)
## acs1 - education enrollment, county level 
temp_edu_enr7 <- acs1_county_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007I_\\d+E")) %>% 
  mutate(B14007I_020E = B14007I_004E + B14007I_005E + B14007I_006E
         + B14007I_007E + B14007I_008E + B14007I_009E + B14007I_010E
         + B14007I_011E + B14007I_012E + B14007I_013E + B14007I_014E
         + B14007I_015E + B14007I_016E) %>%
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'enrollment',         
    values_to = 'values') %>%
  filter(enrollment %in% c("B14007I_003E", "B14007I_020E", "B14007I_017E", "B14007I_018E")) %>%
  mutate(
    enrollment_level = case_when(enrollment == "B14007I_002E" ~ "enrolled in school",
                                 enrollment == "B14007I_003E" ~ "enrolled in nursery school",
                                 enrollment == "B14007I_017E" ~ "enrolled in college/universities",
                                 enrollment == "B14007I_018E" ~ "enrolled in graduate or professional school",
                                 enrollment == "B14007I_019E" ~ "not enrolled in school",
                                 enrollment == "B14007I_020E" ~ "enrolled in k-12", 
                                 TRUE ~ "total")) %>%
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))

## graph, los angeles county, california
temp_edu_enr7 %>% filter(NAME == "Los Angeles County, California") %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() + 
  ggtitle('Latinx Education Enrollment Levels in Los Angeles County, California (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_enrollment_lacounty.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_enrollment_lacounty.png", width = 10,   height = 6)

## counties
temp_edu_enr7 %>% filter(NAME %in% c("Los Angeles County, California", "Cook County, Illinois","Maricopa County, Arizona", "Orange County, California", "Bexar County, Texas",
                                        "Riverside County, California", "San Bernardino County, California", "San Diego County, California")) %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Latinx Education Enrollment Levels in Counties Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_enrollment_counties.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_enrollment_counties.png", width = 10,   height = 6)





## acs1 - education enrollment, metro level 
temp_edu_enr8 <- acs1_metro_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007I_\\d+E")) %>% 
  mutate(B14007I_020E = B14007I_004E + B14007I_005E + B14007I_006E
         + B14007I_007E + B14007I_008E + B14007I_009E + B14007I_010E
         + B14007I_011E + B14007I_012E + B14007I_013E + B14007I_014E
         + B14007I_015E + B14007I_016E) %>%
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'enrollment',         
    values_to = 'values') %>%
  filter(enrollment %in% c("B14007I_003E", "B14007I_020E", "B14007I_017E", "B14007I_018E")) %>%
  mutate(
    enrollment_level = case_when(enrollment == "B14007I_002E" ~ "enrolled in school",
                                 enrollment == "B14007I_003E" ~ "enrolled in nursery school",
                                 enrollment == "B14007I_017E" ~ "enrolled in college/universities",
                                 enrollment == "B14007I_018E" ~ "enrolled in graduate or professional school",
                                 enrollment == "B14007I_019E" ~ "not enrolled in school",
                                 enrollment == "B14007I_020E" ~ "enrolled in k-12", 
                                 TRUE ~ "total")) %>%
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))

# graph, metro: Los Angeles-Long Beach-Anaheim, CA Metro Area; New York-Newark-Jersey City, NY-NJ-PA Metro Area;
# Miami-Fort Lauderdale-West Palm Beach, FL Metro Area; Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area; 
# Providence-Warwick, RI-MA Metro Area
temp_edu_enr8 %>% filter(NAME == "Los Angeles-Long Beach-Anaheim, CA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() +
  ggtitle('Latinx Education Enrollnment Levels in Los Angeles-Long Beach-Anaheim, CA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_la.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_la.png", width = 10,   height = 6)

temp_edu_enr8 %>% filter(NAME == "New York-Newark-Jersey City, NY-NJ-PA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() +
  ggtitle('Latinx Education Enrollment Levels in New York-Newark-Jersey City, NY-NJ-PA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_ny.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_ny.png", width = 10,   height = 6)

temp_edu_enr8 %>% filter(NAME == "Miami-Fort Lauderdale-West Palm Beach, FL Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() +
  ggtitle('Latinx Education Enrollment Levels in Miami-Fort Lauderdale-West Palm Beach, FL Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_miami.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_miami.png", width = 10,   height = 6)

temp_edu_enr8 %>% filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() +
  ggtitle('Latinx Education Enrollment Levels in Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_dmv.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_dmv.png", width = 10,   height = 6)

temp_edu_enr8 %>% filter(NAME == "Providence-Warwick, RI-MA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() +
  ggtitle('Latinx Education Enrollment Levels in Providence-Warwick, RI-MA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_providence.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_providence.png", width = 10,   height = 6)








## acs1 - education enrollment, state level
temp_edu_enr9 <- acs1_state_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007I_\\d+E")) %>% 
  mutate(B14007I_020E = B14007I_004E + B14007I_005E + B14007I_006E
         + B14007I_007E + B14007I_008E + B14007I_009E + B14007I_010E
         + B14007I_011E + B14007I_012E + B14007I_013E + B14007I_014E
         + B14007I_015E + B14007I_016E) %>%
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'enrollment',         
    values_to = 'values') %>%
  filter(enrollment %in% c("B14007I_003E", "B14007I_020E", "B14007I_017E", "B14007I_018E")) %>%
  mutate(
    enrollment_level = case_when(enrollment == "B14007I_002E" ~ "enrolled in school",
                                 enrollment == "B14007I_003E" ~ "enrolled in nursery school",
                                 enrollment == "B14007I_017E" ~ "enrolled in college/universities",
                                 enrollment == "B14007I_018E" ~ "enrolled in graduate or professional school",
                                 enrollment == "B14007I_019E" ~ "not enrolled in school",
                                 enrollment == "B14007I_020E" ~ "enrolled in k-12", 
                                 TRUE ~ "total")) %>%
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, states: CA, TX, FL, NY
temp_edu_enr9 %>% filter(NAME %in% c("California", "Texas", "Florida", "New York")) %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Latinx Education Enrollment Levels in States Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_states.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_states.png", width = 10,   height = 6)


## acs5 - education enrollment, county level 
temp_edu_enr10 <- acs5_county_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007I_\\d+E")) %>% 
  mutate(B14007I_020E = B14007I_004E + B14007I_005E + B14007I_006E
         + B14007I_007E + B14007I_008E + B14007I_009E + B14007I_010E
         + B14007I_011E + B14007I_012E + B14007I_013E + B14007I_014E
         + B14007I_015E + B14007I_016E) %>%
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'enrollment',         
    values_to = 'values') %>%
  filter(enrollment %in% c("B14007I_003E", "B14007I_020E", "B14007I_017E", "B14007I_018E")) %>%
  mutate(
    enrollment_level = case_when(enrollment == "B14007I_002E" ~ "enrolled in school",
                                 enrollment == "B14007I_003E" ~ "enrolled in nursery school",
                                 enrollment == "B14007I_017E" ~ "enrolled in college/universities",
                                 enrollment == "B14007I_018E" ~ "enrolled in graduate or professional school",
                                 enrollment == "B14007I_019E" ~ "not enrolled in school",
                                 enrollment == "B14007I_020E" ~ "enrolled in k-12", 
                                 TRUE ~ "total")) %>%
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))

## graph, los angeles county, california
temp_edu_enr10 %>% filter(NAME == "Los Angeles County, California") %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() + 
  ggtitle('Latinx Education Enrollment Levels in Los Angeles County, California (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_enrollment_lacounty.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_enrollment_lacounty.png", width = 10,   height = 6)

## counties
temp_edu_enr10 %>% filter(NAME %in% c("Los Angeles County, California", "Cook County, Illinois","Maricopa County, Arizona", "Orange County, California", "Bexar County, Texas",
                                     "Riverside County, California", "San Bernardino County, California", "San Diego County, California")) %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Latinx Education Enrollment Levels in Counties Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_enrollment_counties.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_enrollment_counties.png", width = 10,   height = 6)





## acs5 - education enrollment, metro level 
temp_edu_enr11 <- acs5_metro_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007I_\\d+E")) %>% 
  mutate(B14007I_020E = B14007I_004E + B14007I_005E + B14007I_006E
         + B14007I_007E + B14007I_008E + B14007I_009E + B14007I_010E
         + B14007I_011E + B14007I_012E + B14007I_013E + B14007I_014E
         + B14007I_015E + B14007I_016E) %>%
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'enrollment',         
    values_to = 'values') %>%
  filter(enrollment %in% c("B14007I_003E", "B14007I_020E", "B14007I_017E", "B14007I_018E")) %>%
  mutate(
    enrollment_level = case_when(enrollment == "B14007I_002E" ~ "enrolled in school",
                                 enrollment == "B14007I_003E" ~ "enrolled in nursery school",
                                 enrollment == "B14007I_017E" ~ "enrolled in college/universities",
                                 enrollment == "B14007I_018E" ~ "enrolled in graduate or professional school",
                                 enrollment == "B14007I_019E" ~ "not enrolled in school",
                                 enrollment == "B14007I_020E" ~ "enrolled in k-12", 
                                 TRUE ~ "total")) %>%
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, metro: Los Angeles-Long Beach-Anaheim, CA Metro Area; New York-Newark-Jersey City, NY-NJ-PA Metro Area;
# Miami-Fort Lauderdale-West Palm Beach, FL Metro Area; Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area; 
# Providence-Warwick, RI-MA Metro Area
temp_edu_enr11 %>% filter(NAME == "Los Angeles-Long Beach-Anaheim, CA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() +
  ggtitle('Latinx Education Enrollnment Levels in Los Angeles-Long Beach-Anaheim, CA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_la.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_la.png", width = 10,   height = 6)

temp_edu_enr11 %>% filter(NAME == "New York-Newark-Jersey City, NY-NJ-PA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() +
  ggtitle('Latinx Education Enrollment Levels in New York-Newark-Jersey City, NY-NJ-PA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_ny.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_ny.png", width = 10,   height = 6)

temp_edu_enr11 %>% filter(NAME == "Miami-Fort Lauderdale-West Palm Beach, FL Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() +
  ggtitle('Latinx Education Enrollment Levels in Miami-Fort Lauderdale-West Palm Beach, FL Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_miami.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_miami.png", width = 10,   height = 6)

temp_edu_enr11 %>% filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() +
  ggtitle('Latinx Education Enrollment Levels in Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_dmv.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_dmv.png", width = 10,   height = 6)

temp_edu_enr11 %>% filter(NAME == "Providence-Warwick, RI-MA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() +
  ggtitle('Latinx Education Enrollment Levels in Providence-Warwick, RI-MA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_providence.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_providence.png", width = 10,   height = 6)







## acs5 - education enrollment, state level
temp_edu_enr12 <- acs5_state_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007I_\\d+E")) %>% 
  mutate(B14007I_020E = B14007I_004E + B14007I_005E + B14007I_006E
         + B14007I_007E + B14007I_008E + B14007I_009E + B14007I_010E
         + B14007I_011E + B14007I_012E + B14007I_013E + B14007I_014E
         + B14007I_015E + B14007I_016E) %>%
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'enrollment',         
    values_to = 'values') %>%
  filter(enrollment %in% c("B14007I_003E", "B14007I_020E", "B14007I_017E", "B14007I_018E")) %>%
  mutate(
    enrollment_level = case_when(enrollment == "B14007I_002E" ~ "enrolled in school",
                                 enrollment == "B14007I_003E" ~ "enrolled in nursery school",
                                 enrollment == "B14007I_017E" ~ "enrolled in college/universities",
                                 enrollment == "B14007I_018E" ~ "enrolled in graduate or professional school",
                                 enrollment == "B14007I_019E" ~ "not enrolled in school",
                                 enrollment == "B14007I_020E" ~ "enrolled in k-12", 
                                 TRUE ~ "total")) %>%
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, states: CA, TX, FL, NY
temp_edu_enr12 %>% filter(NAME %in% c("California", "Texas", "Florida", "New York")) %>%
  ggplot(aes(x = YEAR, y = pct, color = enrollment_level)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Latinx Education Enrollment Levels in States Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_states.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_states.png", width = 10,   height = 6)








##### Ethnic/Racial Groups ####
## acs1 - education attainment, county level (total educational attainment rates (aggregated from: total less than a high school diploma, 
## total high school graduate, total some college or associate’s degree, total bachelor’s degree or higher))
temp_edu_attain_race13 <- acs1_county_data %>% 
  select(GEOID, NAME, YEAR, matches("C15002\\w_001E")) %>% 
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "C15002A_001E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
   race,  "C15002B_001E" = "Black", 
   "C15002C_001E" = "American Indian and Alaska Native",
   "C15002D_001E" = "Asian",
   "C15002E_001E" = "Native Hawaiian and Other Pacific Islander",
   "C15002F_001E" = "Other",
   "C15002G_001E" = "Two or More Races",
   "C15002H_001E" = "white",
   "C15002I_001E" = "Latino"
  )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
## graphs, counties
temp_edu_attain_race13 %>% filter(NAME %in% c("Los Angeles County, California", "Cook County, Illinois","Maricopa County, Arizona", "Orange County, California", "Bexar County, Texas",
                                        "Riverside County, California", "San Bernardino County, California", "San Diego County, California")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Education Attainment Levels in Counties By Race (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_attainment_counties_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_attainment_counties_race.png", width = 10,   height = 6)




## acs1 - education attainment, metro level (total educational attainment rates (aggregated from: total less than a high school diploma, 
## total high school graduate, total some college or associate’s degree, total bachelor’s degree or higher))
temp_edu_attain_race14 <- acs1_metro_data %>% 
  select(GEOID, NAME, YEAR, matches("C15002\\w_001E")) %>% 
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "C15002A_001E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "C15002B_001E" = "Black", 
      "C15002C_001E" = "American Indian and Alaska Native",
      "C15002D_001E" = "Asian",
      "C15002E_001E" = "Native Hawaiian and Other Pacific Islander",
      "C15002F_001E" = "Other",
      "C15002G_001E" = "Two or More Races",
      "C15002H_001E" = "white",
      "C15002I_001E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, metro: Los Angeles-Long Beach-Anaheim, CA Metro Area; New York-Newark-Jersey City, NY-NJ-PA Metro Area;
# Miami-Fort Lauderdale-West Palm Beach, FL Metro Area; Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area; 
# Providence-Warwick, RI-MA Metro Area
temp_edu_attain_race14 %>% filter(NAME == "Los Angeles-Long Beach-Anaheim, CA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Attainment Levels By Race in Los Angeles-Long Beach-Anaheim, CA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_attainment_metro_la_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_attainment_metro_la_race.png", width = 10,   height = 6)

temp_edu_attain_race14 %>% filter(NAME == "New York-Newark-Jersey City, NY-NJ-PA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Attainment Levels By Race in New York-Newark-Jersey City, NY-NJ-PA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_attainment_metro_ny_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_attainment_metro_ny_race.png", width = 10,   height = 6)

temp_edu_attain_race14 %>% filter(NAME == "Miami-Fort Lauderdale-West Palm Beach, FL Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Attainment Levels By Race in Miami-Fort Lauderdale-West Palm Beach, FL Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_attainment_metro_miami_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_attainment_metro_miami_race.png", width = 10,   height = 6)

temp_edu_attain_race14 %>% filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Attainment Levels By Race in Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_attainment_metro_dmv_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_attainment_metro_dmv_race.png", width = 10,   height = 6)

temp_edu_attain_race14 %>% filter(NAME == "Providence-Warwick, RI-MA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Attainment Levels By Race in Providence-Warwick, RI-MA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_attainment_metro_providence_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_attainment_metro_providence_race.png", width = 10,   height = 6)




## acs1 - education attainment, state level (total educational attainment rates (aggregated from: total less than a high school diploma, 
## total high school graduate, total some college or associate’s degree, total bachelor’s degree or higher))
temp_edu_attain_race15 <- acs1_state_data %>% 
  select(GEOID, NAME, YEAR, matches("C15002\\w_001E")) %>% 
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "C15002A_001E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "C15002B_001E" = "Black", 
      "C15002C_001E" = "American Indian and Alaska Native",
      "C15002D_001E" = "Asian",
      "C15002E_001E" = "Native Hawaiian and Other Pacific Islander",
      "C15002F_001E" = "Other",
      "C15002G_001E" = "Two or More Races",
      "C15002H_001E" = "white",
      "C15002I_001E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, states: CA, TX, FL, NY
temp_edu_attain_race15 %>% filter(NAME %in% c("California", "Texas", "Florida", "New York")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Education Attainment Levels By Race in States Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_attainment_states_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_attainment_states_race.png", width = 10,   height = 6)





## acs5 - education attainment, county level (total educational attainment rates (aggregated from: total less than a high school diploma, 
## total high school graduate, total some college or associate’s degree, total bachelor’s degree or higher))
temp_edu_attain_race16 <- acs5_county_data %>% 
  select(GEOID, NAME, YEAR, matches("C15002\\w_001E")) %>% 
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "C15002A_001E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "C15002B_001E" = "Black", 
      "C15002C_001E" = "American Indian and Alaska Native",
      "C15002D_001E" = "Asian",
      "C15002E_001E" = "Native Hawaiian and Other Pacific Islander",
      "C15002F_001E" = "Other",
      "C15002G_001E" = "Two or More Races",
      "C15002H_001E" = "white",
      "C15002I_001E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
## graphs, counties
temp_edu_attain_race16 %>% filter(NAME %in% c("Los Angeles County, California", "Cook County, Illinois","Maricopa County, Arizona", "Orange County, California", "Bexar County, Texas",
                                              "Riverside County, California", "San Bernardino County, California", "San Diego County, California")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Education Attainment Levels in Counties By Race (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_attainment_counties_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_attainment_counties_race.png", width = 10,   height = 6)




## acs5 - education attainment, metro level (total educational attainment rates (aggregated from: total less than a high school diploma, 
## total high school graduate, total some college or associate’s degree, total bachelor’s degree or higher))
temp_edu_attain_race17 <- acs5_metro_data %>% 
  select(GEOID, NAME, YEAR, matches("C15002\\w_001E")) %>% 
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "C15002A_001E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "C15002B_001E" = "Black", 
      "C15002C_001E" = "American Indian and Alaska Native",
      "C15002D_001E" = "Asian",
      "C15002E_001E" = "Native Hawaiian and Other Pacific Islander",
      "C15002F_001E" = "Other",
      "C15002G_001E" = "Two or More Races",
      "C15002H_001E" = "white",
      "C15002I_001E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, metro: Los Angeles-Long Beach-Anaheim, CA Metro Area; New York-Newark-Jersey City, NY-NJ-PA Metro Area;
# Miami-Fort Lauderdale-West Palm Beach, FL Metro Area; Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area; 
# Providence-Warwick, RI-MA Metro Area
temp_edu_attain_race17 %>% filter(NAME == "Los Angeles-Long Beach-Anaheim, CA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Attainment Levels By Race in Los Angeles-Long Beach-Anaheim, CA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_attainment_metro_la_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_attainment_metro_la_race.png", width = 10,   height = 6)

temp_edu_attain_race17 %>% filter(NAME == "New York-Newark-Jersey City, NY-NJ-PA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Attainment Levels By Race in New York-Newark-Jersey City, NY-NJ-PA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_attainment_metro_ny_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_attainment_metro_ny_race.png", width = 10,   height = 6)

temp_edu_attain_race17 %>% filter(NAME == "Miami-Fort Lauderdale-West Palm Beach, FL Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Attainment Levels By Race in Miami-Fort Lauderdale-West Palm Beach, FL Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_attainment_metro_miami_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_attainment_metro_miami_race.png", width = 10,   height = 6)

temp_edu_attain_race17 %>% filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Attainment Levels By Race in Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_attainment_metro_dmv_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_attainment_metro_dmv_race.png", width = 10,   height = 6)

temp_edu_attain_race17 %>% filter(NAME == "Providence-Warwick, RI-MA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Attainment Levels By Race in Providence-Warwick, RI-MA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_attainment_metro_providence_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_attainment_metro_providence_race.png", width = 10,   height = 6)



## acs5 - education attainment, state level, (total educational attainment rates (aggregated from: total less than a high school diploma, 
## total high school graduate, total some college or associate’s degree, total bachelor’s degree or higher))
temp_edu_attain_race18 <- acs5_state_data %>% 
  select(GEOID, NAME, YEAR, matches("C15002\\w_001E")) %>% ## this number can change to any enrollment level preferred
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "C15002A_001E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "C15002B_001E" = "Black", 
      "C15002C_001E" = "American Indian and Alaska Native",
      "C15002D_001E" = "Asian",
      "C15002E_001E" = "Native Hawaiian and Other Pacific Islander",
      "C15002F_001E" = "Other",
      "C15002G_001E" = "Two or More Races",
      "C15002H_001E" = "white",
      "C15002I_001E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, states: CA, TX, FL, NY
temp_edu_attain_race18 %>% filter(NAME %in% c("California", "Texas", "Florida", "New York")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Education Attainment Levels By Race in States Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_attainment_states_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_attainment_states_race.png", width = 10,   height = 6)






## acs1 - education enrollment, county level, (educational enrollment rates of students enrolled in school (enrolled in school means enrolled in nursery school,
## k-12, college/universities, and graduate or professional school))
temp_edu_enr_race19 <- acs1_county_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_002E")) %>% ## this number can change to any enrollment level preferred
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_002E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_002E" = "Black", 
      "B14007C_002E" = "American Indian and Alaska Native",
      "B14007D_002E" = "Asian",
      "B14007E_002E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_002E" = "Other",
      "B14007G_002E" = "Two or More Races",
      "B14007H_002E" = "white",
      "B14007I_002E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
## graphs, counties
## counties
temp_edu_enr_race19 %>% filter(NAME %in% c("Los Angeles County, California", "Cook County, Illinois","Maricopa County, Arizona", "Orange County, California", "Bexar County, Texas",
                                        "Riverside County, California", "San Bernardino County, California", "San Diego County, California")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Education Enrollment Levels in Counties By Race (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_enrollment_counties_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_enrollment_counties_race.png", width = 10,   height = 6)


## acs1 - education enrollment, metro level (educational enrollment rates of students enrolled in school (enrolled in school means enrolled in nursery school,
## k-12, college/universities, and graduate or professional school))
temp_edu_enr_race20 <- acs1_metro_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_002E")) %>% 
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_002E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_002E" = "Black", 
      "B14007C_002E" = "American Indian and Alaska Native",
      "B14007D_002E" = "Asian",
      "B14007E_002E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_002E" = "Other",
      "B14007G_002E" = "Two or More Races",
      "B14007H_002E" = "white",
      "B14007I_002E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, metro: Los Angeles-Long Beach-Anaheim, CA Metro Area; New York-Newark-Jersey City, NY-NJ-PA Metro Area;
# Miami-Fort Lauderdale-West Palm Beach, FL Metro Area; Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area; 
# Providence-Warwick, RI-MA Metro Area
temp_edu_enr_race20 %>% filter(NAME == "Los Angeles-Long Beach-Anaheim, CA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Los Angeles-Long Beach-Anaheim, CA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_enrollment_metro_la_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_enrollment_metro_la_race.png", width = 10,   height = 6)

temp_edu_enr_race20 %>% filter(NAME == "New York-Newark-Jersey City, NY-NJ-PA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in New York-Newark-Jersey City, NY-NJ-PA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_enrollment_metro_ny_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_enrollment_metro_ny_race.png", width = 10,   height = 6)

temp_edu_enr_race20 %>% filter(NAME == "Miami-Fort Lauderdale-West Palm Beach, FL Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Miami-Fort Lauderdale-West Palm Beach, FL Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_enrollment_metro_miami_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_enrollment_metro_miami_race.png", width = 10,   height = 6)

temp_edu_enr_race20 %>% filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_enrollment_metro_dmv_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_enrollment_metro_dmv_race.png", width = 10,   height = 6)

temp_edu_enr_race20 %>% filter(NAME == "Providence-Warwick, RI-MA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Providence-Warwick, RI-MA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_enrollment_metro_providence_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_enrollment_metro_providence_race.png", width = 10,   height = 6)


## acs1 - education enrollment, state level (educational enrollment rates of students enrolled in school (enrolled in school means enrolled in nursery school,
## k-12, college/universities, and graduate or professional school))
temp_edu_enr_race21 <- acs1_state_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_002E")) %>% 
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_002E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_002E" = "Black", 
      "B14007C_002E" = "American Indian and Alaska Native",
      "B14007D_002E" = "Asian",
      "B14007E_002E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_002E" = "Other",
      "B14007G_002E" = "Two or More Races",
      "B14007H_002E" = "white",
      "B14007I_002E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, states: CA, TX, FL, NY
temp_edu_enr_race21 %>% filter(NAME %in% c("California", "Texas", "Florida", "New York")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Education Enrollment Levels By Race in States Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_enrollment_states_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_enrollment_states_race.png", width = 10,   height = 6)


## acs5 - education enrollment, county level (educational enrollment rates of students enrolled in school (enrolled in school means enrolled in nursery school,
## k-12, college/universities, and graduate or professional school))
temp_edu_enr_race22 <- acs5_county_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_002E")) %>% 
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_002E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_002E" = "Black", 
      "B14007C_002E" = "American Indian and Alaska Native",
      "B14007D_002E" = "Asian",
      "B14007E_002E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_002E" = "Other",
      "B14007G_002E" = "Two or More Races",
      "B14007H_002E" = "white",
      "B14007I_002E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
## graphs, counties
## counties
temp_edu_enr_race22 %>% filter(NAME %in% c("Los Angeles County, California", "Cook County, Illinois","Maricopa County, Arizona", "Orange County, California", "Bexar County, Texas",
                                           "Riverside County, California", "San Bernardino County, California", "San Diego County, California")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Education Enrollment Levels in Counties By Race (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_counties_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_counties_race.png", width = 10,   height = 6)


## acs5 - education enrollment, metro level (educational enrollment rates of students enrolled in school (enrolled in school means enrolled in nursery school,
## k-12, college/universities, and graduate or professional school))
temp_edu_enr_race23 <- acs5_metro_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_002E")) %>% 
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_002E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_002E" = "Black", 
      "B14007C_002E" = "American Indian and Alaska Native",
      "B14007D_002E" = "Asian",
      "B14007E_002E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_002E" = "Other",
      "B14007G_002E" = "Two or More Races",
      "B14007H_002E" = "white",
      "B14007I_002E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, metro: Los Angeles-Long Beach-Anaheim, CA Metro Area; New York-Newark-Jersey City, NY-NJ-PA Metro Area;
# Miami-Fort Lauderdale-West Palm Beach, FL Metro Area; Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area; 
# Providence-Warwick, RI-MA Metro Area
temp_edu_enr_race23 %>% filter(NAME == "Los Angeles-Long Beach-Anaheim, CA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Los Angeles-Long Beach-Anaheim, CA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_la_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_la_race.png", width = 10,   height = 6)

temp_edu_enr_race23 %>% filter(NAME == "New York-Newark-Jersey City, NY-NJ-PA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in New York-Newark-Jersey City, NY-NJ-PA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_ny_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_ny_race.png", width = 10,   height = 6)

temp_edu_enr_race23 %>% filter(NAME == "Miami-Fort Lauderdale-West Palm Beach, FL Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Miami-Fort Lauderdale-West Palm Beach, FL Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_miami_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_miami_race.png", width = 10,   height = 6)

temp_edu_enr_race23 %>% filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_dmv_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_dmv_race.png", width = 10,   height = 6)

temp_edu_enr_race23 %>% filter(NAME == "Providence-Warwick, RI-MA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Providence-Warwick, RI-MA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_providence_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_providence_race.png", width = 10,   height = 6)



## acs5 - education enrollment, state level (educational enrollment rates of students enrolled in school (enrolled in school means enrolled in nursery school,
## k-12, college/universities, and graduate or professional school))
temp_edu_enr_race24 <- acs5_state_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_002E")) %>% 
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_002E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_002E" = "Black", 
      "B14007C_002E" = "American Indian and Alaska Native",
      "B14007D_002E" = "Asian",
      "B14007E_002E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_002E" = "Other",
      "B14007G_002E" = "Two or More Races",
      "B14007H_002E" = "white",
      "B14007I_002E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, states: CA, TX, FL, NY
temp_edu_enr_race24 %>% filter(NAME %in% c("California", "Texas", "Florida", "New York")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Education Enrollment Levels By Race in States Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollnment_states_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_states_race.png", width = 10,   height = 6)










## ENROLLMENT IN NURSERY SCHOOL ###
## acs1 - education enrollment, county level, (educational enrollment rates of students enrolled in nursery school)
temp_edu_enr_race25 <- acs1_county_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_003E")) %>% ## this number can change to any enrollment level preferred
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_003E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_003E" = "Black", 
      "B14007C_003E" = "American Indian and Alaska Native",
      "B14007D_003E" = "Asian",
      "B14007E_003E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_003E" = "Other",
      "B14007G_003E" = "Two or More Races",
      "B14007H_003E" = "white",
      "B14007I_003E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
## graphs, counties
## counties
temp_edu_enr_race25 %>% filter(NAME %in% c("Los Angeles County, California", "Cook County, Illinois","Maricopa County, Arizona", "Orange County, California", "Bexar County, Texas",
                                           "Riverside County, California", "San Bernardino County, California", "San Diego County, California")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Education Enrollment Levels in Counties By Race (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_enrollment_counties_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_enrollment_counties_race.png", width = 10,   height = 6)



## acs1 - education enrollment, state level, (educational enrollment rates of students enrolled in nursery school)
temp_edu_enr_race26 <- acs1_state_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_003E")) %>% 
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_003E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_003E" = "Black", 
      "B14007C_003E" = "American Indian and Alaska Native",
      "B14007D_003E" = "Asian",
      "B14007E_003E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_003E" = "Other",
      "B14007G_003E" = "Two or More Races",
      "B14007H_003E" = "white",
      "B14007I_003E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, states: CA, TX, FL, NY
temp_edu_enr_race26 %>% filter(NAME %in% c("California", "Texas", "Florida", "New York")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Education Enrollment Levels By Race in States Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_enrollment_states_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_enrollment_states_race.png", width = 10,   height = 6)


## acs1 - education enrollment, metro level, (educational enrollment rates of students enrolled in nursery school)
temp_edu_enr_race27 <- acs1_metro_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_003E")) %>% 
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_003E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_003E" = "Black", 
      "B14007C_003E" = "American Indian and Alaska Native",
      "B14007D_003E" = "Asian",
      "B14007E_003E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_003E" = "Other",
      "B14007G_003E" = "Two or More Races",
      "B14007H_003E" = "white",
      "B14007I_003E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, metro: Los Angeles-Long Beach-Anaheim, CA Metro Area; New York-Newark-Jersey City, NY-NJ-PA Metro Area;
# Miami-Fort Lauderdale-West Palm Beach, FL Metro Area; Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area; 
# Providence-Warwick, RI-MA Metro Area
temp_edu_enr_race27 %>% filter(NAME == "Los Angeles-Long Beach-Anaheim, CA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Los Angeles-Long Beach-Anaheim, CA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_la_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_la_race.png", width = 10,   height = 6)

temp_edu_enr_race27 %>% filter(NAME == "New York-Newark-Jersey City, NY-NJ-PA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in New York-Newark-Jersey City, NY-NJ-PA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_ny_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_ny_race.png", width = 10,   height = 6)

temp_edu_enr_race27 %>% filter(NAME == "Miami-Fort Lauderdale-West Palm Beach, FL Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Miami-Fort Lauderdale-West Palm Beach, FL Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_miami_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_miami_race.png", width = 10,   height = 6)

temp_edu_enr_race27 %>% filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_dmv_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_dmv_race.png", width = 10,   height = 6)

temp_edu_enr_race27 %>% filter(NAME == "Providence-Warwick, RI-MA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Providence-Warwick, RI-MA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_providence_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_providence_race.png", width = 10,   height = 6)








## ENROLLMENT IN COLLEGE/UNIVERSITIES ### 
B14007I_017E = enrolled in college/universities
## acs1 - education enrollment, county level, (educational enrollment rates of students enrolled in college/universities)
temp_edu_enr_race28 <- acs1_county_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_017E")) %>% ## this number can change to any enrollment level preferred
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_017E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_017E" = "Black", 
      "B14007C_017E" = "American Indian and Alaska Native",
      "B14007D_017E" = "Asian",
      "B14007E_017E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_017E" = "Other",
      "B14007G_017E" = "Two or More Races",
      "B14007H_017E" = "white",
      "B14007I_017E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
## graphs, counties
## counties
temp_edu_enr_race28 %>% filter(NAME %in% c("Los Angeles County, California", "Cook County, Illinois","Maricopa County, Arizona", "Orange County, California", "Bexar County, Texas",
                                           "Riverside County, California", "San Bernardino County, California", "San Diego County, California")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Education Enrollment Levels in Counties By Race (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))


## acs1 - education enrollment, metro level, (educational enrollment rates of students enrolled in college/universities)
temp_edu_enr_race29 <- acs1_metro_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_017E")) %>% ## this number can change to any enrollment level preferred
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_017E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_017E" = "Black", 
      "B14007C_017E" = "American Indian and Alaska Native",
      "B14007D_017E" = "Asian",
      "B14007E_017E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_017E" = "Other",
      "B14007G_017E" = "Two or More Races",
      "B14007H_017E" = "white",
      "B14007I_017E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, metro: Los Angeles-Long Beach-Anaheim, CA Metro Area; New York-Newark-Jersey City, NY-NJ-PA Metro Area;
# Miami-Fort Lauderdale-West Palm Beach, FL Metro Area; Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area; 
# Providence-Warwick, RI-MA Metro Area
temp_edu_enr_race29 %>% filter(NAME == "Los Angeles-Long Beach-Anaheim, CA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Los Angeles-Long Beach-Anaheim, CA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_la_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_la_race.png", width = 10,   height = 6)

temp_edu_enr_race29 %>% filter(NAME == "New York-Newark-Jersey City, NY-NJ-PA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in New York-Newark-Jersey City, NY-NJ-PA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_ny_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_ny_race.png", width = 10,   height = 6)

temp_edu_enr_race29 %>% filter(NAME == "Miami-Fort Lauderdale-West Palm Beach, FL Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Miami-Fort Lauderdale-West Palm Beach, FL Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_miami_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_miami_race.png", width = 10,   height = 6)

temp_edu_enr_race29 %>% filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_dmv_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_dmv_race.png", width = 10,   height = 6)

temp_edu_enr_race29 %>% filter(NAME == "Providence-Warwick, RI-MA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Providence-Warwick, RI-MA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_providence_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_providence_race.png", width = 10,   height = 6)

## acs1 - education enrollment, state level, (educational enrollment rates of students enrolled in college/universities)
temp_edu_enr_race30 <- acs1_state_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_017E")) %>% ## this number can change to any enrollment level preferred
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_017E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_017E" = "Black", 
      "B14007C_017E" = "American Indian and Alaska Native",
      "B14007D_017E" = "Asian",
      "B14007E_017E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_017E" = "Other",
      "B14007G_017E" = "Two or More Races",
      "B14007H_017E" = "white",
      "B14007I_017E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, states: CA, TX, FL, NY
temp_edu_enr_race30 %>% filter(NAME %in% c("California", "Texas", "Florida", "New York")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Education Enrollment Levels By Race in States Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))


## ENROLLMENT IN GRAD OR PROFESSIONAL SCHOOL ###
B14007I_018E = enrolled in graduate or professional school
## acs1 - education enrollment, county level, (educational enrollment rates of students enrolled in graduate or professional school)
temp_edu_enr_race31 <- acs1_county_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_018E")) %>% ## this number can change to any enrollment level preferred
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_018E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_018E" = "Black", 
      "B14007C_018E" = "American Indian and Alaska Native",
      "B14007D_018E" = "Asian",
      "B14007E_018E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_018E" = "Other",
      "B14007G_018E" = "Two or More Races",
      "B14007H_018E" = "white",
      "B14007I_018E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
## graphs, counties
## counties
temp_edu_enr_race31 %>% filter(NAME %in% c("Los Angeles County, California", "Cook County, Illinois","Maricopa County, Arizona", "Orange County, California", "Bexar County, Texas",
                                           "Riverside County, California", "San Bernardino County, California", "San Diego County, California")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Education Enrollment Levels in Counties By Race (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))

## acs1 - education enrollment, metro level, (educational enrollment rates of students enrolled in graduate or professional school)
temp_edu_enr_race32 <- acs1_metro_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_018E")) %>% ## this number can change to any enrollment level preferred
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_018E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_018E" = "Black", 
      "B14007C_018E" = "American Indian and Alaska Native",
      "B14007D_018E" = "Asian",
      "B14007E_018E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_018E" = "Other",
      "B14007G_018E" = "Two or More Races",
      "B14007H_018E" = "white",
      "B14007I_018E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, metro: Los Angeles-Long Beach-Anaheim, CA Metro Area; New York-Newark-Jersey City, NY-NJ-PA Metro Area;
# Miami-Fort Lauderdale-West Palm Beach, FL Metro Area; Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area; 
# Providence-Warwick, RI-MA Metro Area
temp_edu_enr_race32 %>% filter(NAME == "Los Angeles-Long Beach-Anaheim, CA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Los Angeles-Long Beach-Anaheim, CA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_la_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_la_race.png", width = 10,   height = 6)

temp_edu_enr_race32 %>% filter(NAME == "New York-Newark-Jersey City, NY-NJ-PA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in New York-Newark-Jersey City, NY-NJ-PA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_ny_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_ny_race.png", width = 10,   height = 6)

temp_edu_enr_race32 %>% filter(NAME == "Miami-Fort Lauderdale-West Palm Beach, FL Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Miami-Fort Lauderdale-West Palm Beach, FL Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_miami_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_miami_race.png", width = 10,   height = 6)

temp_edu_enr_race32 %>% filter(NAME == "Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_dmv_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_dmv_race.png", width = 10,   height = 6)

temp_edu_enr_race32 %>% filter(NAME == "Providence-Warwick, RI-MA Metro Area") %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() +
  ggtitle('Education Enrollment Levels By Race in Providence-Warwick, RI-MA Metro Area (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_enrollment_metro_providence_race.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_enrollment_metro_providence_race.png", width = 10,   height = 6)

## acs1 - education enrollment, state level, (educational enrollment rates of students enrolled in graduate or professional school)
temp_edu_enr_race33 <- acs1_state_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_018E")) %>% ## this number can change to any enrollment level preferred
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_018E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_018E" = "Black", 
      "B14007C_018E" = "American Indian and Alaska Native",
      "B14007D_018E" = "Asian",
      "B14007E_018E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_018E" = "Other",
      "B14007G_018E" = "Two or More Races",
      "B14007H_018E" = "white",
      "B14007I_018E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
# graph, states: CA, TX, FL, NY
temp_edu_enr_race33 %>% filter(NAME %in% c("California", "Texas", "Florida", "New York")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Enrollment Levels in Graduate School By Race in States Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))

## ENROLLMENT IN K-12 SCHOOL ### UNABLE TO GET GRAPHS HERE
B14007I_020E = enrolled in kinder-grade12 
## acs1 - education enrollment, county level, (educational enrollment rates of students enrolled in K-12 school)
temp_edu_enr_race34 <- acs1_county_data %>% 
  select(GEOID, NAME, YEAR, matches("B14007\\w_020E")) %>% ## this number can change to any enrollment level preferred
  pivot_longer(
    cols = -c(GEOID, NAME, YEAR),           
    names_to = 'race',         
    values_to = 'values') %>%
  filter(race != "B14007A_020E") %>%
  mutate(
    values = if_else(is.na(values), 0, values),
    race = recode_factor(
      race,  "B14007B_020E" = "Black", 
      "B14007C_020E" = "American Indian and Alaska Native",
      "B14007D_020E" = "Asian",
      "B14007E_020E" = "Native Hawaiian and Other Pacific Islander",
      "B14007F_020E" = "Other",
      "B14007G_020E" = "Two or More Races",
      "B14007H_020E" = "white",
      "B14007I_020E" = "Latino"
    )) %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(pct = values/sum(values))
## graphs, counties
## counties
temp_edu_enr_race34 %>% filter(NAME %in% c("Los Angeles County, California", "Cook County, Illinois","Maricopa County, Arizona", "Orange County, California", "Bexar County, Texas",
                                           "Riverside County, California", "San Bernardino County, California", "San Diego County, California")) %>%
  ggplot(aes(x = YEAR, y = pct, color = race)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('K-12 School Enrollment Levels in Counties By Race (2012-2020)') + 
  xlab('Year') + ylab('Percent') +
  theme(plot.title = element_text(hjust = 0.5))

## acs1 - education enrollment, metro level, (educational enrollment rates of students enrolled in K-12 school)
## acs1 - education enrollment, state level, (educational enrollment rates of students enrolled in K-12 school)

