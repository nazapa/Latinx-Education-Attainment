## Competency Product: Latinx educational attainment levels

## Latinx educational attainment levels

## libraries
library(tidyverse)
library(tidycensus)
library(labelled)

############ ACS 5-year County Level Data 2012-2020 ############ 
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

## County level 2020 data (keep margin of error and estimates)
acs5_county_data_2020 <- get_acs(
  geography = 'county',  
  variables = var_list, 
  year = 2020,
  output = 'wide'
)
acs5_county_data_2020

## County level 2019 data (keep margin of error and estimates)
acs5_county_data_2019 <- get_acs(
  geography = 'county',  
  variables = var_list, 
  year = 2019,
  output = 'wide'
)
acs5_county_data_2019

## County level 2018 data (keep margin of error and estimates)
acs5_county_data_2018 <- get_acs(
  geography = 'county',  
  variables = var_list, 
  year = 2018,
  output = 'wide'
)
acs5_county_data_2018

## County level 2017 data (keep margin of error and estimates)
acs5_county_data_2017 <- get_acs(
  geography = 'county',  
  variables = var_list, 
  year = 2017,
  output = 'wide'
)
acs5_county_data_2017

## County level 2016 data (keep margin of error and estimates)
acs5_county_data_2016 <- get_acs(
  geography = 'county',  
  variables = var_list, 
  year = 2016,
  output = 'wide'
)
acs5_county_data_2016

## County level 2015 data (keep margin of error and estimates)
acs5_county_data_2015 <- get_acs(
  geography = 'county',  
  variables = var_list, 
  year = 2015,
  output = 'wide'
)
acs5_county_data_2015

## County level 2014 data (keep margin of error and estimates)
acs5_county_data_2014 <- get_acs(
  geography = 'county',  
  variables = var_list, 
  year = 2014,
  output = 'wide'
)
acs5_county_data_2014

## County level 2013 data (keep margin of error and estimates)
acs5_county_data_2013 <- get_acs(
  geography = 'county',  
  variables = var_list, 
  year = 2013,
  output = 'wide'
)
acs5_county_data_2013

## County level 2012 data (keep margin of error and estimates)
acs5_county_data_2012 <- get_acs(
  geography = 'county',  
  variables = var_list, 
  year = 2012,
  output = 'wide'
)
acs5_county_data_2012

############ ACS 5-year Metropolitan Data 2012-2020 ############

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

## Metropolitan Data 2014
acs5_metro_2014 <- get_acs(
  geography = 'metropolitan statistical area/micropolitan statistical area',  
  variables = var_list, 
  year = 2014,
  output = 'wide'
)
## rename column names
var_labels_metro_2014 <- Vectorize(function(var_name) {
  print(var_name)
  if (var_name == "GEOID" || var_name == "NAME"){
    return(var_name)
  }
  print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
  return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
}) 

acs5_metro_2014 <- acs5_metro_2014 %>% rename_with(var_labels_metro_2014)

## Metropolitan Data 2013
acs5_metro_2013 <- get_acs(
  geography = 'metropolitan statistical area/micropolitan statistical area',  
  variables = var_list, 
  year = 2013,
  output = 'wide'
)
## rename column names
var_labels_metro_2013 <- Vectorize(function(var_name) {
  print(var_name)
  if (var_name == "GEOID" || var_name == "NAME"){
    return(var_name)
  }
  print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
  return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
}) 

acs5_metro_2013 <- acs5_metro_2013 %>% rename_with(var_labels_metro_2013)

## Metropolitan Data 2012
acs5_metro_2012 <- get_acs(
  geography = 'metropolitan statistical area/micropolitan statistical area',  
  variables = var_list, 
  year = 2012,
  output = 'wide'
)
## rename column names
var_labels_metro_2012 <- Vectorize(function(var_name) {
  print(var_name)
  if (var_name == "GEOID" || var_name == "NAME"){
    return(var_name)
  }
  print((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label)
  return(str_c((filter(acs5_vars, name == str_sub(var_name, start = 1L, end = -2)))$label, var_name))
}) 

acs5_metro_2012 <- acs5_metro_2012 %>% rename_with(var_labels_metro_2012)

############ ACS 5-year State Level Data 2012-2020 ############

## State Data 2020
acs5_state_data_2020 <- get_acs(
  geography = 'state',  
  variables = var_list, 
  year = 2020,
  output = 'wide'
)
acs5_state_data_2020

## State Data 2019
acs5_state_data_2019 <- get_acs(
  geography = 'state',  
  variables = var_list, 
  year = 2019,
  output = 'wide'
)
acs5_state_data_2019

## State Data 2018
acs5_state_data_2018 <- get_acs(
  geography = 'state',  
  variables = var_list, 
  year = 2018,
  output = 'wide'
)
acs5_state_data_2018

## State Data 2017
acs5_state_data_2017 <- get_acs(
  geography = 'state',  
  variables = var_list, 
  year = 2017,
  output = 'wide'
)
acs5_state_data_2017

## State Data 2016
acs5_state_data_2016 <- get_acs(
  geography = 'state',  
  variables = var_list, 
  year = 2016,
  output = 'wide'
)
acs5_state_data_2016

## State Data 2015
acs5_state_data_2015 <- get_acs(
  geography = 'state',  
  variables = var_list, 
  year = 2015,
  output = 'wide'
)
acs5_state_data_2015

## State Data 2014
acs5_state_data_2014 <- get_acs(
  geography = 'state',  
  variables = var_list, 
  year = 2014,
  output = 'wide'
)
acs5_state_data_2014

## State Data 2013
acs5_state_data_2013 <- get_acs(
  geography = 'state',  
  variables = var_list, 
  year = 2013,
  output = 'wide'
)
acs5_state_data_2013

## State Data 2012
acs5_state_data_2012 <- get_acs(
  geography = 'state',  
  variables = var_list, 
  year = 2012,
  output = 'wide'
)
acs5_state_data_2012










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
# graph: CA, TX, FL, NY
temp_edu_attain3 %>% filter(NAME %in% c("California", "Texas", "Florida", "New York")) %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Latinx Education Attainment Levels in States Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs1_eduattain_states.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs1_eduattain_states.png", width = 10,   height = 6)

## acs5 - education attainment, state level
temp_edu_attain4 <- acs5_state_data %>% 
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
# graph: CA, TX, FL, NY
temp_edu_attain4 %>% filter(NAME %in% c("California", "Texas", "Florida", "New York")) %>%
  ggplot(aes(x = YEAR, y = pct, color = education_level, linetype = gender)) + geom_line() + facet_wrap(~ NAME) +
  ggtitle('Latinx Education Attainment Levels in States Most Populated By Latinx (2012-2020)') + 
  xlab('Year') + ylab('Percent of Latinx') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(path = "graphs","acs5_eduattain_states.pdf", width = 10,   height = 6)
ggsave(path = "graphs","acs5_eduattain_states.png", width = 10,   height = 6)

