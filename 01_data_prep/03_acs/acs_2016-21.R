# ACS census data from 2016 to 2021
# MSahu
# November 27, 2023

# TO DO:
# Extract 2020 data from: https://data.census.gov/table/ACSDP1Y2021.DP03?q=DP03
# Extract SAHIE data 

# Note:
# >75% of counties don't have published ACS data: https://censusreporter.org/topics/health-insurance/ ; 
# To address this, the Census Bureau conducts the Small Areas Health Insurance Estimates (SAHIE) program. SAHIE is the only source for single-year estimates of health insurance coverage for all US counties

# ------------------------------------------------------------------------------

# NOTES from Kelly of what we want to extract:

# Some of these we may already be planning to pull from other markets, but the ones that initially catch my attention include: 
# 1 Unemployment (already have this from BLS)
# 2 Industry 
# 3 Household Income
# 4 Health Insurance Coverage 

# From the project summary:
# 1) Median household income - OK
# 2) urbanicity - NTO FOUND
# 3) health insurance coverage - OK
# 4) poverty status _ OK
# 5) community demographics - race??
# 6) physicians per 1,000 residents - HRSA

#-------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, readxl, tidycensus)

setwd("/")
dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
in_dir <- paste0(dir, "00_data/raw/ACS/")
out_dir <- paste0(dir, "00_data/processed/ACS/")

source(paste0(dir, "01_code/01_data_prep/functions.R"))

# tidycensus package 

census_api_key("8babc646937002e75e6671479bb31cc2541b988a", install = TRUE)

# -----------------------------------------------------------------------------

# Get ACS data

# variables of interest and their names

acs_vars <- c(
  
  "B01003_001", # TOTAL POPULATION
  "B19013_001", # MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2021 INFLATION-ADJUSTED DOLLARS)
  "B01002_001", # MEDIAN AGE BY SEX
  
 # "B27001_001", # HEALTH INSURANCE COVERAGE STATUS BY SEX BY AGE - total
  "B17020_001", # POVERTY STATUS IN THE PAST 12 MONTHS BY AGE - total
  "B99021_001", # ALLOCATION OF RACE - total
  "B99241_001", # ALLOCATION OF INDUSTRY FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER - total 
  
) 

# check availability for all 6 years [NOTE: 2020 DATA NOT AVAILABLE IN TIDYCENSUS -- NEED TO EXTRACT SEPARATELY]

for (m in c(1, 5)) {

  for (y in c(2016, 2017, 2018, 2019, 2021)) {
    
    ACS_vars <- load_variables(y, paste0("acs", m)) %>% filter(name %in% acs_vars)
    
    if (nrow(ACS_vars) < 6) { print(paste0(y, ": missing variable for ", m, "-year ACS")) 
      
      }  else if (nrow(ACS_vars) == 6) { 
      
      print(paste0(y, ": all variables present for ", m, "-year ACS"))
          
    }
  }
}

# all are present!

# GET CODES FOR RACE / INDUSTRY

#codebook
ACS_vars2021 <- load_variables(2021, paste0("acs", 1)) 
write.csv(ACS_vars2021, file = paste0(in_dir, "acs_vars_2021.csv"), row.names = F)

# ------------------------------------------------------------------------------

# The default dataset in get_acs() is the 2015-2019 5-year dataset
# The 1-year dataset is also available for geographies of population 65,000 and greater, so not good for county-level data

acs_1year <- get_acs(
  
  geography = "county",
 # table = "DP03",
 # cache_table = TRUE, # makes this run faster the second time around
  survey = "acs1",
  variables = c(
    
    hh_income_med = "B19001_001",
    poverty_status = "B17020_001",
    health_insurance_pct = "B27001_001",
    industry_alloc = "B99241_001",
    race_alloc = "B99021_001",
    age_med = "B01002_001"
    
    ),
  summary_var = "B01003_001", # Total pop
  output = "wide",
  year = 2021
) # only has data for 841 counties



acs_5year_2021 <- get_acs(
  geography = "county",
  variables = c(
    
    hh_income_med = "B19001_001",
    poverty_status = "B17020_001",
    health_insurance_pct = "B27001_001",
    industry_alloc = "B99241_001",
    race_alloc = "B99021_001",
    age_med = "B01002_001"
    
  ),
  cache_table = TRUE, # makes this run faster the second time around
  output = "wide",
  year = 2021
) # has data for 3221 variables

acs_5year_2020 <- get_acs(
  geography = "county",
  variables = c(
    
    hh_income_med = "B19001_001",
    poverty_status = "B17020_001",
    health_insurance_pct = "B27001_001",
    industry_alloc = "B99241_001",
    race_alloc = "B99021_001",
    age_med = "B01002_001"
    
  ),
  cache_table = TRUE, # makes this run faster the second time around
  output = "wide",
  year = 2020
)

acs_5year_2019 <- get_acs(
  geography = "county",
  variables = c(
    
    hh_income_med = "B19001_001",
    poverty_status = "B17020_001",
    health_insurance_pct = "B27001_001",
    industry_alloc = "B99241_001",
    race_alloc = "B99021_001",
    age_med = "B01002_001"
    
  ),
  cache_table = TRUE, # makes this run faster the second time around
  output = "wide",
  year = 2019
) 
acs_5year_2019 <- acs_5year_2019 %>% arrange(GEOID)

# ------------------------------------------------------------------------------

# For insurance coverage, use SAHIE: https://censusreporter.org/topics/health-insurance/
# Downloaded from https://www.census.gov/data/datasets/time-series/demo/sahie/estimates-acs.html

sahie_dir <- paste0(in_dir, "SAHIE/")

