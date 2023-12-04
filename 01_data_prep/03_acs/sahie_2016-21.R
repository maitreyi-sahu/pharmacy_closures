# ACS census SAHIE data from 2016 to 2021
# MSahu
# Dec 2, 2023

# Downloaded from https://www.census.gov/data/datasets/time-series/demo/sahie/estimates-acs.html

#-------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, readxl, stringr)

setwd("/")
dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
in_dir <- paste0(dir, "00_data/raw/ACS_SAHIE/")
out_dir <- paste0(dir, "00_data/processed/ACS_SAHIE/")

source(paste0(dir, "01_code/01_data_prep/functions.R"))

# ------------------------------------------------------------------------------

# LOAD FILES

sahie <- read.csv(paste0(in_dir, "sahie_2016.csv"), skip = 79)[0,]  

for(y in 2016:2021) {
  
  if (y %in% 2016:2020) {
    temp <- read.csv(paste0(in_dir, "sahie_", y,".csv"), skip = 79) %>% select(-X)
    }

  if (y == 2021) {
    temp <- read.csv(paste0(in_dir, "sahie_", y,".csv"), skip = 83) %>% select(-X)
    }
  
  sahie <- rbind(sahie, temp)
}

rm(temp)

#  -----------------------------------------------------------------------------

# CLEANUP

# filter to counties only (not states)
sahie_cleaned <- sahie %>% filter(geocat == 50) %>%  select(-geocat) %>% 
  
  # Year = year
  mutate(Year = year) %>%  select(-year) %>% 
  
  # county_fips
  mutate(county_fips = paste0(str_pad(as.character(statefips), width = 2, pad = "0"),
                              str_pad(as.character(countyfips), width = 3, pad = "0"))) %>% 
  select(-version, -statefips, -countyfips) %>% 
  
  # state name
  mutate(state_name = str_trim(state_name), county_name = str_trim(county_name)) %>% 
  select(Year, county_fips, state_name, county_name, everything()) %>% 
  
  # convert to numeric
  mutate_at(., vars(8:22), as.numeric)
  
#  -----------------------------------------------------------------------------

# Aggregate

# agecat: 0 - under 65 years
# racecat: 0 - all races
# sexcat: 0 - both sexes
# iprcat: 0 - all income levels

sahie_cleaned <- sahie_cleaned %>% 
  
  filter(agecat == 0,
         racecat == 0,
         sexcat == 0,
         iprcat == 0) %>% 
  
  mutate(pop = NIPR,
         pop_uninsured = NUI,
         pop_insured = NIC,
         pct_uninsured = PCTUI,
         pct_insured = PCTIC) %>% 

  select(Year, county_fips, state_name, county_name, pop, pop_insured, pop_uninsured, pct_insured, pct_uninsured)

#  -----------------------------------------------------------------------------

# SAVE

saveRDS(sahie_cleaned, paste0(out_dir, "sahie_cleaned.rds"))
write.csv(sahie_cleaned, paste0(out_dir, "sahie_cleaned.csv"), row.names = F)