# BLS Unemployment Data Cleaning for full dataset from 2017 to 2023
# MSahu
# Oct 9, 2023

#-------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, readxl)

setwd("/")
dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
in_dir <- paste0(dir, "00_data/raw/BLS_LAU/")
out_dir <- paste0(dir, "00_data/processed/BLS_LAU/")

source(paste0(dir, "01_code/01_data_prep/functions.R"))

# -----------------------------------------------------------------------------

# read BLS unemployment data

bls <- data.frame(
  county_fips = as.character(),
  county_state_name = as.character(),
  Year = as.integer(),
  labor_force = as.numeric(),
  unemployment_pct = as.numeric()
)

for (i in 16:21) {
  
  temp <- read_xlsx(paste0(in_dir, "laucnty", i, ".xlsx"), skip = 4) %>% 
    
    filter(row_number()!=1) %>% 
    select(2:5, 7, 10) %>% # select only the county / state codes, labor force, and unemployment percentage
    rename(state_id = 1, 
           county_id = 2, 
           county_state_name = 3, 
           Year = 4, 
           labor_force = 5, 
           unemployment_pct = 6) %>% 
    filter(!is.na(state_id) & !is.na(county_id)) %>% 
    mutate(county_fips = paste0(state_id, county_id),
           Year = as.integer(Year),
           state_code = substr(county_state_name, nchar(county_state_name) - 1, nchar(county_state_name))) %>% # last 2 chars
    mutate(state_code = ifelse(state_code == "ia", "IA", state_code)) %>%  # clean one weird one
    select(county_fips, state_code, Year, labor_force, unemployment_pct)
  
  bls <- rbind(bls, temp)
  
}

rm(temp)

# ------------------------------------------------------------------------------

# Filter to only 50 states

bls <- bls %>%  filter(state_code %in% postal_codes_50_states)

# ------------------------------------------------------------------------------

# Save

saveRDS(bls, paste0(out_dir, "bls_cleaned.rds"))
write.csv(bls, paste0(out_dir, "bls_cleaned.csv"), row.names = F)
