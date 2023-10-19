# BLS Unemployment Data Cleaning for full dataset from 2017 to 2023
# MSahu
# Oct 9, 2023

#-------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, readxl)

setwd("/")
dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/00_data/"
in_dir <- paste0(dir, "raw/BLS_LAU/")
out_dir <- paste0(dir, "processed/BLS/")

# -----------------------------------------------------------------------------

# read BLS unemployment data

bls <- data.frame(
  county_fips = as.character(),
  county_state_name = as.character(),
  year = as.integer(),
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
           year = 4, 
           labor_force = 5, 
           unemployment_pct = 6) %>% 
    filter(!is.na(state_id) & !is.na(county_id)) %>% 
    mutate(county_fips = paste0(state_id, county_id),
           year = as.integer(year)) %>% 
    select(county_fips, county_state_name, year, labor_force, unemployment_pct)
  
  bls <- rbind(bls, temp)
  
}

rm(temp)

# ------------------------------------------------------------------------------

# Save

saveRDS(bls, paste0(out_dir, "bls_cleaned.rds"))
write.csv(bls, paste0(out_dir, "bls_cleaned.csv"), row.names = F)
