# NCPDP convert address to coordinates
# MSahu
# Oct 19, 2023

# This code creates a list of unique addresses and saves it out 

# ------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, tidygeocoder, furrr, mapboxapi)

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
data_dir <- paste0(dir, "00_data/processed/NCPDP/")

ncpdp <- readRDS(paste0(data_dir, "ncpdp_cleaned.rds"))

# ------------------------------------------------------------------------------

# Create single address var

ncpdp <- ncpdp %>% 
  
  mutate(address_zip5 = substr(address_zip, 1, 5)) %>% # get first 5 characters only
  
  mutate(address = paste0(address_street, ", ", address_city, ", ", address_state, ", ", address_zip5)) %>% 
  
  select(-c(address_street, address_street2, address_city, address_state, address_zip, address_zip5)) 

# Create list of unique addresses

address_list <- ncpdp$address %>% as.list()

unique_address_list <- unique(address_list) %>% unlist() # Mapbox has a limit of 100,000 requests .. this gets us to 97,379

unique_address_DF <- data.frame(address = unique_address_list) %>% 
  
  head(20)

# ------------------------------------------------------------------------------


  
# ------------------------------------------------------------------------------

# SAVE

write.csv(unique_address_DF, paste0(data_dir, "ncpdp_unique_addresses.csv"), row.names = F)
saveRDS(unique_address_DF, paste0(data_dir, "ncpdp_unique_addresses.rds"))  
