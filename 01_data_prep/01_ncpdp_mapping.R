# NCPDP convert address to coordinates
# MSahu
# Oct 19, 2023

## Expected time to run: 43 hours!!

# ------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, tidygeocoder, furrr)

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
data_dir <- paste0(dir, "00_data/processed/NCPDP/")

ncpdp <- readRDS(paste0(data_dir, "ncpdp_cleaned.rds"))

# ------------------------------------------------------------------------------

# Create single address var

ncpdp <- ncpdp %>% 
  
  mutate(address_zip5 = substr(address_zip, 1, 5)) %>% # get first 5 characters only
  
  mutate(address = paste0(address_street, ", ", address_city, ", ", address_state, ", ", address_zip5)) %>% 
  
  select(-c(address_street, address_street2, address_city, address_state, address_zip, address_zip5)) %>% 

# The iteration in furrr requires a list 

address_list <- ncpdp$address %>% as.list()

# Preparation for parallelization

plan(strategy = "multisession", workers = availableCores() - 1)

# Parallel iteration using tidygeocoder package

address_geodata <- future_map(.x = address_list, 
                              ~ geo(address = .x, method = 'osm', lat = latitude , long = longitude)) %>% 
  bind_rows()   # puts the lists back together into a dataframe/tibble

# Join original data and geo data

address_df_geo <- ncpdp %>% 
  
  left_join(address_geodata, by = "address")

# ------------------------------------------------------------------------------

# SAVE

write.csv(address_df_geo, paste0(data_dir, "ncpdp_cleaned_with_address.csv"), row.names = F)
saveRDS(address_df_geo, paste0(dara_dir, "ncpdp_cleaned_with_address.rds"))
