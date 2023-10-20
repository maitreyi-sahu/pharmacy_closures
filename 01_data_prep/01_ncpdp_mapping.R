# NCPDP convert address to coordinates
# MSahu
# Oct 19, 2023

# ------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, tidygeocoder)

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
data_dir <- paste0(dir, "00_data/processed/NCPDP/")

ncpdp <- readRDS(paste0(data_dir, "ncpdp_cleaned.rds"))

# ------------------------------------------------------------------------------

# Create single address var

ncpdp <- ncpdp %>% 
  
  mutate(address = paste0(address_street, ", ", address_city, ", ", address_state, ", ", address_zip)) %>% 
  
  select(-c(address_street, address_street2, address_city, address_state, address_zip))

# ------------------------------------------------------------------------------

# Geocode these using the tidygeocoder package

# NEED TO RUN ON CLUSTER

lat_longs <- ncpdp %>%
  
  geocode(address, method = 'osm', lat = latitude , long = longitude)