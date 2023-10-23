# NCPDP convert address to coordinates
# MSahu
# Oct 19, 2023

# ------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, furrr, progress)
library(mapboxapi)

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
data_dir <- paste0(dir, "00_data/processed/NCPDP/")
out_dir <- paste0(data_dir, "geocoded_counties/")

ncpdp <- readRDS(paste0(data_dir, "ncpdp_cleaned.rds"))

msahu_token <- "pk.eyJ1IjoibWl0YTkxNDciLCJhIjoiY2xvMDlocG9pMTJrcDJsbGxpYWtyNDUydiJ9.RDhH-obvahJXRIajzMZ9Tg"
mb_access_token(msahu_token, install = TRUE, overwrite = T)

# ------------------------------------------------------------------------------

# Create single address var

ncpdp <- ncpdp %>% 
  
  mutate(address_zip5 = substr(address_zip, 1, 5)) %>% # get first 5 characters only
  
  mutate(address = paste0(address_street, ", ", address_city, ", ", address_state, ", ", address_zip5)) %>% 
  
  select(-c(address_street, address_street2, address_city, address_state, address_zip, address_zip5)) 

# ------------------------------------------------------------------------------

# Function to geocode a single address

geocode_address <- function(address, token) {
  result <- mb_geocode(
    address,
    endpoint = "mapbox.places",
    limit = 1,
    types = NULL,
    search_within = # look only within US; format: c(min_longitude, min_latitude, max_longitude, max_latitude)
      c(-170, # westernmost point in north america 
        20, # southernmost point, Hawaii 
        70, # easternmost point, ~maine  
        50 # northernmost point, alaska
        ),
    language = NULL,
    output = "coordinates",
    access_token = token
  )
  return(result)
}

# ------------------------------------------------------------------------------

# Create list of unique addresses

unique_address_DF <- ncpdp %>% 
  select(county_fips, address) %>% distinct() %>%   # Mapbox has a limit of 100,000 requests .. this gets us to 97,379
  filter(!grepl("^(0[1-6])", county_fips))
  
# Preparation for parallelization

plan(strategy = "multisession", workers = availableCores() - 1)

# Initialize progress bar
pb <- progress_bar$new(
  format = "[:bar] :percent Elapsed: :elapsed ETA: :eta",
  total = length(unique(unique_address_DF$county_fips))
)

# Loop through counties and save out 

for (c in unique(unique_address_DF$county_fips)) {
  
  # Subset to county and geocode
  geocoded_DF <- unique_address_DF %>% filter(county_fips == c) %>% 
    
    # Parallelized geocoding process
    rowwise() %>%
    mutate(coordinates = future_map(
      .x = address,
      .progress = F, 
      .f = geocode_address,
      token = msahu_token
    ))
  
  # Update the progress bar
  pb$tick()
  
  saveRDS(geocoded_DF, paste0(out_dir, "geocoded_", c,".rds"))
  
}

# ------------------------------------------------------------------------------

# LOAD DATA AND COMBINE

# List all RDS files in the directory
county_rds_files <- list.files(path = out_dir, pattern = "\\.rds$", full.names = TRUE)

# Initialize an empty list to store loaded data frames
county_geocodes <- list()

# Load and bind each RDS file
for (file in county_rds_files) {
  loaded_data <- readRDS(file)
  county_geocodes <- c(county_geocodes, list(loaded_data))
}

# Combine all data frames into a single data frame
combined_geocodes <- do.call(rbind, county_geocodes)

# ------------------------------------------------------------------------------

# MERGE BACK ONTO NCPDP

address_df_geo <- ncpdp %>% left_join(combined_geocodes, by = "address")

# ------------------------------------------------------------------------------

# SAVE

saveRDS(address_df_geo, paste0(data_dir, "ncpdp_cleaned_with_coords.rds"))
