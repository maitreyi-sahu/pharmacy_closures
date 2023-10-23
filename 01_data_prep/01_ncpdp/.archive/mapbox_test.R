# NCPDP convert address to coordinates
# MSahu
# Oct 19, 2023

# ------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, furrr)
library(mapboxapi)
library(httr)

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
data_dir <- paste0(dir, "00_data/processed/NCPDP/")

ncpdp <- readRDS(paste0(data_dir, "ncpdp_cleaned.rds"))

msahu_token <- # UPDATE WITH CORRECT TOKEN
mb_access_token(msahu_token, install = TRUE, overwrite = T)

# ------------------------------------------------------------------------------

# Create single address var

ncpdp <- ncpdp %>% 
  
  mutate(address_zip5 = substr(address_zip, 1, 5)) %>% # get first 5 characters only
  
  mutate(address = paste0(address_street, ", ", address_city, ", ", address_state, ", ", address_zip5)) %>% 
  
  select(-c(address_street, address_street2, address_city, address_state, address_zip, address_zip5)) 

# ------------------------------------------------------------------------------

# Function to get geocode

geocode_mapbox <- function(address, mapbox_token) {
  base_url <- "https://api.mapbox.com/geocoding/v5/mapbox.places/"
  query <- list(
    access_token = mapbox_token,
    types = "address",
    limit = 1
  )
  url <- paste0(base_url, URLencode(address), ".json?")
  
  response <- GET(url, query = query)
  if (http_type(response) == "application/json") {
    data <- content(response, "parsed")
    if (length(data$features) > 0) {
      coords <- data$features[[1]]$geometry$coordinates
      return(coords)
    } else {
      return("Address not found")
    }
  } else {
    return("Failed to retrieve data")
  }
}

# ------------------------------------------------------------------------------

# Unique addresses

address_list <- ncpdp$address %>% as.list()

unique_address_list <- unique(address_list) %>% unlist() # Mapbox has a limit of 100,000 requests .. this gets us to 97,379

unique_address_DF <- data.frame(address = unique_address_list) %>% 
  
  head(5) # test on 5 addresses


# Get addresses

unique_address_DF <- unique_address_DF %>% 
  rowwise() %>% 
  mutate(coordinates = geocode_mapbox(address, msahu_token))