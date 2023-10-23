# NCPDP check geocoded data
# MSahu
# Oct 23, 2023

# ------------------------------------------------------------------------------

# SETUP

rm(list=ls())
pacman::p_load(tidyr, dplyr, ggplot2, usmap, sf)

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
data_dir <- paste0(dir, "00_data/processed/NCPDP/")
out_dir <- paste0(data_dir, "geocoded_counties/")

# Read NCPDP data with geocoded addresses
ncpdp <- readRDS(paste0(data_dir, "ncpdp_cleaned_with_coords.rds")) %>% 
  mutate(county_fips = county_fips.x) %>%  select(-county_fips.x, -county_fips.y) %>% 
  select(ncpdp_id, legal_name, ncpdp_name, state_code, county_fips, everything())

# ------------------------------------------------------------------------------

# READ AND FORMAT DATA
 
ncpdp <- ncpdp %>% 
  
  mutate(Coordinates = I(coordinates)) %>% 
  mutate(Coordinates = gsub("c\\(|\\)", "", Coordinates)) %>% 
  separate(Coordinates, into = c("LONG", "LAT"), sep = ", ") 

# ------------------------------------------------------------------------------

# MANUALLY ADD MISSING ADDRESSES (FROM GOOGLE)

ncpdp <- ncpdp %>% 
  
  mutate(
    
    LONG = ifelse(ncpdp_id == "5616721", "41.48831616677966", LONG),
    LAT = ifelse(ncpdp_id == "5616721", "-120.54193061534421", LAT),
    
    LONG = ifelse(ncpdp_id == "5627596", "37.49213241267396", LONG),
    LAT = ifelse(ncpdp_id == "5627596", "-119.97413151826365", LAT),
    
    LONG = ifelse(ncpdp_id == "5631898", "37.48862509412614", LONG),
    LAT = ifelse(ncpdp_id == "5631898", "-119.97206153360783", LAT),
    
    LONG = ifelse(ncpdp_id == "5653161", "37.74894901592902", LONG),
    LAT = ifelse(ncpdp_id == "5653161", "-119.58268174781166", LAT),
    
    LONG = ifelse(ncpdp_id == "5662312", "41.494530602369615", LONG),
    LAT = ifelse(ncpdp_id == "5662312", "-120.55367284312192", LAT)
    
    )

# check <- ncpdp %>% filter(is.na(LAT))

# ------------------------------------------------------------------------------

# Convert to SF

ncpdp_sf <- st_as_sf(ncpdp, 
                     coords = c("LONG", "LAT"), 
                     crs = 4326)  # tells R that these coords should be interpreted as degrees on a sphere (WGS84)

# ------------------------------------------------------------------------------

# Loop through each state and map

pdf(paste0(data_dir, "byState_.pdf"), width = 12, height = 8, onefile = TRUE) %>% 
  
  


# ------------------------------------------------------------------------------


# Loop through each county and map

# clean if point falls outside state

