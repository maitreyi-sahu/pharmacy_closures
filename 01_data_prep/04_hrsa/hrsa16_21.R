# HRSA Data Cleaning / Merging
# MSahu
# Dec 6, 2023

# ------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(tidyverse, readxl)

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
in_dir <- paste0(dir, "00_data/raw/HRSA/")
out_dir <- paste0(dir, "00_data/processed/HRSA/")

source(paste0(dir, "01_code/01_data_prep/functions.R"))

# ------------------------------------------------------------------------------

# PHYSICIAN DENSITY
# Main variable of interest is “Total Active M.D.s Non-Federal” from Area Health Resources File
# Details are in the "AHRF 2021-2022 Technical Documentation" Excel file

ahrf_RAW <- read.delim(paste0(in_dir, "ahrf2022.asc"), header = F, row.names = NULL)
ahrf_RAW$V1 <- enc2utf8(ahrf_RAW$V1)

# Define start and end positions for each field, from Technical Docs rows 365-369
start_positions <- c(2, seq(846, 826, by = -5), 65, 19472, 31800, seq(17498, 17466, by = -8), 17450)
end_positions <- c(6, seq(850, 830, by = -5), 66, 19475, 31807, seq(17505, 17473, by = -8), 17457)

# Initialize a list
hrsa_list = list()

# Loop through each pair of start and end positions and extract
for (i in seq_along(start_positions)) {
  extracted_variable <- substr(ahrf_RAW$V1, start_positions[i], end_positions[i])
  hrsa_list[[i]] <- extracted_variable
}

# Convert the list to a data frame
hrsa16_20 <- data.frame(
  
  state_code = hrsa_list[[7]],
  county_fips = hrsa_list[[1]],
  
  tot_mds2016 = as.numeric(hrsa_list[[2]]),
  tot_mds2017 = as.numeric(hrsa_list[[3]]),
  tot_mds2018 = as.numeric(hrsa_list[[4]]),
  tot_mds2019 = as.numeric(hrsa_list[[5]]),
  tot_mds2020 = as.numeric(hrsa_list[[6]]),
  
  pct_urban2010 = as.numeric(hrsa_list[[8]]),
  tot_land_area2020 = as.numeric(hrsa_list[[9]]),
  
  tot_pop2016 = as.numeric(hrsa_list[[10]]),
  tot_pop2017 = as.numeric(hrsa_list[[11]]),
  tot_pop2018 = as.numeric(hrsa_list[[12]]),
  tot_pop2019 = as.numeric(hrsa_list[[13]]),
  tot_pop2020 = as.numeric(hrsa_list[[14]]),
  tot_pop2021 = as.numeric(hrsa_list[[15]]))

rm(ahrf_RAW, hrsa_list, extracted_variable)

# ------------------------------------------------------------------------------

# Convert to long on Year, state_code

hrsa_mds_long <- hrsa16_20 %>%  select(county_fips, starts_with("tot_mds")) %>% 
  pivot_longer(cols = c(starts_with("tot_mds")), names_to = "Year", values_to = "tot_mds") %>% 
  mutate(Year = as.integer(substr(Year, nchar(Year) - 3, nchar(Year)))) %>% 
  select(Year, everything())

hrsa_pop_long <- hrsa16_20 %>%  select(county_fips, starts_with("tot_pop")) %>% 
  pivot_longer(cols = c(starts_with("tot_pop")), names_to = "Year", values_to = "tot_pop") %>% 
  mutate(Year = as.integer(substr(Year, nchar(Year) - 3, nchar(Year)))) %>% 
  select(Year, everything())

hrsa_cleaned <- hrsa16_20 %>% 
  select(state_code, county_fips, pct_urban2010, tot_land_area2020) %>% 
  left_join(hrsa_pop_long, by = "county_fips" ) %>% 
  left_join(hrsa_mds_long, by = c("Year", "county_fips")) %>% 
  select(Year, state_code, county_fips, 
         pct_urban2010, tot_land_area2020, 
         tot_mds) 

# ------------------------------------------------------------------------------

# Save

saveRDS(hrsa_cleaned, paste0(out_dir, "hrsa_cleaned.rds"))
write.csv(hrsa_cleaned, paste0(out_dir, "hrsa_cleaned.csv"), row.names = F)
