# ACS census SAIPE data from 2016 to 2021
# MSahu
# Dec 6, 2023

# Downloaded from : https://www.census.gov/programs-surveys/saipe/data/datasets.html

#-------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, readxl, stringr, lubridate)

setwd("/")
dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
in_dir <- paste0(dir, "00_data/raw/ACS_SAIPE/")
out_dir <- paste0(dir, "00_data/processed/ACS_SAIPE/")

source(paste0(dir, "01_code/01_data_prep/functions.R"))

# ------------------------------------------------------------------------------

# READ AND BIND FILES

# Function to read and process each file
read_saipe <- function(year) {

  data <- read_excel(paste0(in_dir, "est", year, "all.xls"), 
                    # col_types = "numeric", 
                     skip = 3) %>% 
    mutate(Year = as.integer( paste0("20", year))) %>% 
    select(Year, everything())
  
  data[, 6:length(data)] <- lapply(data[, 6:length(data)], as.numeric)
  
  return(data)
}

# Apply function across all years and bind
years = 16:21
saipe_list <- lapply(years, read_saipe)
saipe16_21 <- bind_rows(saipe_list)

# Clear environment
rm(saipe_list)

#  -----------------------------------------------------------------------------

# Variables
id_vars  <- c("Year", "State FIPS Code", "County FIPS Code", "Postal Code", "Name")
data_vars <- c("Poverty Percent, All Ages", "Median Household Income")

# CLEANUP
saipe_cleaned <- saipe16_21 %>% 
  
  # Filter to counties only (not US/states)
  filter(`County FIPS Code`!= "000") %>%  
  
  # Select variables of interest
  select(all_of(c(id_vars, data_vars))) %>% 
  
  # county_fips
  mutate(county_fips = paste0(`State FIPS Code`, `County FIPS Code`)) %>% 
  
  # rename vars
  rename(State = "Postal Code",
         poverty_pct = "Poverty Percent, All Ages",
         hh_income_med = "Median Household Income") %>% 
  
  # select vars
  select(Year, State, Name, county_fips, poverty_pct, hh_income_med)
  
#  -----------------------------------------------------------------------------

# INFLATE: https://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package

# Use the CPI data from FRED
# "CPIAUCSL" =  Consumer Price Index for All Urban Consumers: All Items

monthly_cpi <-
  read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
             skip = 54, header = TRUE, row.names = NULL)
monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% summarize(cpi = mean(VALUE)) 
yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2020]
yearly_cpi <- yearly_cpi %>% mutate(Year = cpi_year) %>% select(Year, adj_factor)

# merge and inflate

saipe_cleaned <- saipe_cleaned %>% 
  
  left_join(yearly_cpi, by = "Year") %>% 
  
  mutate(hh_income_med_2020 = hh_income_med / adj_factor)

#  -----------------------------------------------------------------------------

# SAVE

saveRDS(saipe_cleaned, paste0(out_dir, "saipe_cleaned.rds"))
write.csv(saipe_cleaned, paste0(out_dir, "saipe_cleaned.csv"), row.names = F)