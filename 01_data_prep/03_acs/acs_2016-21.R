# ACS census data from 2016 to 2021
# MSahu
# December 18, 2023

# Note:
# >75% of counties don't have published 1-year ACS data: https://censusreporter.org/topics/health-insurance/ ; 
# However the 5-year estimates are rolling so can use those 

#-------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, readxl, tidycensus)

setwd("/")
dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
in_dir <- paste0(dir, "00_data/raw/ACS/")
out_dir <- paste0(dir, "00_data/processed/ACS/")

source(paste0(dir, "01_code/01_data_prep/functions.R"))

# tidycensus package 

census_api_key("8babc646937002e75e6671479bb31cc2541b988a", install = T, overwrite = T)

# -----------------------------------------------------------------------------

# Codebook

ACS_vars2021 <- load_variables(2021, paste0("acs", 1)) 
# write.csv(ACS_vars2021, file = paste0(in_dir, "acs_vars_2021.csv"), row.names = F)

industry_labels <- ACS_vars2021 %>% 
  filter(concept == "INDUSTRY BY OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER")

# ------------------------------------------------------------------------------

# Variables of interest and their names

# acs_vars <- c(
#   
#   "B01003_001", # total population
#   "B99241_001", # total civilian employed pop 16+
#   
#   "B24050_009", # transport
#   "B24050_002", # agriculture
#   "B24050_005", # construction
#   "B24050_006", # manufacturing 
#   "B24050_020" # education
# ) 
# 
# # check availability for all 6 years [NOTE: 2020 DATA NOT AVAILABLE IN TIDYCENSUS -- NEED TO EXTRACT SEPARATELY]
# 
# for (m in c(1, 5)) {
# 
#   for (y in c(2016, 2017, 2018, 2019, 2021)) {
#     
#     ACS_vars <- load_variables(y, paste0("acs", m)) %>% filter(name %in% acs_vars)
#     
#     if (nrow(ACS_vars) < 6) { print(paste0(y, ": missing variable for ", m, "-year ACS")) 
#       
#       }  else if (nrow(ACS_vars) == 6) { 
#       
#       print(paste0(y, ": all variables present for ", m, "-year ACS"))
#           
#     }
#   }
# }
# 
# # some are missing for 5-year ACS, but not for 1-year --> use 1 year acs
# 
# # ------------------------------------------------------------------------------
# 
# # CENSUS INDUSTRY VARIABLE - DON'T INCLUDE
# 
# #	Transportation and Warehousing, and Utilities
# #	Agriculture, Forestry, Fishing, and Hunting, and Mining
# #	Construction
# #	Manufacturing
# #	Educational Services, and Health Care and Social Assistance
# 
# industry_vars <- c(
#   
#   tot_pop = "B01003_001",
#   tot_working_pop = "B99241_001",  
#   
#   transport  = "B24050_009",
#   agriculture = "B24050_002",
#   construction = "B24050_005",
#   manufacturing = "B24050_006",
#   education = "B24050_020"
# ) 
# 
# industry_acs1year_2021 <- get_acs(
#   
#   geography = "county",
#   # table = "DP03",
#   # cache_table = TRUE, # makes this run faster the second time around
#   survey = "acs1",
#   variables = c(industry_vars),
#   summary_var = "B01003_001", # Total pop
#   output = "wide",
#   year = 2021
# ) # only has data for 841 counties
# 
# 
# write.csv(industry_acs1year_2021, paste0(out_dir, "industry2021.csv"), row.names = F)

# ------------------------------------------------------------------------------

# The default dataset in get_acs() is the 2015-2019 5-year dataset
# The 1-year dataset is also available for geographies of population 65,000 and greater, so not good for county-level data

extract_pop <- function(year) {
  
  tot_pop <- get_acs(
    
    geography = "county",
    survey = "acs5",
    variables =  c(tot_pop = "B01003_001"),
    cache_table = TRUE, # makes this run faster the second time around
    output = "wide",
    year = year ) %>% 
    
    mutate(Year = year)
  
  return(tot_pop)
  
}

years = 2016:2021
combined_pop <- do.call(rbind, lapply(years, extract_pop)) %>% 
  rename(county_fips = GEOID, tot_pop_acs = tot_popE) %>% 
  select(Year, county_fips, tot_pop_acs)

# ------------------------------------------------------------------------------

# Manually add population for 2 counties which only have data for 2020 and 2021 using the 2020 population

# County_fips = 
# https://data.statesmanjournal.com/census/total-population/total-pochange/chugach-census-area-alaska/050-02063/

cid1 =  "02066"
r10 <- c(Year = 2016, county_fips = cid1, combined_pop[combined_pop$county_fips == cid1 & combined_pop$Year == 2020, "tot_pop_acs"])
r1A <- c(Year = 2017, county_fips = cid1, combined_pop[combined_pop$county_fips == cid1 & combined_pop$Year == 2020, "tot_pop_acs"])
r1B <- c(Year = 2018, county_fips = cid1, combined_pop[combined_pop$county_fips == cid1 & combined_pop$Year == 2020, "tot_pop_acs"])
r1C <- c(Year = 2019, county_fips = cid1, combined_pop[combined_pop$county_fips == cid1 & combined_pop$Year == 2020, "tot_pop_acs"])

cid2 =  "02063"
r20 <- c(Year = 2016, county_fips = cid2, combined_pop[combined_pop$county_fips == cid1 & combined_pop$Year == 2020, "tot_pop_acs"])
r2A <- c(Year = 2017, county_fips = cid2, combined_pop[combined_pop$county_fips == cid1 & combined_pop$Year == 2020, "tot_pop_acs"])
r2B <- c(Year = 2018, county_fips = cid2, combined_pop[combined_pop$county_fips == cid1 & combined_pop$Year == 2020, "tot_pop_acs"])
r2C <- c(Year = 2019, county_fips = cid2, combined_pop[combined_pop$county_fips == cid1 & combined_pop$Year == 2020, "tot_pop_acs"])

combined_pop <- rbind(combined_pop, r10, r1A, r1B, r1C, r20, r2A, r2B, r2C) %>% arrange(Year, county_fips)

# ------------------------------------------------------------------------------

# SAVE

write.csv(combined_pop, paste0(out_dir, "pop_acs16_21_with_dc.csv"), row.names = F)
saveRDS(combined_pop, paste0(out_dir, "pop_acs16_21_with_dc.rds"))
