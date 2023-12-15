# Merged dataset for regressions with lagged economic variables
# MSahu

# This dataset is long on pharmacy and year, with the economic indicators merged;
# Then we lag the economic indicators by 1 year

# ------------------------------------------------------------------------------

# Setup

rm(list=ls())

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
data_dir <- paste0(dir, "00_data/processed/")

source(paste0(dir, "01_code/01_data_prep/functions.R"))

pacman::p_load(dplyr, slider, data.table)

# ------------------------------------------------------------------------------

# Load data

ncpdp_wide <- readRDS(paste0(data_dir, "NCPDP/ncpdp_cleaned.rds"))
ncpdp_long <- readRDS(paste0(data_dir, "NCPDP/ncpdp_cleaned_long.rds"))
hrsa <- readRDS(paste0(data_dir, "HRSA/hrsa_cleaned.rds"))
saipe <- readRDS(paste0(data_dir, "ACS_SAIPE/saipe_cleaned.rds"))
sahie <- readRDS(paste0(data_dir, "ACS_SAHIE/sahie_cleaned.rds"))
bls <- readRDS(paste0(data_dir, "BLS_LAU/bls_cleaned.rds")) %>% 
  mutate(employment_pct = 100 - unemployment_pct)

# variable list

id_vars <- c("ncpdp_id", "legal_name", "ncpdp_name", "state_code", "county_fips")
ncpdp_vars <- c("activeJan", "opening", "closure")
nolag_covars <- c("pct_urban2010", "tot_land_area2020")
lag_covars <- c("tot_pop", "pop_density", "tot_mds", "mds_per_10k", 
                "labor_force", "unemployment_pct", "employment_pct",
                "hh_income_med", "hh_income_med_2020usd", "poverty_pct",
                "pop_insured", "pop_uninsured", "pct_insured", "pct_uninsured")

# merge 

years <- 2016:2021
startingDF <- expand.grid(ncpdp_id = ncpdp_wide$ncpdp_id, Year = years)

merged_long <- startingDF %>% 
  
  left_join(ncpdp_wide %>% select(id_vars, classR, chain_name, open24hours, active17_21, opening17_21, closure17_21), by = "ncpdp_id") %>% 
  left_join(ncpdp_long %>% select(ncpdp_id, Year, ncpdp_vars), by = c("ncpdp_id", "Year")) %>% 
  left_join(hrsa, by = c("county_fips", "state_code", "Year")) %>% 
  left_join(bls, by = c("county_fips", "state_code", "Year")) %>% 
  left_join(saipe, by = c("county_fips", "state_code","Year")) %>% 
  left_join(sahie, by = c("county_fips", "Year")) %>% 
  
  select(Year, all_of(id_vars), 
         classR, chain_name, open24hours, 
         active17_21, opening17_21, closure17_21, all_of(ncpdp_vars), 
         all_of(nolag_covars), all_of(lag_covars)) %>% 
  
  setDT()

#rm(startingDF, ncpdp_long, ncpdp_wide, hrsa, bls, saipe, sahie)

# ------------------------------------------------------------------------------

# Aggregate at county level 

# NEED TO DISAGGREGATE BY CHAIN V INDEPENDENT?

ids <- merged_long[, c("state_code", "county_fips")] %>% distinct()

merged_county <- 
  merged_long[, lapply(.SD, sum), by = .(Year, county_fips), .SDcols = c("active17_21", "opening17_21", "closure17_21", ncpdp_vars)][
  merged_long[, lapply(.SD, mean), by = .(Year, county_fips), .SDcols = nolag_covars], on = .(Year, county_fips)][
  merged_long[, lapply(.SD, mean), by = .(Year, county_fips), .SDcols = lag_covars], on = .(Year, county_fips)]

merged_county <- merged_county %>% left_join(ids) %>% select(Year, state_code, county_fips, everything())
  
# Lag covariates

merged_county_lagged <- merged_county %>%
  group_by(state_code, county_fips) %>%
  arrange(Year) %>%
  mutate(across(all_of(lag_covars), ~lag(.), .names = "{.col}_lag1")) %>%
  ungroup() %>% 
  
  # Restrict to 2016
  filter(Year %in% 2017:2021)

# ------------------------------------------------------------------------------

# Add outcome vars

merged_county_lagged <- merged_county_lagged %>% 
  
  # pharmacies per cap
  mutate(pharm_per_100k = activeJan/tot_pop * 100000,
         national_median_pharm_per100k_17 = median(pharm_per_100k[Year == 2017], na.rm = T)) %>% 
  
  # raw and percent change from 2017 to 2021
  group_by(state_code, county_fips) %>%
  mutate(
    pharm_chg17_21 = (activeJan[Year == 2021] - activeJan[Year == 2017]),
    pharm_chg_pct17_21 = (activeJan[Year == 2021] - activeJan[Year == 2017]) / activeJan[Year == 2017] * 100,
    primary_outcome = ifelse(pharm_chg_pct17_21 < -10 & activeJan < national_median_pharm_per100k_17, 1, 0)) %>% 
  
  # select vars
  
  select(Year, state_code, county_fips, 
         primary_outcome, pharm_per_100k, national_median_pharm_per100k_17, pharm_chg17_21, pharm_chg_pct17_21,  active17_21, opening17_21, closure17_21, all_of(ncpdp_vars), 
         everything()) 


# ------------------------------------------------------------------------------

# Save

write.csv(merged_long, paste0(data_dir, "merged_data_long.csv"), row.names = F)
write.csv(merged_county_lagged, paste0(data_dir, "merged_data_county.csv"), row.names = F)
