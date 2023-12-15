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

pacman::p_load(dplyr, slider)

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
nolag_covars <- c("pct_urban2010", "tot_land_area2020")
lag_covars <- c("tot_pop", "pop_density", "tot_mds", "mds_per_10k", 
                "labor_force", "unemployment_pct", "employment_pct",
                "hh_income_med", "hh_income_med_2020usd", "poverty_pct",
                "pop_insured", "pop_uninsured", "pct_insured", "pct_uninsured")

# merge 

years <- 2016:2021
startingDF <- expand.grid(ncpdp_id = ncpdp_wide$ncpdp_id, Year = years)

merged_long <- startingDF %>% 
  
  left_join(ncpdp_wide %>% select(id_vars), by = "ncpdp_id") %>% 
  left_join(ncpdp_long, by = c("ncpdp_id", "Year")) %>% 
  left_join(hrsa, by = c("county_fips", "state_code", "Year")) %>% 
  left_join(bls, by = c("county_fips", "state_code", "Year")) %>% 
  left_join(saipe, by = c("county_fips", "state_code","Year")) %>% 
  left_join(sahie, by = c("county_fips", "Year")) %>% 
  
  select(Year, all_of(id_vars), 
         classR, chain_name, open24hours, active17_21, opening17_21, closure17_21, activeJan, opening, closure,
         all_of(nolag_covars), all_of(lag_covars))

rm(startingDF, ncpdp_long, hrsa, bls, saipe, sahie)

# ------------------------------------------------------------------------------
 
# LAG the economic indicators [is there a better way to do this???]

bls16_21 <- bls16_21 %>% mutate(Year = Year + 1)

# ------------------------------------------------------------------------------

# Merge data - long on pharmacy and year

merged_df_long <- ncpdp17_21_long %>% 
  
  left_join(bls16_21, by = c("county_fips", "Year", "state_code"))

# ------------------------------------------------------------------------------

# Save

saveRDS(merged_df_long, paste0(data_dir, "merged_data_long_lagged.rds"))
write.csv(merged_df_long, paste0(data_dir, "merged_data_long_lagged.csv"), row.names = F)