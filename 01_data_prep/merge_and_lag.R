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

ncpdp_dir <- paste0(data_dir, "NCPDP/")
ncpdp17_21_long <- readRDS(paste0(ncpdp_dir, "ncpdp_cleaned_long.rds"))   

bls_dir <- paste0(data_dir, "BLS_LAU/")
bls16_21 <- readRDS(paste0(bls_dir, "bls_cleaned.rds"))

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