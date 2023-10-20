# Merged dataset for regressions
# MSahu

# This dataset is long on pharmacy and year, with the economic indicators merged

# Then we lag the economic indicators

# Then regress the pharmacy closures on economic indicators

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

# Merge data - long on pharmacy and year

merged_df_long <- ncpdp17_21_long %>% 
  
  left_join(bls16_21, by = c("county_fips", "Year", "state_code"))

# ------------------------------------------------------------------------------

# Save

saveRDS(merged_df_long, paste0(data_dir, "merged_data_long.rds"))
write.csv(merged_df_long, paste0(data_dir, "merged_data_long.csv"), row.names = F)