# NCPDP Data Cleaning
# MSahu
# Oct 2, 2023

# ------------------------------------------------------------------------------

# Setup

rm(list=ls())
library(dplyr)

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/00_data/"
in_dir <- paste0(dir, "raw/NCPDP_Univ of Utah/NCPDP_v3.1_Monthly_Master_20230601/")
out_dir <- paste0(dir, "processed/NCPDP/")

# ------------------------------------------------------------------------------

# Read the lines from the Provider Detail file
provider_detail_6.1.2 <- paste0(in_dir, "mas.txt")
lines <- readLines(provider_detail_6.1.2, encoding = "latin1")

# Define the start and end positions for each field, from Table 6.1.2 on pg. 39
start <- c(1, 8, 68, 338, 474, 545, 840, 842, 844, 846, 487)
end <- c(7, 67, 127, 339, 478, 552, 841, 843, 845, 847, 487)

# Define a function to extract data from a line
extract_data <- function(line) {
  data <- sapply(seq_along(start), function(i) {
    substr(line, start[i], end[i])
  })
  return(data)
}

# Apply the extract_data function to each line
extracted_data <- t(sapply(lines, extract_data))

# Create a data frame from the extracted data
data <- data.frame(
  ncpdp_id = trimws(extracted_data[, 1]),
  legal_name = trimws(extracted_data[, 2]),
  ncpdp_name = trimws(extracted_data[, 3]),
  state_code = trimws(extracted_data[, 4]),
  county_fips = trimws(extracted_data[, 5]),
  closure_date = trimws(extracted_data[, 6]),
  class = trimws(extracted_data[, 7]),
  type1 = trimws(extracted_data[, 8]),
  type2 = trimws(extracted_data[, 9]),
  type3 = trimws(extracted_data[, 10]),
  open24hours = trimws(extracted_data[, 11]))

# Drop the top and bottom rows, which are just the copyright info
data <- data[-c(1, nrow(data)), ]

# Clear environment
rm(extracted_data)

# ------------------------------------------------------------------------------

# CLOSURES

# Note: "Closure Date" variable is not available for the full time series 

data %>% filter(closure_date != "00000000") # 371 obs; all are from 2023

# Note from Pg. 74, 8.1.15:
# Provider Information will appear for 45 days after the NCPDP internal deletion date. 
# On day 46, this record would no longer appear on the dataQ standard output file.
# (same for the provider relationship file)


# Closure year 

data <- data %>% 
  mutate(closure_year = ifelse(closure_date != "00000000", substr(closure_date, 5, 8), NA))

# ------------------------------------------------------------------------------

# Recode class and type vars (Tables 7.1 & 7.2, pg. 55)

# Class 

class_levels = c("Independent", "Chain", "Franchise", "Government", "Other") 

data <- data %>% mutate(classR = case_when(class == "01" ~ "Independent",
                                           class == "02" ~ "Chain",
                                           class == "05" ~ "Franchise",
                                           class == "06" ~ "Government",
                                           class == "07" ~ "Other")) %>%
  mutate(classR = factor(classR, levels = class_levels))
  
  
  
# Provider Type
  
recode_function <- function(variable) {
  
  variable <- case_when(
    
    variable == "01" ~ "Community/Retail Pharmacy",
    variable == "04" ~ "Long Term Care Pharmacy",
    variable == "05" ~ "Mail Order Pharmacy",
    variable == "06" ~ "Home Infusion Therapy Provider",
    variable == "07" ~ "Non-Pharmacy Dispensing Site",
    variable == "08" ~ "Indian Health Service/Tribal/Urban Indian Health (I/T/U) Pharmacy",
    variable == "09" ~ "Department of Veterans Affairs (VA) Pharmacy",
    variable == "11" ~ "Institutional Pharmacy",
    variable == "12" ~ "Managed Care Organization Pharmacy",
    variable == "13" ~ "DME",
    variable == "14" ~ "Clinic Pharmacy",
    variable == "15" ~ "Specialty Pharmacy",
    variable == "16" ~ "Nuclear Pharmacy",
    variable == "17" ~ "Military/U.S. Coast Guard Pharmacy",
    variable == "18" ~ "Compounding Pharmacy",
    variable == "19" ~ "Oxygen Equipment",
    variable == "20" ~ "Nursing Facility Supplies",
    variable == "21" ~ "Customized Equipment",
    variable == "22" ~ "Dialysis Equipment",
    variable == "23" ~ "Parenteral and Enteral Nutrition") 
  
  return(variable)
}

type_vars <- c("type1", "type2", "type3")

for (v in type_vars) {
  data[[v]] <- recode_function(data[[v]])
}

# ------------------------------------------------------------------------------

# Save

saveRDS(data, paste0(out_dir, "ncpdp_cleaned.rds"))
write.csv(data, paste0(out_dir, "ncpdp_cleaned.csv"), row.names = F)
