# NCPDP Data Cleaning for full dataset from 2017 to 2023
# MSahu
# Oct 9, 2023

# Steps:
# 	1. Start with active pharmacies in June 2023
#   2. Add any pharmacies that opened from Jan 1, 2017 to June 2023
#   3. Add closures from Jan 1, 2017 to June 2023

# ------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, readxl)

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/00_data/"
in_dir <- paste0(dir, "raw/NCPDP_Univ of Utah/")
out_dir <- paste0(dir, "processed/NCPDP/")

# ------------------------------------------------------------------------------

# ACTIVE PHARMACIES IN JUNE 2023

# Read the lines from the Provider Detail file
provider_detail_6.1.2 <- paste0(in_dir, "NCPDP_v3.1_Monthly_Master_20230601/mas.txt")
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
active2023 <- data.frame(
  ncpdp_id = trimws(extracted_data[, 1]),
  legal_name = trimws(extracted_data[, 2]),
  ncpdp_name = trimws(extracted_data[, 3]),
  state_code = trimws(extracted_data[, 4]),
  county_fips = trimws(extracted_data[, 5]),
#  closure_date = trimws(extracted_data[, 6]),
  class = trimws(extracted_data[, 7]),
  type1 = trimws(extracted_data[, 8]),
  type2 = trimws(extracted_data[, 9]),
  type3 = trimws(extracted_data[, 10]),
  open24hours = trimws(extracted_data[, 11]))

# Drop the top and bottom rows, which are just the copyright info
active2023 <- active2023[-c(1, nrow(active2023)), ]

# Clear environment
rm(extracted_data)

# ------------------------------------------------------------------------------

# OPENINGS BETWEEN 2017 and 2023

openings17_23 <- read_xlsx(paste0(in_dir, "Open_Pharmarcy_Report_2017-2023.xlsx"), 
                           skip = 4) %>% 
  mutate(open24hours = ifelse(`Sunday 24 Hours` == "Y" & `Monday 24 \r\nHours` == "Y" & `Tuesday 24 \r\nHours` == "Y" & `Wednesday 24 \r\nHours` == "Y" & `Thursday 24 \r\nHours` == "Y" & `Friday 24 Hours` == "Y" & `Saturday 24 \r\nHours` == "Y", 
                              "Y", "N")) %>% 
  select(`Pharmacy NCPDP No`, `Pharmacy Legal Name`, `Pharmacy DBA Name`, 
         `State Physical`, `County Code`,  
         `Dispenser Class\r\nCode`, `Dispenser Type Code`, open24hours,
         `Store Open Date`) %>% 
  rename(ncpdp_id = `Pharmacy NCPDP No`,
         legal_name = `Pharmacy Legal Name`,
         ncpdp_name = `Pharmacy DBA Name`,
         state_code = `State Physical`,
         county_fips = `County Code`,
         class = `Dispenser Class\r\nCode`,
         type1 = `Dispenser Type Code`) %>% 
  mutate(open_date = as.Date(`Store Open Date`, format = "%m/%d/%Y"),
         open_year = as.integer(format(open_date, "%Y"))) %>% 
  select(-`Store Open Date`)

# ------------------------------------------------------------------------------

# CLOSURES OVER TIME, 2017 to 2023

closures17_23 <- read_xlsx(paste0(in_dir, "Closed_Pharmarcy_Report_2017-2023.xlsx"), 
                           skip = 4) %>% 
  mutate(open24hours = ifelse(`Sunday 24 Hours` == "Y" & `Monday 24 \r\nHours` == "Y" & `Tuesday 24 \r\nHours` == "Y" & `Wednesday 24 \r\nHours` == "Y" & `Thursday 24 \r\nHours` == "Y" & `Friday 24 Hours` == "Y" & `Saturday 24 \r\nHours` == "Y", 
                              "Y", "N")) %>% 
  select(`Pharmacy NCPDP No`, `Pharmacy Legal Name`, `Pharmacy DBA Name`, 
         `State Physical`, `County Code`,  
         `Dispenser Class\r\nCode`, `Dispenser Type Code`, open24hours,
         `Store Close Date`) %>% 
  rename(ncpdp_id = `Pharmacy NCPDP No`,
         legal_name = `Pharmacy Legal Name`,
         ncpdp_name = `Pharmacy DBA Name`,
         state_code = `State Physical`,
         county_fips = `County Code`,
         class = `Dispenser Class\r\nCode`,
         type1 = `Dispenser Type Code`) %>% 
  mutate(closure_date = as.Date(`Store Close Date`, format = "%m/%d/%Y"),
         closure_year = as.integer(format(closure_date, "%Y"))) %>% 
  select(-`Store Close Date`)

# ------------------------------------------------------------------------------

# COMBINE AND DROP DUPLICATES

combined_pharmacies <- plyr::rbind.fill(active2023, openings17_23, closures17_23) %>% 
  
  # Order by closure and open date, so these are on top
  
  arrange(desc(closure_date), desc(open_date)) %>% 
  
  # Select only the top row
  
  group_by(ncpdp_id) %>% slice(1) %>% ungroup() 

# If needed, can merge back active2023 type2/type3 info - not included for now

# ------------------------------------------------------------------------------

# RECODE AND CLEAN class and type vars (Tables 7.1 & 7.2, pg. 55)

# Class 

class_levels = c("Independent", "Chain", "Franchise", "Government", "Other") 

combined_pharmacies <- combined_pharmacies %>% mutate(classR = case_when(class == "01" ~ "Independent",
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
  combined_pharmacies[[v]] <- recode_function(combined_pharmacies[[v]])
}

# ------------------------------------------------------------------------------

# Save

saveRDS(combined_pharmacies, paste0(out_dir, "ncpdp_cleaned.rds"))
write.csv(combined_pharmacies, paste0(out_dir, "ncpdp_cleaned.csv"), row.names = F)
