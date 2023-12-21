# NCPDP Data Cleaning for full dataset from 2017 to 2023
# MSahu
# Oct 9, 2023

# Steps:
# 	1. Start with active pharmacies in June 2023
#   2. Add any pharmacies that opened from Jan 1, 2017 to June 2023
#   3. Load closures from Jan 1, 2017 to June 2023
#   4. Combine
#   5. Clean/recode variables
#   6. Denominators for each year - active pharmacies on Jan 1 of that year

# ------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, readxl)

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
in_dir <- paste0(dir, "00_data/raw/NCPDP_Univ of Utah/")
out_dir <- paste0(dir, "00_data/processed/NCPDP/")

source(paste0(dir, "01_code/01_data_prep/functions.R"))

# ------------------------------------------------------------------------------

# ACTIVE PHARMACIES IN JUNE 2023

# Read the lines from the Provider Detail file
provider_detail_6.1.2 <- paste0(in_dir, "NCPDP_v3.1_Monthly_Master_20230601/mas.txt")
lines <- readLines(provider_detail_6.1.2, encoding = "latin1")

# Define the start and end positions for each field, from Table 6.1.2 on pg. 39
start <- c(1, 8, 68, 338, 474, 545, 840, 842, 844, 846, 487, 198, 253, 308, 340)
end <- c(7, 67, 127, 339, 478, 552, 841, 843, 845, 847, 487, 252, 307, 337, 348)

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
  open24hours = trimws(extracted_data[, 11]),
  
  address_street = trimws(extracted_data[, 12]),
  address_street2 = trimws(extracted_data[, 13]),
  address_city = trimws(extracted_data[, 14]),
  address_state = trimws(extracted_data[, 4]),
  address_zip = trimws(extracted_data[, 15])
  
)

# Drop the top and bottom rows, which are just the copyright info
active2023 <- active2023[-c(1, nrow(active2023)), ]

# Clear environment
rm(extracted_data)

# ------------------------------------------------------------------------------

# OPENINGS BETWEEN 2017 and 2023

openings17_23 <- read_xlsx(paste0(in_dir, "Open_Pharmacy_Report_2017-2023.xlsx"), 
                           skip = 4) %>% 
  
  mutate(open24hours = ifelse(`Sunday 24 Hours` == "Y" & `Monday 24 \r\nHours` == "Y" & `Tuesday 24 \r\nHours` == "Y" & `Wednesday 24 \r\nHours` == "Y" & `Thursday 24 \r\nHours` == "Y" & `Friday 24 Hours` == "Y" & `Saturday 24 \r\nHours` == "Y", 
                              "Y", "N")) %>% 
  
  select(`Pharmacy NCPDP No`, `Pharmacy Legal Name`, `Pharmacy DBA Name`, 
         `State Physical`, `County Code`,  
         `Dispenser Class\r\nCode`, `Dispenser Type Code`, open24hours,
         `Store Open Date`, 
         `Address1 - Physical`, `Address2 - Physical\r\n`, `City Physical`, `State Physical`, `Zip Code \r\nas reported`) %>% 
  
  rename(ncpdp_id = `Pharmacy NCPDP No`,
         legal_name = `Pharmacy Legal Name`,
         ncpdp_name = `Pharmacy DBA Name`,
         state_code = `State Physical`,
         county_fips = `County Code`,
         class = `Dispenser Class\r\nCode`,
         type1 = `Dispenser Type Code`,
         address_street = `Address1 - Physical`, 
         address_street2 = `Address2 - Physical\r\n`,
         address_city = `City Physical`,
         address_zip = `Zip Code \r\nas reported`
         ) %>% 
  
  mutate(address_state = state_code,
         open_date = as.Date(`Store Open Date`, format = "%m/%d/%Y"),
         open_year = as.integer(format(open_date, "%Y"))) %>% 
  
  select(-`Store Open Date`)

# ------------------------------------------------------------------------------

# CLOSURES OVER TIME, 2017 to 2023

closures17_23 <- read_xlsx(paste0(in_dir, "Closed_Pharmacy_Report_2017-2023.xlsx"), 
                           skip = 4) %>% 
  
  mutate(open24hours = ifelse(`Sunday 24 Hours` == "Y" & `Monday 24 \r\nHours` == "Y" & `Tuesday 24 \r\nHours` == "Y" & `Wednesday 24 \r\nHours` == "Y" & `Thursday 24 \r\nHours` == "Y" & `Friday 24 Hours` == "Y" & `Saturday 24 \r\nHours` == "Y", 
                              "Y", "N")) %>% 
  
  select(`Pharmacy NCPDP No`, `Pharmacy Legal Name`, `Pharmacy DBA Name`, 
         `State Physical`, `County Code`,  
         `Dispenser Class\r\nCode`, `Dispenser Type Code`, open24hours,
         `Address1 - Physical`, `Address2 - Physical\r\n`, `City Physical`, `State Physical`, `Zip Code \r\nas reported`,
         `Store Close Date`) %>% 
  
  rename(ncpdp_id = `Pharmacy NCPDP No`,
         legal_name = `Pharmacy Legal Name`,
         ncpdp_name = `Pharmacy DBA Name`,
         state_code = `State Physical`,
         county_fips = `County Code`,
         class = `Dispenser Class\r\nCode`,
         type1 = `Dispenser Type Code`, 
         address_street = `Address1 - Physical`, 
         address_street2 = `Address2 - Physical\r\n`,
         address_city = `City Physical`,
         address_zip = `Zip Code \r\nas reported`
  ) %>% 
  
  mutate(address_state = state_code,
         closure_date = as.Date(`Store Close Date`, format = "%m/%d/%Y"),
         closure_year = as.integer(format(closure_date, "%Y"))) %>% 
  
  select(-`Store Close Date`)

# ------------------------------------------------------------------------------

# COMBINE AND DROP DUPLICATES

combined_pharmacies <- plyr::rbind.fill(active2023, openings17_23, closures17_23) %>% 
  
  # Order by closure and open date, so these are on top
  
  arrange(desc(closure_date), desc(open_date))

# CHECK that there's no pharmacies that both opened and closed in this period - true
nrow(combined_pharmacies %>% filter(!is.na(open_year) & !is.na(closure_year)))
  
  # Select only the top row. 
  # NOTE this code only works because there are no 
  
combined_pharmacies <- combined_pharmacies %>% group_by(ncpdp_id) %>% slice(1) %>% ungroup() 

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

# Chain Names

combined_pharmacies <- combined_pharmacies %>% 
  mutate(chain_name = ifelse(classR == "Chain", 
                               case_when(
                                 grepl("^CVS", legal_name) ~ "CVS",
                                 grepl("^Walgreen", legal_name) ~ "Walgreens",
                                 grepl("^WALGREEN", legal_name) ~ "Walgreens",
                                 grepl("^Cigna", legal_name) ~ "Cigna",
                                 grepl("^WAL-MART", legal_name) ~ "Walmart",
                                 grepl("^WALMART", legal_name) ~ "Walmart",
                                 grepl("^RITE AID", legal_name) ~ "Rite Aid",
                                 grepl("^PUBLIX", legal_name) ~ "Publix",
                                 grepl("^KROGER", legal_name) ~ "Kroger",
                                 # grepl("^COSTCO", legal_name) ~ "Costco", 
                                 grepl("^ALBERTSONS", legal_name) ~ "Albertsons", 
                                 grepl("^SAFEWAY", legal_name) ~ "Safeway",  
                                 grepl("^THRIFTY", legal_name) ~ "Thrifty PayLess", 
                                 grepl("^ECKERD", legal_name) ~ "Eckerd", 
                                 grepl("^K MART", legal_name) ~ "Kmart", 
                                 T ~ "Other"), 
                             NA))

# ------------------------------------------------------------------------------

# Find active pharmacies for Jan 1 of each year, 2017-2022

combined_pharmacies <- combined_pharmacies %>% 
  
        # Active pharmacies for that year
  
  mutate(activeJan2017 = ifelse(is.na(open_year) | open_year < 2017, 1, 0),
         activeJan2018 = ifelse((is.na(open_year) | open_year < 2018) & !closure_year %in% 2017, 1, 0),
         activeJan2019 = ifelse((is.na(open_year) | open_year < 2019) & !closure_year %in% 2017:2018, 1, 0),
         activeJan2020 = ifelse((is.na(open_year) | open_year < 2020) & !closure_year %in% 2017:2019, 1, 0),
         activeJan2021 = ifelse((is.na(open_year) | open_year < 2021) & !closure_year %in% 2017:2020, 1, 0),
         
         active17_21 = ifelse(activeJan2017 ==1 | activeJan2018 == 1 | activeJan2019 == 1 | activeJan2020 == 1 | activeJan2021 ==1, 1, 0),
  
         # Openings for each year and the full time period
         
         opening = ifelse(is.na(open_date), 0, 1), # if date is NA, did not open during 2017-23
         opening17_21 = ifelse(!is.na(open_date) & open_year %in% 2017:2021, 1, 0),

         
         opening2017 = ifelse(open_year == 2017, 1, 0),
         
         opening2018 = ifelse(open_year == 2018, 1, 0),
         
         opening2019 = ifelse(open_year == 2019, 1, 0),
         
         opening2020 = ifelse(open_year == 2020, 1, 0),
         
         opening2021 = ifelse(open_year == 2021, 1, 0),
                  
          # Closures for each year and the full time period
         
         closure = ifelse(is.na(closure_date), 0, 1), # if date is NA, did not close during 2017-23
         closure17_21 = ifelse(!is.na(closure_date) & closure_year %in% 2017:2021, 1, 0),
         
         # Closures for each year, coded as follows:
         #        1 if it closed that year
         #        0 if it was active in jan and did not close that year
         #        NA if it was not active that year
         
         closure2017 = case_when(closure_year == 2017 ~ 1,
                               activeJan2017 == 1 & ( is.na(closure_date) | closure_year != 2017) ~ 0,
                               activeJan2017 == 0 ~ NA),
         
         closure2018 = case_when(closure_year == 2018 ~ 1,
                               activeJan2018 == 1 & ( is.na(closure_date) | closure_year != 2018) ~ 0,
                               activeJan2018 == 0 ~ NA),
 
         closure2019 = case_when(closure_year == 2019 ~ 1,
                               activeJan2019 == 1 & ( is.na(closure_date) | closure_year != 2019) ~ 0,
                               activeJan2019 == 0 ~ NA),    
         
         closure2020 = case_when(closure_year == 2020 ~ 1,
                               activeJan2020 == 1 & ( is.na(closure_date) | closure_year != 2020) ~ 0,
                               activeJan2020 == 0 ~ NA),
         
         closure2021 = case_when(closure_year == 2021 ~ 1,
                               activeJan2021 == 1 & ( is.na(closure_date) | closure_year != 2021) ~ 0,
                               activeJan2021 == 0 ~ NA))
         
# ------------------------------------------------------------------------------

# Filter to only 50 states

combined_pharmacies <- combined_pharmacies %>%  filter(state_code %in% postal_codes_51_states)

# ------------------------------------------------------------------------------

# Add missing fips codes 

combined_pharmacies <- combined_pharmacies %>% 
  
  mutate(county_fips = ifelse(ncpdp_id == "3732105", "40027", county_fips)) %>% 
  mutate(county_fips = ifelse(ncpdp_id == "5939597", "48201", county_fips))

# ------------------------------------------------------------------------------

# Create long version

active_long <- combined_pharmacies %>% 
  select(ncpdp_id, county_fips, 
         activeJan2017, activeJan2018, activeJan2019, activeJan2020, activeJan2021) %>% 
  tidyr::pivot_longer(cols = starts_with("active"),
                      names_to = "Year",
                      values_to = "activeJan",
                      names_prefix = "activeJan") %>% 
  
  mutate(Year = as.integer(Year))

openings_long <- combined_pharmacies %>% 
  select(ncpdp_id, county_fips, 
         opening2017, opening2018, opening2019, opening2020, opening2021) %>% 
  tidyr::pivot_longer(cols = starts_with("opening"),
                      names_to = "Year",
                      values_to = "opening",
                      names_prefix = "opening") %>% 
  mutate(Year = as.integer(Year))

closures_long <- combined_pharmacies %>% 
  select(ncpdp_id, county_fips, 
         closure2017, closure2018, closure2019, closure2020, closure2021) %>% 
  tidyr::pivot_longer(cols = starts_with("closure"),
                      names_to = "Year",
                      values_to = "closure",
                      names_prefix = "closure") %>% 
  mutate(Year = as.integer(Year))

# merge

years <- 2017:2021
startingDF <- expand.grid(ncpdp_id = unique(combined_pharmacies$ncpdp_id), Year = years) 

combined_pharmacies_long <- startingDF %>% 
  
  left_join(combined_pharmacies %>%  
              select(ncpdp_id, legal_name, ncpdp_name, state_code, county_fips,
                     classR, chain_name, open24hours,
                     active17_21, opening17_21, closure17_21), by = "ncpdp_id") %>% 
  
  left_join(active_long, by = c("ncpdp_id", "county_fips", "Year")) %>% 
  
  left_join(openings_long, by = c("ncpdp_id", "county_fips", "Year")) %>% 
  
  left_join(closures_long, by = c("ncpdp_id", "county_fips", "Year"))

# ------------------------------------------------------------------------------

# Save

saveRDS(combined_pharmacies, paste0(out_dir, "ncpdp_cleaned_with_dc.rds"))
write.csv(combined_pharmacies, paste0(out_dir, "ncpdp_cleaned_with_dc.csv"), row.names = F)

saveRDS(combined_pharmacies_long, paste0(out_dir, "ncpdp_cleaned_long_with_dc.rds"))
write.csv(combined_pharmacies_long, paste0(out_dir, "ncpdp_cleaned_long_with_dc.csv"), row.names = F)