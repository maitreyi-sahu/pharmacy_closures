# NCPDP Data Cleaning
# MSahu
# Oct 2, 2023

# ------------------------------------------------------------------------------

# Setup

rm(list=ls())
library(dplyr)

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/00_data/"
ncpdp_dir <- paste0(dir, "NCPDP_Univ of Utah/NCPDP_v3.1_Monthly_Master_20230601/")

# ------------------------------------------------------------------------------

# Read the lines from the Provider Detail file
provider_detail_6.1.2 <- paste0(ncpdp_dir, "mas.txt")
lines <- readLines(provider_detail_6.1.2, encoding = "latin1")

# Define the start and end positions for each field, from Table 6.1.2 on pg. 39
start <- c(1, 8, 68, 338, 474, 545, 840, 842, 844, 846)
end <- c(7, 67, 127, 339, 478, 552, 841, 843, 845, 847)

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
  type3 = trimws(extracted_data[, 10]))

# Drop the top and bottom rows, which are just the copyright info
data <- data[-c(1, nrow(data)), ]

# Clear environment
rm(extracted_data)

# ------------------------------------------------------------------------------

# Note: "Closure Date" variable is not available for the full time series 

data %>% filter(closure_date!="00000000") # 371 obs; all are from 2023

# Note from Pg. 74, 8.1.15:
# Provider Information will appear for 45 days after the NCPDP internal deletion date. 
# On day 46, this record would no longer appear on the dataQ standard output file.
# (same for the provider relationship file)


# ------------------------------------------------------------------------------
# Recode class and type vars (Tables 7.1 & 7.2, pg. 55)

data <- data %>% 
  mutate(classR = case_when(class == "01" ~ "Independent Pharmacy"))

# Closures




