# NCPDP Check if possible to find parent organization / "system"
# MSahu
# February 13, 2024

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

# Define a function to extract data from a line
extract_data <- function(line) {
  data <- sapply(seq_along(start), function(i) {
    substr(line, start[i], end[i])
  })
  return(data)
}

# Relationship demographic information
relationship_file <- paste0(in_dir, "NCPDP_v3.1_Monthly_Master_20230601/mas_af.txt")
start <- c(1, 19, 4, 792)
end <- c(7, 24, 5, 797)
lines <- readLines(relationship_file, encoding = "latin1")
extracted_data <- t(sapply(lines, extract_data))
relationship2023 <- data.frame(
  ncpdp_id = trimws(extracted_data[, 1]), ## WHAT ARE THESE VALUES?? (PSEUDO??)
  n_records = trimws(extracted_data[, 3]),
  relationship_type = trimws(extracted_data[, 4]),
  parent_id = trimws(extracted_data[, 4])
)


# Ownership
ownership_file <- paste0(in_dir, "NCPDP_v3.1_Monthly_Master_20230601/mas_pr.txt")
start <- c(1, 6)
end <- c(5, 40)
lines <- readLines(relationship_file, encoding = "latin1")
extracted_data <- t(sapply(lines, extract_data))
parent2023 <- data.frame(
  parent_id = trimws(extracted_data[, 1]),
  parent_name = trimws(extracted_data[, 2])
)
