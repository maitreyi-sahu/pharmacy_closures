# NCPDP convert address to coordinates
# MSahu
# Oct 19, 2023

# ------------------------------------------------------------------------------

# Setup

rm(list=ls())
pacman::p_load(dplyr, tidygeocoder)

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
data_dir <- paste0(dir, "00_data/processed/NCPDP/")

ncpdp <- readRDS(paste0(data_dir, "ncpdp_cleaned.rds"))

# ------------------------------------------------------------------------------

# Create single address var


# ------------------------------------------------------------------------------