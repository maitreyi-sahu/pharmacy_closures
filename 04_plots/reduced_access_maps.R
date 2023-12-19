# MSahu
# Dec 19, 2023

# State and county maps of reduced access to pharmacies, using data prepped
# by Kelly

# ==============================================================================

# Setup
rm(list=ls())
pacman::p_load(dplyr, gtsummary, kableExtra, flextable, ggplot2,
               usmap, RColorBrewer, wesanderson, viridis)

# Directories
dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
data_dir <- paste0(dir, "00_data/for_figures/")
out_dir <- paste0(dir, "02_plots/")

# Load data
fig1_data <- read.csv(paste0(data_dir, "reduced_access_by_state_for_mapping.csv")) %>% 
  rename(state = state_name)
fig2_data <- read.csv(paste0(data_dir, "reduced_access_counties_for_mapping.csv")) %>% 
  rename(fips = county_fips)

# ==============================================================================

# Function to edit legend position and size
formatPlot <- function(myPlot) {
  myPlot +
    guides(color = guide_legend(override.aes = list(size = 1))) +
    theme(title = element_text(size = 16),
          legend.title = element_text(size = 15), 
          legend.text  = element_text(size = 12),
          legend.key.size = unit(1, "lines"),
          legend.position = "right")
}

# ------------------------------------------------------------------------------

# FIG 1 - STATE LEVEL

pdf(paste0(out_dir, "fig1_states_reduced_access.pdf"), height = 10, width = 15)

formatPlot(

  plot_usmap(data = fig1_data, regions = "states", values = "pct_counties_reduced_access") + 
    geom_polygon(data = usmapdata::us_map(regions = "states"),
                 aes(x, y, group = group), fill = NA, size = .25, color = "black") +
    labs(title = "Percent of counties with reduced access to pharmacies by state, 2017") + 
    scale_fill_distiller(name = "Share of counties \n with reduced access (%)", label = scales::comma, direction = -1, palette = "RdBu") 

)

dev.off()

# ------------------------------------------------------------------------------

# FIG 2 - COUNTY LEVEL

pdf(paste0(out_dir, "fig2_counties_reduced_access.pdf"), height = 10, width = 15)

formatPlot(

  plot_usmap(data = fig2_data, regions = "counties", values = "reduced_access_county", color = NA) + 
    geom_polygon(data = usmapdata::us_map(regions = "counties"),
                 aes(x, y, group = group), fill = NA, size = .05, color = "grey40") +
    geom_polygon(data = usmapdata::us_map(regions = "states"),
                 aes(x, y, group = group), fill = NA, size = .3, color = "black") +
    labs(title = "Counties with reduced access to pharmacies in 2017",
         subtitle = paste0("[Note: gray counties had no data in 2017]")) + 
    scale_fill_manual(values = c( "#B2182B", "#FDDBC7", "#4393C3"),
                        name = "Pharmacy accessibility") +
    theme(legend.position = "right")

)

dev.off()

# TESTING COLORS ...

brewer.pal(11, "RdBu")
# to display that palette:
display.brewer.pal(11, "RdBu")