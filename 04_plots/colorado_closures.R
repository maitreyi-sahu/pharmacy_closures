# Colorado map of pharmacy closures for Kelly's paper
# MSahu
# April 27

# ==============================================================================

# Setup
rm(list=ls())
pacman::p_load(dplyr, data.table, gtsummary, kableExtra, flextable, ggplot2, sf, usmap, 
               RColorBrewer, wesanderson, viridis)

# Directories
dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/PCMA/"
data_dir <- paste0(dir, "00_data/for_figures/")
out_dir <- paste0(dir, "02_plots/")

# ==============================================================================

# Load data

ncpdp_ids_CO <- readRDS(paste0(dir, "00_data/processed/NCPDP/ncpdp_cleaned_long_with_dc.rds")) %>% 
  filter(state_code == "CO") %>% 
  select(ncpdp_id, npi, Year, active17_21) %>% 
  filter(active17_21 == 1 & Year == 2019) %>% select(-active17_21, -Year)

geocodes_CO <- readRDS(paste0(dir, "00_data/processed/NCPDP/ncpdp_with_coords_CLEAN.rds")) %>% 
  filter(state_code == "CO") %>% 
  left_join(ncpdp_ids_CO, by = "ncpdp_id") %>% 
  select(npi, LAT, LONG) %>% 
  mutate(npi = as.integer(npi))

kelly_data <- fread(paste0(data_dir, "CO Pharmacy Closures_For Mapping.csv")) %>% 
  rename(state = StatePhysical) %>% 
  left_join(geocodes_CO, by = "npi") %>% 
  mutate(LAT = as.numeric(LAT),
         LONG = as.numeric(LONG)) %>% 
  mutate(`Pharmacy Type` = ifelse(dispenser_class_code == "Chain Pharmacy", "Chain", "Independent"))

fig1_data <- usmap_transform(kelly_data, 
                            input_names = c("LONG", "LAT"),
                            output_names = c("x", "y")) 

# ==============================================================================

# Function to edit legend position and size
formatPlot <- function(myPlot) {
  myPlot +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    theme(title = element_text(size = 14),
          legend.title = element_text(size = 13), 
          legend.text  = element_text(size = 13),
          legend.key.size = unit(3.5, "lines"),
          legend.position = "bottom")
}

# ------------------------------------------------------------------------------

# FIG 1 - COLORADO STATE

pdf(paste0(out_dir, "colorado_closures.pdf"), height = 6, width = 6)

formatPlot(
  
  plot_usmap(regions = "counties", include ="CO") + 
    geom_point(data = fig1_data,
                 aes(x, y, color = `Pharmacy Type`), size = 2.5, alpha = .8) +
    scale_color_manual(values = c("chocolate4", "seagreen")) +
    labs(title = "Pharmacy closures in Colorado" #,
         #subtitle = paste0("")
         )
  
)

dev.off()