
library(sf)
library(terra)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(nngeo)
library(leaflet)
library(leaflegend)
library(tigris)
library(tictoc)
library(mapview)
setwd(r"{E:\01_PROJECTS\01_Roads}")

# This is basically just the first couple of lines copied from my working script.
# these lines just read in vector data sets that serve as the basis for our analysis


data <- st_read('06_fixing_final_files\\pairwise_data_07_28.gpkg')

glimpse(data)

# The data is already tidy. 
# each row is an observation and each column is a variable

data %>% st_drop_geometry() %>% sample_n(10000) %>% ggplot(aes(x = dist_00, y = dist_24)) +
  geom_point(alpha = 0.2) +
  geom_abline(color = "red", linetype = "dashed") +
  theme_minimal()


data_trim <- data %>% st_drop_geometry() %>% sample_n(500)

write.csv(data_trim, r"{C:\Users\wj436\OneDrive\Desktop\cds-will-jones\distance_points_class.csv}")
