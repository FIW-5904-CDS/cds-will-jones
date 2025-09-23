
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



us_states <- st_read("us_state_boundaries\\USA_State.shp") %>% st_transform(crs='EPSG:5070')
va_and_surrounding <- us_states %>% filter(STATE_ABBR %in% c("VA", "DC", "TN", "KY", "WV", "NC", "MD"))


roads_00 <- st_read(r"{E:\01_PROJECTS\01_Roads\02_vectors\02_tiger2k\snapped_00.gpkg}") %>% st_transform(crs= 'EPSG:5070')
roads_00 <- roads_00 %>% select(CFCC)

roads_00 <- roads_00[npts(roads_00, by_feature = TRUE) >= 2, ] ################################# BIG IMPORTANT STEP!!!!!

roads_24 <- st_read(r"{E:\01_PROJECTS\01_Roads\02_vectors\03_TL_2024_lines\all_2024_roads.shp}") %>% st_transform(crs='EPSG:5070')
roads_24 <-  roads_24 %>% select('MTFCC')