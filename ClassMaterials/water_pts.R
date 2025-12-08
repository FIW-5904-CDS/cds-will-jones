
#read in USGS 3dhp .gpkg file
#clip to VA for test analysis

library(sf)
library(tidyverse)
library(ggplot2)
library(plotly)
library(tictoc)

us_states <- st_read(r"{E:\01_PROJECTS\01_Roads\02_vectors\USA_State.shp}") %>% st_transform(crs='EPSG:5070')
va_and_surrounding <- us_states %>% filter(STATE_ABBR %in% c("VA", "DC", "TN", "KY", "WV", "NC", "MD"))

t <- st_layers(r"{E:\01_PROJECTS\01_Roads\02_vectors\3dhp_all_CONUS_20250313_GDB.gdb}")
t
# we want:
# 'hydro_3dhp_all_flowline'
# 'hydro_3dhp_all_waterbody'

clip_geom <- st_transform(va_and_surrounding[6,], crs = 6350) %>% 
  st_union() %>%
  st_as_sf()



waterbodies <- st_read(
  dsn   = r"{E:\01_PROJECTS\01_Roads\02_vectors\3dhp_all_CONUS_20250313_GDB.gdb}",
  layer ='hydro_3dhp_all_waterbody',
  wkt_filter = st_as_text(st_geometry(clip_geom))
) %>%
    st_zm()  %>%
  select(c(7,8))


wb_bounds <- st_boundary(waterbodies)


flowlines <- st_read(
  dsn   = r"{E:\01_PROJECTS\01_Roads\02_vectors\3dhp_all_CONUS_20250313_GDB.gdb}",
  layer ='hydro_3dhp_all_flowline',
  wkt_filter = st_as_text(st_geometry(clip_geom))
) %>%
    st_zm() %>% 
  select(5:8)





water_features <- st_combine(c(st_geometry(flowlines), st_geometry(wb_bounds))) %>% 
  st_intersection(clip_geom)


water_pts <- st_sample(water_features, size = 10000) %>% st_as_sf()
water_pts %>% st_geometry() %>% plot()

#st_write(water_pts, "02_vectors\\water_pts_test.gpkg")

########################################

# remove flowline segments that fall inside waterbodies
flowlines_outside <- st_difference(flowlines, st_union(waterbodies))

# combine flowlines (outside ponds) with waterbody boundaries
water_features <- st_combine(c(st_geometry(flowlines_outside),
                               st_geometry(wb_bounds))) %>%
  st_intersection(clip_geom)

# sample points along the combined linear features
water_pts <- st_sample(water_features, size = 10000) %>% 
  st_as_sf()

plot(st_geometry(water_features))
plot(st_geometry(water_pts), add = TRUE, col = "red", pch = 20)
