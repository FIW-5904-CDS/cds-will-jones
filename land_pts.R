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


us_states <- st_read("us_state_boundaries\\USA_State.shp") %>% st_transform(crs='EPSG:5070')
va_and_surrounding <- us_states %>% filter(STATE_ABBR %in% c("VA", "DC", "TN", "KY", "WV", "NC", "MD"))


roads_00 <- st_read(r"{E:\01_PROJECTS\01_Roads\02_vectors\02_tiger2k\snapped_00.gpkg}") %>% st_transform(crs= 'EPSG:5070')
roads_00 <- roads_00 %>% select(CFCC)

roads_00 <- roads_00[npts(roads_00, by_feature = TRUE) >= 2, ] ################################# BIG IMPORTANT STEP!!!!!

roads_24 <- st_read(r"{E:\01_PROJECTS\01_Roads\02_vectors\03_TL_2024_lines\all_2024_roads.shp}") %>% st_transform(crs='EPSG:5070')
roads_24 <-  roads_24 %>% select('MTFCC')


sum(npts(roads_00, by_feature = TRUE) <=2 )


set.seed(97)
random_points <- st_sample(va_and_surrounding[6,], size = 1e6, type = "random")

random_points_sf <- st_sf(geometry = random_points) %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )


tic()
index_24 <- st_nearest_feature(random_points_sf, roads_24)
toc()

tic()
index_00<- st_nearest_feature(random_points_sf, roads_00)
toc()

random_points_sf$rdcls_00 <- roads_00$CFCC[index_00]
random_points_sf$rdcls_24 <- roads_24$MTFCC[index_24]

tic()
nearest_road_geom_00 <- roads_00[index_00, ]
distance_meters_00 <- st_distance(random_points_sf, nearest_road_geom_00, by_element = TRUE)
distance_meters_00 <- as.numeric(distance_meters_00)
random_points_sf$dist_00 <- as.numeric(distance_meters_00)
summary(random_points_sf$dist_00)
quantile(random_points_sf$dist_00, 0.95)
toc()

tic()
nearest_road_geom_24 <- roads_24[index_24, ]
distance_meters_24 <- st_distance(random_points_sf, nearest_road_geom_24, by_element = TRUE)
distance_meters_24 <- as.numeric(distance_meters_24)
random_points_sf$dist_24 <- as.numeric(distance_meters_24)
summary(random_points_sf$dist_24)
quantile(random_points_sf$dist_24, 0.95)
toc()

random_points_vect <- vect(random_points_sf)
va_nlcd <- rast("va_nlcd_2023.tif")
nlcd_at_points <- terra::extract(va_nlcd, random_points_vect) 
random_points_sf$nlcd <- nlcd_at_points$va_nlcd_2023
random_points_sf$lulc <- as.factor(random_points_sf$nlcd)
levels(random_points_sf$lulc)
levels(random_points_sf$lulc) = c('water', 'developed_open', 'developed_light', 'developed_med', 'developed_high',
                                  'barren', 'deciduous', 'evergreen', 'mixed forest', 'shrub', 'grassland',
                                  'pasture','crops', 'woody wetlands', 'emergent wetlands'
)

us_counties <- st_read("02_vectors\\tl_2024_us_county.shp")
va_counties <- us_counties %>% filter(STATEFP == 51)
va_counties <- st_transform(va_counties, crs(random_points_sf))

#random_points_sf <- st_join(random_points_sf, va_counties["NAME"])
ix <- st_intersects(random_points_sf, va_counties)
county_index <- sapply(ix, function(i) if (length(i) > 0) i[1] else NA)
random_points_sf$county_name <- va_counties$NAME[county_index]



hu8s <- st_read("02_vectors\\Shape\\WBDHU8.shp") %>% select(c('name', 'huc8')) %>% 
  st_transform(crs = "EPSG:5070")
ix <- st_intersects(random_points_sf, hu8s)
hu_index <- sapply(ix, function(i) if (length(i) > 0) i[1] else NA)
random_points_sf$ws_name <- hu8s$name[hu_index]
random_points_sf$ws_code <- hu8s$huc8[hu_index]

eco_l4 <- st_read("02_vectors\\us_eco_l4.shp") %>% filter(STATE_NAME == 'Virginia') %>% 
  select(c("US_L4CODE", "US_L4NAME", "US_L3NAME", "US_L3CODE")) %>% 
  st_transform(crs = 'EPSG:5070')
plot(st_geometry(eco_l4))
ix <- st_intersects(random_points_sf, eco_l4)
eco_index <- sapply(ix, function(i) if (length(i) > 0) i[1] else NA)
random_points_sf$ecoregion <- eco_l4$US_L4NAME[eco_index]
random_points_sf$eco_code <- eco_l4$US_L4CODE[eco_index]


random_no_na <- na.omit(random_points_sf)


st_write(random_no_na, '06_fixing_final_files\\pairwise_points_final.gpkg')

plot(st_geometry(random_points_sf %>% sample_n(2000))) 


