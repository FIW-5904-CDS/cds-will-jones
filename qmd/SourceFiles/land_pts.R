
setwd(r"{E:\01_PROJECTS\01_Roads}")

# Read in the TIGER/line state boundaries shapefile
# this will be used for defining our study area and sampling area (VA)
us_states <- st_read("02_vector_data\\USA_State.shp") %>% st_transform(crs='EPSG:5070')
va_and_surrounding <- us_states %>% filter(STATE_ABBR %in% c("VA", "DC", "TN", "KY", "WV", "NC", "MD"))

# read in road network files for 2000 and 2024
# ensure that the 2000 dataset is "snapped"
roads_00 <- st_read(r"{02_vector_data\02_tiger2k\snapped_00.gpkg}") %>% st_transform(crs= 'EPSG:5070')
roads_00 <- roads_00 %>% select(CFCC)
# this step is important (below). it ensures that no single point features created by the densify step
# are included as road segments
roads_00 <- roads_00[npts(roads_00, by_feature = TRUE) >= 2, ] ################################# BIG IMPORTANT STEP!!!!!

roads_24 <- st_read(r"{E:\01_PROJECTS\01_Roads\02_vectors\03_TL_2024_lines\all_2024_roads.shp}") %>% st_transform(crs='EPSG:5070')
roads_24 <-  roads_24 %>% select('MTFCC')

sum(npts(sample_n(roads_00, 100000), by_feature = TRUE) <2 ) # should be 0 !!!

# set seed for reproducibility, sample 1e6 points randomly
# sf's  type = random samples uniformly across the space by default
# *note: I have left our study area as 'va_and_surrounding' and then subset to VA (row 6) because eventually we
# will scale up to the entire US, so I think this is easier for documentation
set.seed(97)
random_points <- st_sample(va_and_surrounding[6,], size = 1e6, type = "random")

random_points_sf <- st_sf(geometry = random_points) %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

# get the index of the road that is nearest to each random point
# should be an index equal in length to the # of pts sampled
tic()
index_24 <- st_nearest_feature(random_points_sf, roads_24)
toc()

tic()
index_00<- st_nearest_feature(random_points_sf, roads_00)
toc()

# for each nearest road feature, grab its classification
random_points_sf$rdcls_00 <- roads_00$CFCC[index_00]
random_points_sf$rdcls_24 <- roads_24$MTFCC[index_24]


# grab the actual geometry of the nearest feature for every point so that we can measure the distances
tic()
nearest_road_geom_00 <- roads_00[index_00, ]
distance_meters_00 <- st_distance(random_points_sf, nearest_road_geom_00, by_element = TRUE)
distance_meters_00 <- as.numeric(distance_meters_00)
# create column called dist_00 to hold our distances
random_points_sf$dist_00 <- as.numeric(distance_meters_00)
summary(random_points_sf$dist_00)
quantile(random_points_sf$dist_00, 0.95)
toc()

# repeat the above process for our 2024 road network data set
tic()
nearest_road_geom_24 <- roads_24[index_24, ]
distance_meters_24 <- st_distance(random_points_sf, nearest_road_geom_24, by_element = TRUE)
distance_meters_24 <- as.numeric(distance_meters_24)
random_points_sf$dist_24 <- as.numeric(distance_meters_24)
summary(random_points_sf$dist_24)
quantile(random_points_sf$dist_24, 0.95)
toc()

# turn random points sf object into a SpatRaster "vect" object
# this is necessary to extract the NLCD land cover value at each point using raster operations
# I might revisit this as we upscale if it turns out to be inefficient, but it has worked well so far
random_points_vect <- vect(random_points_sf)
va_nlcd <- rast("va_nlcd_2023.tif")
nlcd_at_points <- terra::extract(va_nlcd, random_points_vect)
# create two columns containing the 2 digit NLCD code and its label as a string
random_points_sf$nlcd <- nlcd_at_points$va_nlcd_2023
random_points_sf$lulc <- as.factor(random_points_sf$nlcd)
levels(random_points_sf$lulc)
levels(random_points_sf$lulc) = c('water', 'developed_open', 'developed_light', 'developed_med', 'developed_high',
                                  'barren', 'deciduous', 'evergreen', 'mixed forest', 'shrub', 'grassland',
                                  'pasture','crops', 'woody wetlands', 'emergent wetlands'
)
# read in TIGER/line county vector data
us_counties <- st_read("02_vectors\\tl_2024_us_county.shp")
va_counties <- us_counties %>% filter(STATEFP == 51)
va_counties <- st_transform(va_counties, crs(random_points_sf))

#random_points_sf <- st_join(random_points_sf, va_counties["NAME"])
ix <- st_intersects(random_points_sf, va_counties)
county_index <- sapply(ix, function(i) if (length(i) > 0) i[1] else NA)
random_points_sf$county_name <- va_counties$NAME[county_index]


# read in USGS 8-digit watershed boundaries
hu8s <- st_read("02_vectors\\Shape\\WBDHU8.shp") %>% select(c('name', 'huc8')) %>% 
  st_transform(crs = "EPSG:5070")
ix <- st_intersects(random_points_sf, hu8s)
hu_index <- sapply(ix, function(i) if (length(i) > 0) i[1] else NA)
random_points_sf$ws_name <- hu8s$name[hu_index]
random_points_sf$ws_code <- hu8s$huc8[hu_index]

# read in EPA level IV eco regions
# level 4 is more specific than level 3, but they contain level 3 and 4 info, so we can extract both for each pt
eco_l4 <- st_read("02_vector_data\\us_eco_l4.shp") %>% filter(STATE_NAME == 'Virginia') %>% 
  select(c("US_L4CODE", "US_L4NAME", "US_L3NAME", "US_L3CODE")) %>% 
  st_transform(crs = 'EPSG:5070')
#plot(st_geometry(eco_l4))
ix <- st_intersects(random_points_sf, eco_l4)
eco_index <- sapply(ix, function(i) if (length(i) > 0) i[1] else NA)
random_points_sf$ecoregion <- eco_l4$US_L4NAME[eco_index]
random_points_sf$eco_code <- eco_l4$US_L4CODE[eco_index]

# omit NA points from final data set
# this simply ensures that no points are designated as NA because of a missing cell or feature in
# one of our datasets
random_no_na <- na.omit(random_points_sf)

#st_write(random_no_na, '06_fixing_final_files\\pairwise_data_TODAYS_DATE.gpkg')

plot(st_geometry(random_no_na %>% sample_n(2000))) 


