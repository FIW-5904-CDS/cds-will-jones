library(sf)
library(tidyverse)
library(ggplot2)
library(plotly)
library(tictoc)
library(terra)


# read in the USGS 3dhp data set
# we only need flowlines (lines) and waterbodies (polygons)

t <- st_layers(r"{E:\01_PROJECTS\01_Roads\02_vector_data\3dhp_all_CONUS_20250313_GDB.gdb}") # get layer names of 
t


# read in a county boundary (could be any boundary file) to limit our analysis for now

county <- st_read(r"{E:\01_PROJECTS\01_Roads\02_vector_data\tl_2024_us_county.shp}") %>%
  filter(STATEFP == '51', NAMELSAD %in% c("Bedford County", 'Franklin County')) %>%
  st_geometry() %>%
  st_transform(crs = 'EPSG:5070') %>%
  st_union()



# read in the waterbody features 

waterbodies <- st_read(
  dsn   = r"{E:\01_PROJECTS\01_Roads\02_vector_data\3dhp_all_CONUS_20250313_GDB.gdb}",
  layer ='hydro_3dhp_all_waterbody',
  wkt_filter = st_as_text(st_geometry(county))
) %>%
  st_zm()  %>%
  select(c(7,8)) %>%
  st_transform(crs = 'EPSG:5070')

# convert to just edges so that we can sample along the outside

wb_edges <- st_boundary(waterbodies)


# read in flowlines and subtract the features that fall within the waterbodies layer

flowlines <- st_read(
  dsn   = r"{E:\01_PROJECTS\01_Roads\02_vector_data\3dhp_all_CONUS_20250313_GDB.gdb}",
  layer ='hydro_3dhp_all_flowline',
  wkt_filter = st_as_text(st_geometry(county))
) %>%
    st_zm() %>% 
  select(5:8) %>%
  st_transform(crs = 'EPSG:5070')


flowlines <- st_difference(flowlines, st_union(waterbodies)) # this line removes flowline segments that fall WITHIN waterbodies



# combine the augmented flowlines with the waterbody edges (shorelines) so that we can sample along
# the linear features

water_features <- st_combine(c(st_geometry(flowlines), # combine flowlines with waterbody edges
                               st_geometry(wb_edges))) %>%
  st_intersection(county) %>% st_cast("LINESTRING") %>% st_as_sf()


water_pts <- st_line_sample(water_features, n = 1000, type = 'random') %>%  # sample points along the combined linear features
  st_as_sf() %>%
  filter(!st_is_empty(.))


plot(st_geometry(water_features))
plot(st_geometry(water_pts), add = TRUE, col = "red", pch = 20)



# test to make sure our method worked:

waterbodies_smaller <- waterbodies %>% st_buffer(-5) # buffer -5m so that the points lying on waterbody edges are not counted

pts_inside <- water_pts %>% st_filter(waterbodies_smaller, predicate = 'within') %>% nrow() # check for pts inside waterbodies
pts_inside == 0 #should be TRUE

#st_write(water_pts, r"{E:\01_PROJECTS\01_Roads\02_vector_data\water_ptsonly.gpkg}", append = FALSE)
#st_write(waterbodies, r"{E:\01_PROJECTS\01_Roads\02_vector_data\waterbodiesSML.gpkg}")
#st_write(water_features, r"{E:\01_PROJECTS\01_Roads\02_vector_data\CDS1201.gpkg}")


##############################

# create a study area outline with a suitable buffer
# this addresses the cases where a point is sampled close to the edge of the study area boundary
# and its closest point is actually a road outside of the study area (in a different county, state, etc.) 

county_buffer <- county %>% st_buffer(3000) %>% st_union()

# read in road network files for 2000 and 2024
# ensure that the 2000 dataset is "snapped"
roads_00 <- st_read(r"{E:\01_PROJECTS\01_Roads\02_vector_data\02_tiger2k\snapped_00.gpkg}",
wkt_filter = st_as_text(st_geometry(county_buffer))) %>% st_transform(crs= 'EPSG:5070') %>% select(CFCC)

plot(st_geometry(roads_00))
plot(st_geometry(water_pts), add = TRUE, col = 'red')

# this step is important (below). it ensures that no single point features created by the densify step
# are included as road segments
roads_00 <- roads_00[npts(roads_00, by_feature = TRUE) >= 2, ] ################################# BIG IMPORTANT STEP!!!!!

# clip to our study area

roads_24 <- st_read(r"{E:\01_PROJECTS\01_Roads\02_vector_data\03_TL_2024_lines\all_2024_roads.gpkg}",
wkt_filter = st_as_text(st_geometry(county_buffer))) %>% st_transform(crs='EPSG:5070')

roads_24 <-  roads_24 %>% select('MTFCC')


# measuring distance to the nearest road for every randomly sampled water point

# get the index of the road that is nearest to each random point
# should be an index equal in length to the # of pts sampled
tic()
index_24 <- st_nearest_feature(water_pts, roads_24)
toc()

tic()
index_00<- st_nearest_feature(water_pts, roads_00)
toc()

# for each nearest road feature, grab its classification
water_pts$rdcls_00 <- roads_00$CFCC[index_00]
water_pts$rdcls_24 <- roads_24$MTFCC[index_24]


# grab the actual geometry of the nearest feature for every point so that we can measure the distances
tic()
nearest_road_geom_00 <- roads_00[index_00, ]
distance_meters_00 <- st_distance(water_pts, nearest_road_geom_00, by_element = TRUE)
distance_meters_00 <- as.numeric(distance_meters_00)
# create column called dist_00 to hold our distances
water_pts$dist_00 <- as.numeric(distance_meters_00)
summary(water_pts$dist_00)
quantile(water_pts$dist_00, 0.95, na.rm = TRUE)
toc()

# repeat the above process for our 2024 road network data set
tic()
nearest_road_geom_24 <- roads_24[index_24, ]
distance_meters_24 <- st_distance(water_pts, nearest_road_geom_24, by_element = TRUE)
distance_meters_24 <- as.numeric(distance_meters_24)
water_pts$dist_24 <- as.numeric(distance_meters_24)
summary(water_pts$dist_24)
quantile(water_pts$dist_24, 0.95, na.rm = TRUE)
toc()


# read in TIGER/line county vector data
us_counties <- st_read(r"{E:\01_PROJECTS\01_Roads\02_vector_data\tl_2024_us_county.shp}")
va_counties <- us_counties %>% filter(STATEFP == 51)
va_counties <- st_transform(va_counties, crs(water_pts))



water_pts %>% st_drop_geometry() %>% pivot_longer(cols = c(dist_00, dist_24), names_to = 'year', values_to = 'meters') %>%
  ggplot(aes(x = log10(meters), fill = year)) +
  geom_density(position = 'identity', alpha = 0.5)



#water_pts <- st_join(water_pts, va_counties["NAME"])
ix <- st_intersects(water_pts, va_counties)
county_index <- sapply(ix, function(i) if (length(i) > 0) i[1] else NA)
water_pts$county_name <- va_counties$NAME[county_index]


# read in USGS 8-digit watershed boundaries
hu8s <- st_read(r"{E:\01_PROJECTS\01_Roads\02_vector_data\us_watersheds_hu8.shp}") %>% select(c('name', 'huc8')) %>% 
  st_transform(crs = "EPSG:5070")
ix <- st_intersects(water_pts, hu8s)
hu_index <- sapply(ix, function(i) if (length(i) > 0) i[1] else NA)
water_pts$ws_name <- hu8s$name[hu_index]
water_pts$ws_code <- hu8s$huc8[hu_index]

# read in EPA level IV eco regions
# level 4 is more specific than level 3, but they contain level 3 and 4 info, so we can extract both for each pt
eco_l4 <- st_read(r"{E:\01_PROJECTS\01_Roads\02_vector_data\us_eco_l4.shp}") %>% filter(STATE_NAME == 'Virginia') %>% 
  select(c("US_L4CODE", "US_L4NAME", "US_L3NAME", "US_L3CODE")) %>% 
  st_transform(crs = 'EPSG:5070')
plot(st_geometry(eco_l4))
ix <- st_intersects(water_pts, eco_l4)
eco_index <- sapply(ix, function(i) if (length(i) > 0) i[1] else NA)
water_pts$ecoregion <- eco_l4$US_L4NAME[eco_index]
water_pts$eco_code <- eco_l4$US_L4CODE[eco_index]

# omit NA points from final data set
# this simply ensures that no points are designated as NA because of a missing cell or feature in
# one of our datasets
random_no_na <- na.omit(water_pts)
