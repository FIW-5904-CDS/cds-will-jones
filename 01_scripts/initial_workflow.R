library(sf)
library(terra)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

setwd(r"(E:\PROJECTS\Roads)")

pts <- st_read("outputs\\mont_pts_sf.shp")
hu8s_full <- st_read("vectors\\Shape\\WBDHU8.shp")
hu8s <- hu8s_full %>% select(c('name', 'huc8'))
plot(st_geometry(hu8s))

joined <- st_join(pts, hu8s, join = st_within)



eco_l4_filter <- eco_l4 %>% select(c("US_L4CODE", "US_L4NAME"))

eco_l4_filter <- st_transform(eco_l4_filter, crs(joined))

eco_l4_filter <- st_make_valid(eco_l4_filter)

joined <- st_join(joined, eco_l4_filter, join = st_within)

plot(st_geometry(joined_))

mc_roads <- st_read("outputs\\mont_roads.shp")
mc_roads <- mc_roads %>% select("mtfcc_code")
mc_roads <- st_transform(mc_roads, crs(joined))

joined_ <- st_join(joined, mc_roads, join = st_nearest_feature)

names(joined_)

colnames(joined_) <- c("dst_to_rd", 'nlcd', 'county', "lon", 'lat', "ws_name",'ws_code', 
                       'eco_code', 'eco_name', 'rd_class', 'geometry')

st_write(joined_, "mont_pts_test1.shp", overwrite = TRUE)

va_nlcd <- rast("va_nlcd_2023.tif")

va <- st_read("vectors\\va_boundary.shp")
plot(st_geometry(va))


random_points <- st_sample(va, size = 1e6, type = "random")

plot(st_geometry(random_points[1:2000]), add = TRUE)

roads <- st_read("va_roads.shp")

nearest_road_index <- st_nearest_feature(random_points, roads)

# Calculate distance to that nearest feature
nearest_road_geom <- roads[nearest_road_index, ]
distances_meters <- st_distance(random_points, nearest_road_geom, by_element = TRUE)
distances_meters <- as.numeric(distances_meters)


random_points_sf <- st_sf(geometry = random_points)

random_points_sf$dist_to_road_m <- as.numeric(distances_meters)


random_points_vect <- vect(random_points_sf)

nlcd_at_points <- terra::extract(va_nlcd, random_points_vect) 

random_points_sf$nlcd <- nlcd_at_points$va_nlcd_2023


quantile(distances_meters)

hist(distances_meters, breaks = 100)

dists_df <- as.data.frame(distances_meters)



va_dist_histogram <- ggplot(data = dists_df, aes(x = distances_meters)) + 
  geom_histogram(bins = 1000)


va_dist_curve <- ggplot(data = dists_df, aes(x = distances_meters)) + 
  geom_density()

ggplotly(va_dist_histogram)

plot(random_points_sf[1:500,"dist_to_road_m"])
sum(distances_meters > 12000)

us_counties <- st_read("vectors/tl_2024_us_county.shp")

va_counties <- us_counties %>% filter(STATEFP == 51)

random_points_sf <- st_join(random_points_sf, va_counties["NAME"])


random_points_sf <- random_points_sf %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

points_df <- as.data.frame(random_points_sf)

mont_pts <- points_df %>% filter(NAME == "Montgomery") %>%
  filter(dist_to_road_m < 1000)

mont_pts_sf <- st_as_sf(mont_pts)
st_write(mont_pts_sf, "outputs/mont_pts_sf.shp")

  
mont_pts_plot <- ggplot(data = mont_pts, aes(x = lon, y = lat, fill = dist_to_road_m)) + 
  geom_point() +
  scale_fill_steps()

mont_pts_plot

mont_hist
ggplotly(mont_pts_plot)

plot(va_counties)



montgomery <- va_counties %>% filter(NAME == "Montgomery")

waterbodies <- st_read("vectors\\All_Lakes___Reservoirs.shp", wkt_filter = st_as_text(st_geometry(montgomery)))



mont_rds <- st_filter(roads, montgomery)

plot(st_geometry(mont_rds))
st_write(mont_rds, r"(outputs\\mont_roads.shp)")
