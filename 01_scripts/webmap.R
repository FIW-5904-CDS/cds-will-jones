library(ggplot2)
library(shiny)
library(plotly)
library(leaflet)
library(dplyr)
library(sf)
library(leaflegend)


diffs_no_snap <- st_read('04_seed98/difference.shp')

both_years <- bind_rows(
  data.frame(value = diffs_no_snap$difference, group = "no_snap"),
  data.frame(value = shapes_diff$difference, group = "snap"))


both_hist <- ggplot(both_years, aes(x = value, fill = group)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 5000) +
  scale_fill_manual(values = c("no_snap" = "#2fb5b5", "snap" = "#F34F00")) +
  theme_minimal()
ggplotly(both_hist)


dist_scatter <- ggplot(data = shapes_diff) +
  geom_point(aes(x = dist_2000, y = dst_t_r))


dist_scatter

setwd('/Volumes/MyPassport/01_PROJECTS/01_Roads')

snapped_00 <- st_read(r'{E:\01_PROJECTS\01_Roads\04_seed98\snapped_00_distribution.shp}')
#shapes_00 <- st_read(r'{E:\01_PROJECTS\01_Roads\04_seed98\year_00_distribution.shp}')
shapes_23 <- st_read(r'{E:\01_PROJECTS\01_Roads\04_seed98\year_23_distribution.shp}')
shapes_diff <- st_transform(shapes_23, st_crs(snapped_00))


# Get match indices
match_idx <- st_equals(shapes_diff, snapped_00)

# Convert list to vector
match_vec <- sapply(match_idx, function(x) if (length(x) == 1) x else NA_integer_)

# Add column using matched indices
shapes_diff$dist_2000 <- snapped_00$dst_t_r[match_vec]

# Drop rows where no match was found
shapes_diff <- shapes_diff[!is.na(shapes_diff$dist_2000), ]


#shapes_diff$dist_2000 <- snapped_00$dst_t_r
shapes_diff$dist_2023 <- shapes_23$dst_t_r
shapes_diff$difference <- (shapes_diff$dst_t_r - shapes_diff$dist_2000)
shapes_diff <- shapes_diff %>%  select(c('dist_2000', 'dst_t_r', 'difference'))
#st_write(shapes_diff, '04_seed98/snapped_difference.shp')

ggplot(data = sample_n(shapes_diff, 10000)) +
  geom_sf(mapping = aes(color = difference ))

sample

test_results <- ks.test(shapes_diff$dist_2000, shapes_diff$dist_2023)

both_years <- bind_rows(
  data.frame(value = snapped_00$dst_t_r, group = "v00"),
  data.frame(value = shapes_23$dst_t_r, group = "v23"))

dists_and_diff <- bind_rows(
  data.frame(value = shapes_00$dst_t_r, group = "v00"),
  data.frame(value = shapes_23$dst_t_r, group = "v23"),
  data.frame(value = (shapes_23$dst_t_r - shapes_00$dst_t_r), group = 'difference'))



diff_only <- both_years %>% filter(group == 'difference')
mean(both_years$value)

diff_hist <- ggplot(data = shapes_diff, aes(x=difference)) +
  geom_histogram(bins = 1000)

ggplotly(diff_hist)

vline_data <- data.frame(
  x = c(mean(both_years$value[both_years$group == "v00"]), mean(both_years$value[both_years$group == "v23"]),
    label = c("2000 Mean", "2023 Mean", "Difference Mean"),color = c("black", "black")))

both_hist <- ggplot(both_years, aes(x = value, fill = group)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 1000) +
  scale_fill_manual(values = c("v00" = "#2fb5b5", "v23" = "#F34F00")) +
  theme_minimal()


ggplotly(both_hist)



### Vertical lines for group means: 
vline_data <- data.frame(
x = c(
  mean(both_years$value[both_years$group == "v00"]),
  mean(both_years$value[both_years$group == "v23"]),
  mean(both_years$value[both_years$group == "difference"])), label = c("2000 Mean", "2023 Mean", "Difference Mean"),
  color = c("blue", "red", "green"))




all_three_hist <- ggplot(both_years, aes(x = value, fill = group, color = group)) +
  geom_density( fill = NA, position = "identity", bins = 1000, size = 0.4, alpha = 0.2) +
  #scale_fill_manual(values = c("v00" = "#66c2a5", "v23" = "#fc8d62", "difference" = "#8da0cb")) +
  scale_color_manual(values = c("v00" = "blue", "v23" = "red", "difference" = "green")) +
  theme_minimal() +
  geom_vline(data = vline_data, aes(xintercept = x, color = label),
             linetype = "dashed", inherit.aes = FALSE) +
  geom_text(data = vline_data, aes(x = x, y = Inf, label = label, color = label),
            vjust = -0.5, angle = 90, hjust = -0.1, size = 3.5,
            show.legend = FALSE, inherit.aes = FALSE)



all_three_hist
ggplotly(all_three_hist)

################################################

virginia_data <- as.data.frame(state_data)

va_sample <- sample_n(virginia_data, size = 50000)
write.csv(va_sample, "outputs\\virginia_sample.csv")


va_extremes <- state_data %>% filter(dst_t_r >= 622)
st_write(va_extremes, "outputs\\va_extremes.shp")

shapes_00 <- st_read(r'{E:\01_PROJECTS\01_Roads\04_seed98\year_00_distribution.shp}')
shapes_23 <- st_read(r'{E:\01_PROJECTS\01_Roads\04_seed98\year_23_distribution.shp}')



both_years <- bind_rows(
  data.frame(value = shapes_00$dst_t_r, group = "v00"),
  data.frame(value = shapes_23$dst_t_r, group = "v23")
)


both_hist <- ggplot(both_years, aes(x = value, fill = group)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 500) +
  scale_fill_manual(values = c("v00" = "blue", "v23" = "red")) +
  theme_minimal()

both_hist
ggplotly(both_hist)

state_hist <- ggplot(data = shapes, aes(x= dst_t_r)) +
  geom_histogram(bins = 500, fill = "#84A83D", color = "black") +
  xlab("Distance to Nearest Road (m)") +
 ggtitle("State Distribution (bins = 500)")

ks.test(shapes_00$dst_t_r, shapes_23$dst_t_r)



title("State Distribution (bins = 500)")

state_hist
ggplotly(both_hist)


summary(shapes$dst_t_r)
quantile(shapes$dst_t_r, 0.95)

colnames(va_extremes) <- c("rd_code" , 
                           "rd_class"  ,
                           "dist_to_road"  ,
                           "nlcd" ,
                           "lulc",
                           "county_name"  ,
                           "lon", 
                           "lat", 
                           "ws_name",
                           "ws_code",
                           "ecoregion",
                           "eco_code",
                           "dist_round",
                           "geometry")

colnames(random_no_na)





va_extremes$lulc <- as.factor(va_extremes$lulc)

color_map <- c('#93C3F1', '#F3BF4B', '#F49A20', '#F34F00', '#F30000', '#CDDDD2', '#84A83D', '#009D50', '#B3E83C', '#FFEBBE', 
               '#FFEBAF', '#F6F56A',
               '#A87000', '#2f96b5', '#2fb5b5')

color_map <- c('#CDDDD2',  '#A87000', '#84A83D','#F49A20','#F3BF4B','#F30000',
                '#F34F00',   '#2fb5b5' 
               , '#009D50','#FFEBAF', '#B3E83C','#F6F56A', '#FFEBBE', 
                
                '#93C3F1','#2f96b5' )



color_map <- c(
  "barren" = '#CDDDD2',
  "crops" = '#A87000',
  "deciduous" = '#84A83D',
  "developed_light" = '#F49A20',
  "developed_open" = '#F3BF4B',
  "emergent wetlands" = '#2fb5b5',
  "evergreen" = '#009D50',
  "grassland" = '#FFEBBE',
  "mixed forest" = '#B3E83C',
  "pasture" = '#F6F56A',
  "shrub" = '#B3E83C',
  "water" = '#93C3F1',
  "woody wetlands" = '#2f96b5'
)

levels_used <- levels(va_extremes$lulc)

# Create a color palette using your custom colors
pal <- colorFactor(palette = color_map, domain = factor(va_extremes$lulc, levels = levels_used))


extremes_geo <- st_transform(va_extremes, "EPSG:4326")
borders_geo <- st_transform(va_and_surrounding, "EPSG:4326")

####extremes############################################
leaflet(data = extremes_geo) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "OSM Grey") %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(
    providers$Esri.WorldImagery, group = "Satellite") %>%

  addPolygons(data = borders_geo[6,],
              fill = FALSE,
              
              stroke = TRUE,
              color = "orange",
              opacity = 0.5,
              group = "State Boundary") %>%
  addCircleMarkers( data = extremes_geo ,, 
    radius = ~scales::rescale(log1p(dist_to_road), to = c(3, 9)),
    fillColor = ~pal(lulc),
    stroke = FALSE,
    fillOpacity = 0.5,
    popup = ~paste("Landcover:", lulc, "<br>", "Watershed:", ws_name , "<br>",
                   "Distance to Nearest Rd:", dist_round,"<br>",
                   "Nearest Rd. Class:", rd_class, "<br>",
                   "Eco Region:", ecoregion, "<br>"
                  
                   )) %>%
  
  #addCircleMarkers( data = extremes_geo %>% filter((lulc == "water" & is.na(ecoregion))), 
   #                 radius = ~scales::rescale(dist_to_road, to = c(3, 15)),
    #                fillColor = "#93C3F1",
     #               opacity = 0.5, 
      #              color = "black",
       #             weight = 0.2,
        #            fillOpacity = 0.5,
         #           popup = ~paste("Landcover:", lulc, "<br>", "Watershed:", ws_name , "<br>",
          #                         "Distance to Nearest Rd:", dist_round,"<br>",
           #                        "Nearest Rd. Class:", rd_class, "<br>",
            #                       "Eco Region:", ecoregion, "<br>"),
             #       
              #      group = "Filtered") %>%
  
  # addPolylines( data = roads,
  #              weight = 0.5, 
  #             color = 'black',
  #            opacity = 0.4, 
  #           group = "Roads"
  #  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~lulc,
    title = "Land Cover"
  ) %>%
  
  addLayersControl(baseGroups = c('OSM Grey', 'Satellite',  "OpenStreetMap" ), overlayGroups = "State Boundary") 


#######all points##################################################

sample_map <- sample_n(state_data, size = 100000)
sample_map <- st_transform(sample_map, "EPSG:4326")

colnames(state_data) <- c("rd_code" , 
                           "rd_class"  ,
                           "dist_to_road"  ,
                           "nlcd" ,
                           "lulc",
                           "county_name"  ,
                           "lon", 
                           "lat", 
                           "ws_name",
                           "ws_code",
                           "ecoregion",
                           "eco_code",
                           "dist_round",
                           "geometry")

state_data$lulc <- as.factor(state_data$lulc)
levels(state_data$lulc)

color_map <- c(
  "barren" = '#CDDDD2',
  "crops" = '#A87000',
  "deciduous" = '#84A83D',
  "developed_high" =   '#F30000' ,
  "developed_light" = '#F49A20',
  "developed_med" =   '#F34F00' ,
  "developed_open" = '#F3BF4B',
  "emergent wetlands" = '#2fb5b5',
  "evergreen" = '#009D50',
  "grassland" = '#FFEBBE',
  "mixed forest" = '#B3E83C',
  "pasture" = '#F6F56A',
  "shrub" = '#B3E83C',
  "water" = '#93C3F1',
  "woody wetlands" = '#2f96b5'
)

levels_used <- levels(state_data$lulc)

# Create a color palette using your custom colors
pal <- colorFactor(palette = color_map, domain = factor(state_data$lulc, levels = levels_used))


leaflet(data = sample_map) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "OSM Grey") %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(
    providers$Esri.WorldImagery, group = "Satellite") %>%
  
  addPolygons(data = borders_geo[6,],
              fill = FALSE,
              
              stroke = TRUE,
              color = "orange",
              opacity = 0.5,
              group = "State Boundary") %>%
  addCircleMarkers( data = sample_map , 
                    radius = ~scales::rescale(log1p(dist_to_road), to = c(1, 8)),
                    fillColor = ~pal(lulc),
                    stroke = FALSE,
                    fillOpacity = 0.5,
                    popup = ~paste("Landcover:", lulc, "<br>", "Watershed:", ws_name , "<br>",
                                   "Distance to Nearest Rd:", paste0(dist_round, " (m)"),"<br>",
                                   "Nearest Rd. Class:", rd_class, "<br>",
                                   "Eco Region:", ecoregion, "<br>"
                                   
                    )) %>%
  
  
  
  # addPolylines( data = roads,
  #              weight = 0.5, 
  #             color = 'black',
  #            opacity = 0.4, 
  #           group = "Roads"
  #  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~lulc,
    title = "Land Cover"
  ) %>%
  #addLegendSize(
  #  values = legend_values,  # Using min/max instead of breaks
  #  color = 'grey',
   # fillColor = 'grey',
  #  opacity = 0.5,
  #  position = "topright",
  # title = 'Distance to Road (m)',
  #  shape = 'circle',
  #  orientation = 'horizontal',
  #  breaks = 2,
  #  baseSize = 5
  #) %>% 
  addLayersControl(baseGroups = c('OSM Grey', 'Satellite',  "OpenStreetMap" ), overlayGroups = "State Boundary") 


###################################################################

ggplot(data = state_data, aes(x = dist_to_road)) +
  geom_histogram(bins = 500)


st_write(random_no_na, "outputs\\whole_state.shp")

summary(state_data$dist_to_road)
quantile(state_data$dist_to_road, 0.95)
