

# This table is easy to use for calculating summary statistics of each distribution
setwd("E:\\01_PROJECTS\\01_Roads\\06_fixing_final_files")
all_data <- st_read('pairwise_data_07_28.gpkg')
all_data <- all_data %>% mutate(ratio = (dist_24 / dist_00)) %>% mutate(log_ratio = log10(ratio))
#head(all_data)
# While we pivot this data to "long" format for easier visualization
long_data <- all_data %>%
  pivot_longer(
    cols = c(dist_00, dist_24, log_ratio),
    names_to = "measure_type", 
    values_to = "meters" 
  )


summary_table <- long_data %>% st_drop_geometry() %>% filter(measure_type != 'log_ratio') %>% group_by(measure_type) %>% summarise(Mean = mean(meters), 
          SD = sd(meters),
          Min = min(meters, na.rm = TRUE),
          `1st quartile` = quantile(meters, 0.25),
          Median = quantile(meters, 0.5),
          `3rd Quartile` = quantile(meters, 0.75),
          Max = max(meters),)

summary_table <- summary_table %>% 
  rename(Year = measure_type) %>% 
  mutate(across(-c(Year, Min), ~ round(.x, 2)))

summary_table[1,1] <- '2000'
summary_table[2,1] <- '2024'

# Identify numeric rows for 2000 and 2024
row_2000 <- summary_table %>% filter(Year == "2000") %>% select(-Year)
row_2024 <- summary_table %>% filter(Year == "2024") %>% select(-Year)

# Compute differences
difference_values <- row_2000 - row_2024

# Build difference row
difference_row <- difference_values %>%
  mutate(Year = "Difference") %>%
  relocate(Year)

# Append to table
summary_table <- bind_rows(summary_table, difference_row)


summary_table <- summary_table %>% gt() %>% fmt_number(columns = 'Min', decimals = 5) %>%
  cols_label(Year ~ '') %>%
  tab_header(title = 'Summary Statistics (m)') %>% 
  tab_style(style = cell_text(weight = 'bold'), locations = cells_body(columns = Year)) %>%
  opt_stylize(style =1, color = 'gray')

#gtsave(summary_table, r'{E:\01_PROJECTS\01_Roads\05_viz\summary_stats1.png}')



{r fig.height =8, fig.width=10}
#| fig-cap: 'Distribution of Paired Sample Points'

# Sample jitter data separately
jitter_sample <- long_data %>%
  filter(measure_type %in% c('dist_00', 'dist_24')) %>%
  mutate(measure_type = factor(measure_type, levels = c('dist_00', 'dist_24'))) %>%
  filter(
    meters >= quantile(meters, 0.05, na.rm = TRUE),
    meters <= quantile(meters, 0.95, na.rm = TRUE)
  ) %>%
  sample_n(2000)  # Smaller sample just for jitter
levels(jitter_sample$measure_type) <- c("2000", "2024")

# Full data for boxplot
box_data <- long_data %>%
  filter(measure_type %in% c('dist_00', 'dist_24')) %>%
  mutate(measure_type = factor(measure_type, levels = c('dist_00', 'dist_24'))) %>%
  filter(
    meters >= quantile(meters, 0.05, na.rm = TRUE),
    meters <= quantile(meters, 0.95, na.rm = TRUE)
  )
levels(box_data$measure_type) <- c("2000", "2024")

box_plot <- ggplot(box_data, aes(x = measure_type, y = meters)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    data = jitter_sample, 
    aes(x = measure_type, y = meters, color = measure_type), # map color to measure_type
    width = 0.35, alpha = 0.4, size = 4
  ) +
  scale_color_manual(values = c("2000" = "#ea5f94", "2024" = "#3940bb")) + # custom colors
  theme_minimal() +
  xlab("Year") +
  ylab("Distance from Nearest Road (meters)") +
  theme(legend.position = "none")  # optional: remove legend


{r fig.height =8, fig.width=10}
#| fig-cap: "Distribution of Paired Points"
pairwise_histogram <- long_data %>%
  filter(measure_type %in% c('dist_00','dist_24'
  )) %>%
  mutate(measure_type = factor(measure_type,
                          levels = c("dist_00", "dist_24"),
                          labels = c("2000", "2024"))) %>%
  ggplot(mapping = aes(x = log10(meters), fill = measure_type)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 500) +
  scale_fill_manual(values = c("2000" = "#ea5f94", "2024" = "#3940bb")) +
    labs(
    fill = "Year",  # <–– legend title
    x = "Distance to Nearest Road (log10 meters)",
    y = "Count"
  ) +
  theme_minimal() 




valleys_coves <- long_data %>%
  filter(measure_type %in% c('dist_00','dist_24'
  )) %>%
  filter(ecoregion == "Limestone Valleys and Coves"
  ) %>%
  mutate(measure_type = factor(measure_type,
                          levels = c("dist_00", "dist_24"),
                          labels = c("2000", "2024"))) %>%
  ggplot(mapping = aes(x = meters, fill = measure_type)) +
  geom_density(position = "identity", alpha = 0.5) +
  scale_fill_manual(name = 'Year', values = c("2000" = "#ea5f94", "2024" = "#3940bb")) +
  theme_minimal() +
  xlab('Distance from Nearest Road (m)') +
  ylab('')



{r fig.height =8, fig.width=10}
#| lightbox: true
library(ggplot2)
library(ggridges)
library(dplyr)
library(forcats)

# Combine and prepare
long_data_viz <- long_data %>%
  filter(measure_type %in% c("dist_00", "dist_24")) %>%
  mutate(
    period = case_when(
      measure_type == "dist_00" ~ "2000",
      measure_type == "dist_24" ~ "2024"
    ),
    lulc = fct_reorder(lulc, desc(meters), .fun = mean, na.rm = TRUE)
  )

# Plot
lulc_2000_vs_2024 <- ggplot(long_data_viz, aes(x = log1p(meters), y = lulc, fill = period)) +
  geom_density_ridges(
    alpha = 0.6,
    scale = 0.97,
    stat = "binline",
    bins = 100,
    position = "identity",
    color = NA
  ) +
  theme_ridges() +
  ylab("Land Use/Land Cover") +
  xlab("log1p(Distance in meters)") +
  labs(fill = "Year") + 
scale_fill_manual(name = 'Year', values = c("2000" = "#ea5f94", "2024" = "#3940bb"
)) + 
  ggtitle("Distribution of Distances (2000 vs 2024)")




#| fig-cap: 'Uniform Random Sampling of 1e6 Points Across Virginia (5,000 shown for interpretability)'
#| echo: false
state_outline <- st_read(r"{E:\01_PROJECTS\01_Roads\02_vector_data\tl_2024_us_state.shp}") %>% filter(GEOID == 51) %>% st_geometry()
sample_pts <- sample_n(all_data, 5000) %>% st_geometry() %>% st_transform(crs = crs(state_outline))

study_area <- ggplot() +
  geom_sf(data = state_outline, fill = NA, color = "black") +
  geom_sf(data = sample_pts, color = "black", size = 2) +
  coord_sf(expand = FALSE, default_crs = sf::st_crs(state_outline)) +
  theme(aspect.ratio = NULL) +
  theme_map()


study_area

pairwise_histogram / (box_plot + valleys_coves + lulc_2000_vs_2024)
