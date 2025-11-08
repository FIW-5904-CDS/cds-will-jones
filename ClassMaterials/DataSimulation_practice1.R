library(tidyverse)
library(ggplot2)
library(ggthemes)



set.seed(1)   # makes results reproducible

# Sample size and parameters
n <- 100
mean_size <- 3.5
sd_size <- 2

# Draw random body sizes from a Normal distribution
body_size <- rnorm(n, mean = mean_size, sd = sd_size)

# Combine into a tibble
sim <- tibble(id = 1:n, body_size = body_size)

# Look at the data
print(sim)
summary(sim$body_size)

# Visualize with a histogram
ggplot(sim, aes(x = body_size)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Simulated body sizes", x = "Body size (lbs)", y = "Count") +
  theme_classic()


# =====================================================
# 2. Add a treatment effect
# =====================================================

# GOAL: Simulate a simple experimental effect on body size.

set.seed(1)

# Create a treatment variable (half control, half treated)
n <- 100
treatment <- rep(c("hair", "no_hair"), each = n/2)

# set means for each group
mean_control <- 4
mean_trt <- 3
sd_both <- 2

# Simulate body sizes with different means by treatment
body_size <- rnorm(
  n = n,
  mean = if_else(treatment == "hair", mean_control, mean_trt),
  sd = sd_both
)

# Combine into tibble
(sim2 <- tibble(treatment, body_size))

# Visualize
ggplot(sim2, aes(x = treatment, y = body_size)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Simulated body sizes by treatment",
       x = "Treatment", y = "Body size (lbs)") +
  theme_classic()

# Quick summaries
sim2 |>
  group_by(treatment) |>
  summarise(mean_size = mean(body_size), sd_size = sd(body_size), n = n())

# =====================================================
# 3. Add a random site effect (optional)
# =====================================================

# GOAL: Add realistic variation among sites.

set.seed(100)

# Define number of sites and individuals per site
n_site <- 5
n_per_site <- 20
n <- n_site * n_per_site

site <- rep(1:n_site, each = n_per_site)

# Each site gets its own random offset (site effect)
site_effect <- rnorm(n_site, mean = 0, sd = 0.5)[site] # the [site] part is indexing by the site vector here (e.g. for site = 1, it pulls out the first value from rnorm, site 2 has 2nd value and so on)

# baseline mean and residual sd
mean_overall <- 3.5
resid_sd <- 1

# Generate body sizes including site effects
body_size <- rnorm(n, mean = mean_overall + site_effect, sd = resid_sd)

# Combine into tibble
(sim3 <- tibble(site = factor(site), body_size))

# Visualize variation among sites
ggplot(sim3, aes(x = site, y = body_size)) +
  geom_boxplot(fill = "plum") +
  labs(title = "Simulated body sizes by site",
       x = "Site", y = "Body size (lbs)") +
  theme_classic()

# Check site means
sim3 |>
  group_by(site) |>
  summarise(mean_size = mean(body_size), sd_size = sd(body_size))



sim3 %>% ggplot(aes(x= body_size, fill = site)) +
  geom_density(position = 'identity', bins = 50, alpha = 0.2, color = NA) +
  theme_fivethirtyeight()
