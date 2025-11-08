# =====================================================
# Simple_Simulation_Intro.R
# FIW5594: Conservation Data Science
# Introduction to Data Simulation
# =====================================================

# Load packages ----
library(tidyverse)
library(ggplot2)

# =====================================================
# 1. Simulate one variable
# =====================================================

# GOAL: Simulate 20 individuals and measure their body size.

set.seed(1)   # makes results reproducible

# Sample size and parameters
n <- 20
mean_size <- 10
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
  labs(title = "Simulated body sizes", x = "Body size (cm)", y = "Count") +
  theme_classic()

# =====================================================
# 2. Add a treatment effect
# =====================================================

# GOAL: Simulate a simple experimental effect on body size.

set.seed(1)

# Create a treatment variable (half control, half treated)
n <- 20
treatment <- rep(c("control", "trt"), each = n/2)

# set means for each group
mean_control <- 9
mean_trt <- 11
sd_both <- 2

# Simulate body sizes with different means by treatment
body_size <- rnorm(
  n = n,
  mean = if_else(treatment == "trt", mean_trt, mean_control),
  sd = sd_both
)

# Combine into tibble
(sim2 <- tibble(treatment, body_size))

# Visualize
ggplot(sim2, aes(x = treatment, y = body_size)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Simulated body sizes by treatment",
       x = "Treatment", y = "Body size (cm)") +
  theme_classic()

# Quick summaries
sim2 |>
  group_by(treatment) |>
  summarise(mean_size = mean(body_size), sd_size = sd(body_size), n = n())

# =====================================================
# 3. Add a random site effect (optional)
# =====================================================

# GOAL: Add realistic variation among sites.

set.seed(1)

# Define number of sites and individuals per site
n_site <- 4
n_per_site <- 5
n <- n_site * n_per_site

site <- rep(1:n_site, each = n_per_site)

# Each site gets its own random offset (site effect)
site_effect <- rnorm(n_site, mean = 0, sd = 0.5)[site] # the [site] part is indexing by the site vector here (e.g. for site = 1, it pulls out the first value from rnorm, site 2 has 2nd value and so on)

# baseline mean and residual sd
mean_overall <- 10
resid_sd <- 1

# Generate body sizes including site effects
body_size <- rnorm(n, mean = mean_overall + site_effect, sd = resid_sd)

# Combine into tibble
(sim3 <- tibble(site = factor(site), body_size))

# Visualize variation among sites
ggplot(sim3, aes(x = site, y = body_size)) +
  geom_boxplot(fill = "plum") +
  labs(title = "Simulated body sizes by site",
       x = "Site", y = "Body size (cm)") +
  theme_classic()

# Check site means
sim3 |>
  group_by(site) |>
  summarise(mean_size = mean(body_size), sd_size = sd(body_size))

# =====================================================
# 4. Try changing parameters
# =====================================================
# Change the sample size, mean, or sd above and re-run the code.
# Observe how the histogram or boxplot changes.
# Questions to ask:
#  - What does increasing SD do to your histogram?
#  - Do you see the treatment effect in the graph? 
#  - What happens when site variation increases?

# =====================================================
# 5. Simple Power Analysis using simulation
# =====================================================
# We'll simulate our simple treatment experiment many times,
# run a t-test each time, and see how often we detect the effect (p < 0.05).
# That proportion ≈ statistical power.

set.seed(1)

# ---- Function 1: Create a function that simulates one experiment and return its p-value ----
simulate_once_ttest <- function(n_per,
                                mean_control,
                                mean_treated,
                                sd_both) {
  control  <- rnorm(n_per, mean_control, sd_both)
  treated  <- rnorm(n_per, mean_treated, sd_both)
  t.test(treated, control)$p.value
}

# ---- Function 2: Create a function that repeats many experiments and estimates power ----
estimate_power_ttest <- function(n_per,
                                 mean_control,
                                 mean_treated,
                                 sd_both,
                                 reps,
                                 alpha = 0.05) {
  pvals <- replicate(reps,
                     simulate_once_ttest(n_per, mean_control, mean_treated, sd_both))
  power <- mean(pvals < alpha)
  return(power)
}

# ---- Apply functions to estimate power for your experiment ----
power_est <- estimate_power_ttest(n_per = 10,
                                  mean_control = 9,
                                  mean_treated = 11,
                                  sd_both = 2,
                                  reps = 2000, 
                                  alpha = 0.05)
power_est #what proportion of pvalues are less than your designated alpha? 

# ---- Explore how power changes with sample size ----

n_grid <- seq(5, 40, by = 5) #make a vector from 5 to 40, counting by 5's to represent your sample size

power_grid <- sapply(X = n_grid, function(n) 
  estimate_power_ttest(n_per = n,
                       mean_control = 9,
                       mean_treated = 11,
                       sd_both = 2,
                       reps = 1000))

power_tbl <- tibble(n_per = n_grid, power = power_grid)

ggplot(power_tbl, aes(x = n_per, y = power)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 3, color = "darkblue") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  labs(title = "Power vs. per-group sample size (t-test)",
       x = "Sample size per group", y = "Estimated power (proportion significant)") +
  theme_minimal()

# ---- Optional experiments ----
# Try changing:
#   - mean_treated (effect size)
#   - sd_both (variability)
#   - reps (precision of simulation)
#   - alpha (significance threshold)
# Then re-run and compare the estimated power.

