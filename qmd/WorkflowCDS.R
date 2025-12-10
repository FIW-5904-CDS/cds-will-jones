
# set WD to parent directory. inside the folder qmd lives SourceFiles, references.bib, WorkflowCDS.R, and FinalProjectCDS1.qmd
setwd("C:\\Users\\wj436\\OneDrive\\Desktop\\cds-will-jones\\qmd") # I provided absolute path just to be safe


source("SourceFiles/Libraries.R") # load necessary geospatial libraries for analysis

# This script runs the spatial sampling of ~1e6 points in Virginia
# records associated attributes; NLCD, ecoregion, distance to nearest road 2000, distance to nearest road 2024
source("SourceFiles/land_pts.R")

# run the script that makes the figures for the final markdown doc
# script saves figures to qmd/ProjectFigures
source("SourceFiles/MakeFigures.R")

# ANALYSIS
# (if we were to run this script start to finish , then all_data would already be an obect
# but for the purposes of this script, I just read it in from my local machine because it would
# take too long to run everytime)
setwd("E:\\01_PROJECTS\\01_Roads\\06_fixing_final_files")
all_data <- st_read('pairwise_data_07_28.gpkg')
all_data <- all_data %>% mutate(ratio = (dist_24 / dist_00)) %>% mutate(log_ratio = log10(ratio))

# check that data is tidy. each row is an observation
head(all_data)

# take a quick look at our data
print('summary statistics for distances to nearest road in year 2000:')
summary(all_data$dist_00)

print('summary statistics for distances to nearest road in year 2000:')
summary(all_data$dist_00)

# pivot this data to "long" format for easier visualization
long_data <- all_data %>%
  pivot_longer(
    cols = c(dist_00, dist_24, log_ratio),
    names_to = "measure_type", 
    values_to = "meters" 
  )

# take a subset of our data (100,000 rows) to make analysis manageable
# create a column 'difference' to measure the PAIRED differences between
# distances to each road network (2000 and 2024)
data <- all_data %>% sample_n(100000) %>% select(dist_00, dist_24) %>% 
    st_drop_geometry() %>%
    mutate(difference = dist_00 - dist_24)

# take a look at the data
summary(data$difference)

# plotting a histogram of the paired differences, we see that they are 0 heavy
# meaning most values are a change of 0 meters closer/farther to the nearest road
# between 2000 and 2024. this makes sense
data %>% ggplot(aes(x=difference)) +
  geom_histogram(bins = 150)

# we can plot a quantile-quantile plot to see if our data is normal (we know it's not),
# and/or if it is symmetric. symmetry is an assumption of wilcoxon signed rank test
qqp <- data %>% ggplot(aes(sample=difference)) +
  geom_qq(color = 'darkblue', size = 5, alpha = 0.2) +
  geom_qq_line() +
  theme_igray() +
  ggtitle('Q-Q Plot of Observed Differences Between Pairs and Theoretical Distribution ')+
  xlab('Theoretical') +
  ylab('Sample (m)')

qqp


# this block of code runs the permutation test
# it permutes, or "shuffles" each pair 5000 different ways
results = c()
for (i in 1:5000) {
  permutedData = sample(c(1,-1),100000,replace=T)*data$difference
  results = c(mean(permutedData),results)
}
hist(results,col='gray',main="Permutation Distribution",xlab="Simulated difference")
mean(results)
results <- as.data.frame(results)

# calculate our test statistic
# in this case the test statistic is the mean of the paired differences
# computed from our REAL SAMPLE
T_obs = mean(data$difference)
T_obs

# plot our data
results %>% ggplot(aes(x = results)) +
  geom_histogram(bins = 100, fill = "lightblue", color = 'black') +
  geom_vline(xintercept = T_obs, color = 'red', 'linetype'= 'dashed', size = 1.75 ) +
  theme_bw() +
  ggtitle('Permutation Sampling Distribution of Means of Paired Differences') +
  xlab('Mean of Paired Differences (m)') + 
  ylab('Count') +
  theme(axis.title = element_text())


# the following code performs bootstrap resampling to construct a confidence
# interval around our test statistic (mean of the paired differences)
obs_mean_diff <- mean((data$dist_00 - data$dist_24))

set.seed(123)  # for reproducibility
B <- 1000      # number of bootstrap replicates

boot_diff <- numeric(B)  # vector to store bootstrap results
n <- nrow(data)

for (b in 1:B) {
  
  # sample row indices with replacement
  idx <- sample(1:n, size = n, replace = TRUE)
  boot_sample <- data[idx, ]
  
  # compute means for this bootstrap sample
  mdiff <- mean((boot_sample$dist_00 - boot_sample$dist_24))
  
  # store the difference
  boot_diff[b] <- mdiff
}

# Look at the bootstrap distribution
boot_dist <- as.data.frame(boot_diff)

quantiles <- quantile(boot_diff, c(0.025, 0.975)) # compute 95% confidence interval
sd(boot_diff) # standard deviation of the bootstrapped sampling distribution is our standard error


# visualize our results
# we see that the value of our SAMPLE test statistic is very close to the mean
# of the 1000 bootstrap calculated test statistics
bootstrap_hist <- boot_dist %>% 
  ggplot(aes(x = boot_diff)) +
  geom_histogram(bins = 50, fill = 'lightblue', color = 'black') +
  geom_vline(xintercept = mean(boot_diff), color = "black", linewidth = 1, linetype = 'dashed') +
  geom_vline(xintercept = quantiles[1], color = 'blue', linetype ='dashed', linewidth =1) +
  geom_vline(xintercept = quantiles[2], color = 'blue', linetype ='dashed', linewidth = 1) +
  theme_bw() +
  ggtitle('Bootstrap Distribution = Test Statistic: Mean of Paired Differences') +
  xlab('Meters') + 
  ylab('Count') +
  theme(axis.title = element_text())

bootstrap_hist


