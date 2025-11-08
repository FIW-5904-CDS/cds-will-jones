# FIW5594: Conservation Data Science
# 1 October 2025 
# Data wrangling part 1, practice script ----------

# We will be working with a real insect pan traps dataset that I've amended
# slightly in order to practice the skills from Monday.  
# The file is called "Data_wrangling_day1_pollination.xlsx" and it is 
# located in the data folder, and then in the raw folder within that. 

# 1) Load libraries -----
# you will need tidyverse and readxl

library(tidyverse)
library(readxl)

# 2) Read in data from the InsectData worksheet --------
setwd(r'{C:\Users\wj436\OneDrive\Desktop\CourseMaterials}')

prac_data <- read_excel('data/raw/Data_wrangling_day1_pollination.xlsx')
str(prac_data)

# 3) Rename columns --------
# Leave columns of insect orders with capital letters, but make all other 
# column names lowercase. 

prac_data <- prac_data |>
  rename_with(tolower, .cols = Island:`Top color - Bowl color`)
str(prac_data)

# Remove any spaces in column names. Change "location" to "site". 
library(janitor)

colnames(prac_data)
prac_data <- prac_data %>% clean_names() %>% rename(site = location)
colnames(prac_data)

# Change "tract" to "transect". 
prac_data <- prac_data %>% rename("transect" = 'tract')
# 4) Add missing data --------
# The people who entered the data did not drag down the island or location
# column to fill every row. Use code to fill in this missing data. 
# Double check to make sure this worked correctly. 

head(prac_data, 10)
prac_data <- prac_data %>% fill(island, site)
head(prac_data, 10)

# 5) Separate "Top color - Bowl color" into two different columns ------
# The first letter represents the top color and the second letter represents the
# bowl color. We do not need to save the original column. 

prac_data <- prac_data %>% separate(top_color_bowl_color, into = c('top_color', 'bowl_color'), sep = '-')
str(prac_data)

# 6) Use the complete function ----------
# Check if we have data for all 3 transects at each location. 
# Do not overwrite the poll data frame when you do this. 

prac_data_comp <- prac_data %>% complete(transect, site)
View(prac_data_comp)
# Which transects appear to be missing, and why? 

# 7) Unite island, site, transect into a single column -----
# Do not add spaces or punctuation between each part. Call this column uniqueID. 
# Keep the original columns too. 

prac_data <- prac_data %>% unite(uniqueid, c(island, site, transect), remove = FALSE)

# 8) Now, make this "wide" dataset into a "long" dataset ---------
# one column should include the insect orders, and one column the number of insects. 

bugs_long <- prac_data %>% pivot_longer(cols = -all_of(c('uniqueid','transect', 'island','site', 'top_color', 'bowl_color')), names_to = 'order',
  values_to = 'count')

# 9) Just to test it out, make your "long" dataset into a "wide" one and see if anything is different. -------

bugs_wide <- bugs_long %>%
  pivot_wider(names_from = 'order', values_from = 'count')

# Are you getting an error? Can you figure out why? 

# 10) Join the "InsectData" with the "CollectionDates" tab ---------
# Add a collection date for each row of InsectData. You'll need to read in the
# CollectionDates tab. Play around with the various types of 'mutating joins' 
# (i.e. inner_join, left_join, right_join, full_join), to see what each one does
# to the final dataframe, and note which one you think does the job correctly. 

dates <- read_excel('data/raw/Data_wrangling_day1_pollination.xlsx', sheet = 'CollectionDates')
head(dates)

t_join <- left_join(bugs_long, dates, by = c('island', 'site'))

# 11) Create a csv with the long dataframe -------
# dataframe should include collection dates
# put new csv in your data/tidy folder

t_join

write.csv(t_join, 'wrangling1output.csv')
