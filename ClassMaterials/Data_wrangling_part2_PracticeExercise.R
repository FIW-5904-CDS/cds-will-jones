# EEOB590A - Data_wrangling part 2 practice exercise ------

# Part 1: Get set up ----------

## 1.1) Load libraries ----------

library(tidyverse)
library(forcats)

## 1.2) Read in data ----------
# From the tidy folder, read in the file on pollination you created after finishing last week's assignment

data <- read_csv('wrangling1output.csv')


## 1.3) Change name of columns -------
# "date traps out" should be "dateout" and and "date traps coll" sould be "datecoll"

data <- data %>% rename(dateout = `date traps out`) %>% rename(datecoll = `date traps coll`) %>% select(-`...1`)
colnames(data)

## 1.4) Change the class of each variable as appropriate ------
# Make variables into factors, numeric, character, etc. Leave the dates as is for now. 

data <- data %>% mutate(order = as.factor(order))


## 1.5) What format are the dates in? Change to date format ----

class(data$dateout)

# Part 2: Fix errors within cells ------

## 2.1) Fix the levels of island and site ------
# Maks sure all island and site names are in lowercase 
# Rename sites: forbigrid as forbig and racetrack as race

data$island <- tolower(data$island)
data$site <- tolower(data$site)

data$site <- plyr::revalue(data$site , c('forbigrid' = 'forbig', 'racetrack' = 'race'))

## 2.2) Do you see any other errors that should be cleaned up? -----
# Just good practice to do a final check on this. Insect orders should remain capitalized. 

# Part 3: Create new columns ------

## 3.1: Create a new column for the duration of time traps were out. ------
# Make sure new column is in the numeric class. 

data <- data %>% mutate(duration = as.integer(datecoll - dateout))

## 3.2: Create a new column with just the first 5 letters of the InsectOrder ------
# Name new column order_abbrev and make sure it is a factor 

data <- data %>% mutate(order_abbrev = as.factor(abbreviate(order, minlength = 5)))


# Part 4: Re-arrange levels of a variable and rearrange rows ------
## 4.1) Arrange levels of insectorder by average number of insects. ------ 
#this will let us create a graph later on of insect orders with the most common on one side and least common on the other side of the x-axis.

## 4.1) Arrange levels of insectorder by average number of insects ------ 

# First, compute average number of insects per order
order_means <- data %>%
  group_by(order) %>%
  summarize(mean_insects = mean(count, na.rm = TRUE)) %>%
  arrange(mean_insects)

# Now reorder the factor levels of 'order' based on this mean
data <- data %>%
  mutate(order = factor(order, levels = order_means$order))


## 4.2) Arrange entire dataset by the number of insects ------
# make these descending, so the greatest number is on row 1. 
## 4.2) Arrange entire dataset by the number of insects ------

data <- data %>%
  arrange(desc(count))


# Part 5: Print tidied, wrangled database ------
# name file "poll_long_tidy.csv" and put in tidy database

print(data)
