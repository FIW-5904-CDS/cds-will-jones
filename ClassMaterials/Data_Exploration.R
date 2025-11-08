#Data Exploration and Visualization ------
#EEOB590A Fall 2021

# Topics in this script --------
# 1. Identify research question, variables of interest
# 2. Load libraries and dataset
# 3. Subset to the dataset you will use for the analysis 
# 4. Get to know your dataset with tables and graphs 
## 4.1. Tables
## 4.2. Graphs using ggplot
## 4.3. Skim/Create Report
# 5. Systematically explore the dataset
## 5.1. Identify Outliers
## 5.2. Zero Inflation
## 5.3. Collinearity
## 5.4. Y vs X's relationship
## 5.5. Sample Independence
## 5.6. Standardizing predictors
# 6. Summarize findings

# 1: Identify research question, variables of interest -------

# Potential Research Questions: 
# Q1: Do spiders build smaller webs when birds are present? 
# If so, then web size should be smaller on Saipan than on Guam. (note the N=1 problem here).
# Q2: Does websize vary depending on whether spider was transplanted or found in the area? 
# Q3: Does duration web persists depend on island or netting or a combination of the two? 

# Variables of Interest
# Response: websize (continuous), duration (continuous)
# Predictor: island (categorical), native (categorical), netting (yes/no)
# Random effect: site (categorical)

# 2: Load libraries and dataset --------
library(tidyverse)
library(skimr) # just to check out a function
library(DataExplorer) # just to check out a function

# Load dataset
transplant<-read.csv("data/tidy/transplant_tidy_clean.csv", header=T)
nrow(transplant) #91 rows

# 3: Subset to the dataset you will use for the analysis ---------

## 3.1: Subset because analysis uses portion of data --------------
# Sometimes your analysis is only for a portion of the data (e.g. one species). # If so, you should subset before doing data exploration. 
# For example, we could use the subset that was transplanted rather than webs 
# that were observed in place. If we did this, we would use filter as shown 
# below, and then do all data exploration using this filtered dataset. 

truetrans <- transplant %>%
  filter(native == "no")

nrow(truetrans)#62 rows 

transplant <- transplant %>%
  mutate(across(c(island, site, native, netting, webpres, spidpres),as.factor))

## 3.2: Subset because of NA's --------------
# Identify important missing values - should be sure to explore the dataset that you will be analyzing, which may mean removing rows with NA's. 
# Do you have NA's in your dataset? Where? 
# Missing response means you cannot use that row in analysis
# Missing value in one of your predictors will typically drop that entire row from analysis

summary(transplant) 

(transplantNA <- transplant %>%
  complete(island, site, duration))

transplantNA %>%
  drop_na() # drops any rows with an NA in any column
    
transplantNA %>%
  drop_na(websize, duration, island, native, netting, site) #drop any rows with NA's in the specified columns. 

#can also use filter to see which rows have na's
transplantNA %>%
  filter(if_any(everything(), is.na))
  

# 4: Get to know your dataset with tables and graphs ---------

## 4.1: Explore using tables ---------
# table() gives a count of number of rows with a combination of variables
with(transplant, table(site, netting)) 
#### mtr doesn't have any with netting, and only has 2 without. 

# ftable() makes a table with three variables more visually appealing
with(transplant, ftable(island, site, netting))

# dplyr package functions: count(), summarize(), and arrange()
transplant %>%
  count(site, netting)

transplant %>%
  group_by(island, site) %>%
  count(netting)

transplant %>% 
  group_by(island, site, netting) %>% 
  summarize (mean_web = mean(websize)) 

transplantord <- transplant %>% 
  arrange(island, duration)  # Orders rows by values of a column (low to high).

## Your Turn: play around - what else might you want to summarize? 
transplant %>%
  group_by(island, site, duration) %>%
  summarise(avg_web = mean(websize),
            sd_web = sd(websize, na.rm = T)) %>%
  arrange(desc(duration))

transplant %>%
  group_by(island, site) %>%
  count(spidpres, webpres)

transplant %>%
  group_by(island) %>%
  summarize(maxweb = max(websize)) 

transplant %>%
  group_by(native) %>%
  summarise(avgweb = mean(websize))

transplant %>%
  group_by(island, site, netting) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

transplant %>%
  group_by(island, site) %>%
  summarize(mean_wa = mean(webarea),
            sd_wa = sd(webarea),
            max_wa = max(webarea),
            min_wa = min(webarea)) ## looking at basic descriptive statistics for web area and range/variation at each sampling site

## 4.2: Introduction to ggplot --------
# Resources
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/

# General structure: ggplot2(dataset, aes(x, y)) + geom()
# Part 1: dataset - can subset using square brackets if needed
# Part 2: aes = aesthetic, means "something you can see"
# Part 3: geom's: http://sape.inf.usi.ch/quick-reference/ggplot2/geom. A plot must have at least one geom; there is no upper limit.

# histogram - to look at variation within continuous variables
durationhist <- ggplot(data = transplant, aes(duration))+
  geom_histogram()

durationhist

# barplot  - to count number of rows per category of a variable
ggplot(transplant, aes(site))+
  geom_bar()

# boxplot - to summarize variation in continuous variable across categorical variables
ggplot(transplant, aes(netting, duration))+
  geom_boxplot() #plot response and predictors, continuous data

# create different boxplots for each island
ggplot(transplant, aes(netting, duration, color=island))+
  geom_boxplot() 

# geom_point for two continuous variables - scatterplot
ggplot(transplant, aes(websize, duration)) +
  geom_point()

# add facet_grid to show other variables
ggplot(transplant, aes(websize, duration))+
  geom_point()+
  facet_grid(netting~island)

# Your Turn: play around - graph the same relationship you used table or summarize to explore in the last section. 
ggplot(transplant, aes(webpres, fill = spidpres)) +
  geom_bar()

transplant %>%
  group_by( duration) %>%
  summarise(avg_web = mean(websize),
            sd_web = sd(websize, na.rm = T)) %>%
  arrange(desc(duration)) %>%
  ggplot(aes(x = duration, y = avg_web)) +
  geom_col( color = "black", fill = "grey70") +
  geom_errorbar(aes(x = duration, ymin = avg_web - sd_web, ymax = avg_web + sd_web),
                width = .25) +
  theme_bw()

## 4.3: Skim and Create Report -----
# try these two functions, from the skimr and DataExplorer packages
skim(transplant)
create_report(transplant) #takes a bit to run, not all of it is relevant

# 5: Systematically explore the dataset ---------------

## 5.1:  Identify Outliers  ---------
# Plot all continuous response and predictors to check for outliers
# useful paper: https://link.springer.com/content/pdf/10.1007/s10531-018-1602-2.pdf 
# Use histogram or Cleveland dotplot or boxplot

ggplot(transplant, aes(websize))+
  geom_histogram()

dotchart(transplant$websize, labels = transplant$site) #Cleveland Dotplot

ggplot(transplant, aes(x = websize, y= reorder(site, websize))) +
  geom_point(size = 3) #ggplot version of a Cleveland dotplot

ggplot(transplant, aes(websize)) +
  geom_boxplot()

ggplot(transplant, aes(duration))+
  geom_histogram()

dotchart(transplant$duration)

#### no major outliers in Y -websize or duration, all X's are categorical

## 5.2:	Examine Zero inflation Y --------
# #For count data, mostly
# If >25% of response values are 0, may have zero-inflated data. Will need to use zero-inflated model approach. 

#### Not applicable for websize, because response is continuous
#### duration doesn't have any zero's so not an issue here. 


## 5.3:	Check for collinearity in X's  ------
# collinearity is when predictor variables are highly correlated
# Plot each continuous predictor against each other 

#### since our predictors are categorical, we can't test this directly

## 5.4: Look at relationships of Y vs X’s -----
# Check if variances are homogenous for each X value
# Check if relationships are linear
# Plot response against each predictor and random effect. 
ggplot(transplant, aes(native, websize, color=island))+
  geom_boxplot()

#### maybe less variance on Saipan than on Guam, but nothing stands out as terrible. 

ggplot(transplant, aes(site, websize))+
  geom_boxplot() #mtr1 not sampled as well, has less variance

## 5.5: Check for independence of samples ----------
# Is there a pattern across time or space that is not incorporated in the model? 
ggplot(transplant, aes(native, websize, color=island))+
  geom_boxplot()+
  facet_grid(.~site)

#### But nothing really stands out in terms of site-level effects. NBLAS higher than Anao, 
#### anao doesn't have "yes" native spiders, MTR doesn't have "no" native spiders

# Are there other potential factors that reduce independence of Y’s? 
#### timing is similar, can't think of anything else that might matter. 

## 5.6 - Do you have sufficient data? --------
# As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# 6 parameters max here

# Do all levels of island have adequate samples of at each level of native? 	Examine interactions
# Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(transplant, table(island, native)) #have all combinations here. 
#if samples are unequal, can't use anova(model)

## good = balanced, fine = unequal sample sizes, ugly = one or more combination is missing

# Check across sites: 
with(transplant, ftable(island, native, site))

#### well, we only sampled at 2 sites on Guam, and three sites on Saipan, and we don't have all levels of native for all sites. Need to be careful about how we use site in the model. 


## 5.7: Standardize continuous predictors (if necessary) -----------
# Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

#### in this situation, not necessary, bc no continuous predictors

# if you needed to do it, you would use this code: 
transplant <- transplant %>%
  mutate(websize_cent = scale(websize))

# 6: Summarize your findings ----------
# It is good practice to add notes to your code with your assessment

#### no obvious outliers
#### can move forward with analysis looking at web size relative to island and native. 
#### there is a lot of variability - is there something I might have measured related to web size that is not the thing I'm testing? 
