## Description

This project is using a variety of geospatial methods in R (sf package) and QGIS to build upon the work of a previous ecology paper from 2003.
## General Info

1. Proposed Title: How Far to the Nearest Road? Geospatial Approach to Road 
Network Analysis 

2. Likely coauthors: Ben Jantzen, Department of Philosophy and Department of 
Computer Science; Yang Shao, Department of Geography

3. Proposed journal (1st choice): Frontiers in Ecology and the Environment

4. Proposed journal (backup): Computers, Environment and Urban Systems


The overarching question of this paper is: For a large set (~50 million) of randomly 
distributed points, how has the distance to the nearest road changed between 2000 and 2024 
in the US? 

Which is important/interesting/unresolved because: 
a. A paper titled “How far to the nearest road?” was published in Frontiers in Ecology 
and the Environment in 2003. We are attempting to measure the change in 
distribution over the past ~20 years. 

b. Data quality has increased since the past study was published, and we can take 
advantage of these new/improved technologies. For example, USGS 3dhp 
hydrological data, more positionally accurate TIGER/Line data, high performance 
computing, etc. 

c. We are taking advantage of a continuous random distribution of measured points, 
rather than a grid-based approach in the original paper. 

d. Similar to above, we are going to analyze a more precise continuous distribution that 
is easier to apply statistical tests for shift in distribution, rather than a binned 
distribution as used in the previous study. 

To answer this question/explore this topic, I addressed the following objectives:

a. Starting with Virginia, uniformly randomly sampled 1 million points across the state. 
Using historical (2000) TIGER/Line road data from the US Census Bureau, 
measured the Euclidian distance from each point to the nearest road segment. Then 
applied the same process using an updated (2024) roads dataset. 

b. Plotted probability density distributions of each paired distribution to compare shift 
in average distance from the nearest road. 

c. Analyzed how these shifts and distance measurements vary between certain groups. 
Attributes we are analyzing include NLCD landcover data and EPA ecoregions. 

I addressed these objectives: (use list/bullet points below)

a. In Virginia, while the plan is to upscale to the contiguous United States

b. With the following focal/model species/model system: randomly sampled points and 
road network data.

c. And the following approaches: Measuring their associated distance from the nearest 
road in two time periods (2000 and 2024).

6. Each row of data in my dataset is a: it varies between numeric (distance measurements are 
float values) and string values (ex, name of ecoregion). 

7. For my analysis, I want to test: shift in distance to the nearest road between two time periods 

8. My response (y-axis) variable is: NA

9. My predictors (x-axis/colors/shapes on the graph) are: NA 

10. I replicated this across multiple: groups to test to see how the distribution has shifted 
differently for different landscape attributes, like NLCD landcover classes and EPA 
ecoregions. Ex: has a certain area (forest, pasture, light development) been impacted more by 
the development of roads in the past 20 years

11. I think I will need to analyze these data using a Wilcoxon signed rank test? I think this 
because it is two paired non-normal distributions. 

12. I anticipate I will get a final figure(s) that will look like this [sketch one or more figures 
below that you could imagine being part of the final paper] 

