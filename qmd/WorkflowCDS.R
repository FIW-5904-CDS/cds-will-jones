
# set WD to parent directory. inside the folder qmd lives SourceFiles, references.bib, WorkflowCDS.R, and FinalProjectCDS1.qmd
setwd("C:\\Users\\wj436\\OneDrive\\Desktop\\cds-will-jones\\qmd") # I provided absolute path just to be safe


source("SourceFiles/Libraries.R") # load necessary geospatial libraries for analysis

# This script runs the spatial sampling of ~1e6 points in Virginia
# records associated attributes; NLCD, ecoregion, distance to nearest road 2000, distance to nearest road 2024
source("SourceFiles/land_pts.R")

# run the script that makes the figures for the final markdown doc
# script saves figures to qmd/ProjectFigures
source("SourceFiles/MakeFigures.R")

