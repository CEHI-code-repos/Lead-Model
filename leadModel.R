# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Script name: leadModel
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Project name: 
#
#   Program description: Lead Model
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Data: 
#
#   Restrictions:
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Outline of program/analysis:
#
#:
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Author: M.C.(H.)Leong           				Date:  April 2018
#
#   Revisions (by/date): 
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Input files:
#      Fname		                                                    Ftype		Created by
#      
#      
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Output files:
#      Fname		                              Ftype	   	Notes
#      
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Remove objects Force garbage collection
rm(list = ls(all.names = TRUE))
gc()

# Install the necessary packages. 
# install.packages(c('foreign', 'sandwich', 'plm', 'foreign'))

# Script isn't designed for running in VRDE. It was running on my local
# machine. .libPaths() .libPaths('S:/Admin/IT/R')

library("sandwich")
library("plm")

# Lead Model
# Method: Weighted Least Squares
# Y: ln_bll
# X: season dummy(converted from month), cnty dummy, 
#    year_blt, hh_median_inc, pct_blk, pct_hisp, pct_pub_ass
# reference: http://alexrdouglas.com/tutorial-regression-in-stata-and-python/
# http://projects.iq.harvard.edu/files/gov2001/files/sesection_5.pdf
# https://cran.r-project.org/web/packages/clubSandwich/vignettes/panel-data-CRVE.html
# http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/
# the website above claimed that stata use HC1 to do the cluster

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Step 1 Lead model
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Step 1.1. Import .dbf file # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # #

dbfFilePath <- ""
dbfTracts <- read.dbf(dbfFilePath)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Step 1.2. Data processing # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # #

# New variable as season
monthToSeason <- function(monthVector) {
  for(month in monthVector) {
    if(month %in% c(12, 1:2)) {
      season <- c(season, "Winter")
    } else if(month %in% c(3:5)) {
      season <- c(season, "Spring")
    } else if(month %in% c(6:8)) {
      season <- c(season, "Summer")
    } else if(month %in% c(9:11)) {
      season <- c(season, "Fall")
    } else {
      print("The wrong value for month!")
      season <- c(season, NA)
    } 
  }
  return(season)
}
dataRead$season <- monthToSeason(dataRead${month})


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Step 1.x Modelling # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # #

#### Note
# data: dataset
# data$cluster_list: variable representing the cluster

# define model formula
modelFormula <- as.formula(ln_bll ~ year_blt + hh_median_inc +
                             pct_blk + pct_hisp + pct_pub_ass + 
                             season_dummies + cnty_dummies)

# plm is used for linea model needed clustering
leadModel <- plm(modelFormula, data = data, 
                 weights = 1/parcel_field, model = "pooling")

# adjust df
G <- length(unique(data$cluster_list))
N <- length(data$cluster_list)
dfa <- (G/(G - 1)) * (N - 1)/leadModel$df.residual
clusterCVcov <- dfa * vcovHC(leadModel, type = "HC1",
                             cluster = "group", adjust = T)
coeftest(leadModel, vcov = clusterCVcov)


