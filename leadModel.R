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
# http://math.furman.edu/~dcs/courses/math47/R/library/car/html/hccm.html
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
monthToSeason <- function(month) {
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
  return(season)
}
dataRead$season <- unlist(lapply(dataRead${month}, monthToSeason))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Step 1.x Modelling # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # #

#### Note
# data: dataset
# potential useful to use lapply for modeling each county
# data$cluster_list: variable representing the cluster

# copy from documentation:
# date: county tax assessor data, or census data
# response: Maximum blood lead level per child per tax parcel
# predictor: 
# Persons receiving public assistance (block group level)
# Median household income (block group level)
# African American persons (block level)
# Hispanic persons (block level)

# Model:
' *Prediction.(can be used as RR)'

' *relative risk is not meant to be predictive for actual blood lead levels of children residing at a particular residence. Rather, it should be interpreted as a mechanism for prioritizing lead poisoning prevention program resources and activities.'

' *explain the relationship between age of housing and BLL.'

' *Outcome was the natural log of blood lead levels. Model controlled for year in which the residence was built (from county tax assessor database), U.S. Census block-level % black and % Hispanic, U.S. Census block group-level household median income, % households receiving public assistance, and county of residence. In addition, we accounted for the season of blood lead screening
*Applied the vector of coefficients from the regression model at the individual blood lead level to predict the expected blood lead level for each parcel
*Regresion model was weighted by inverse of count of children per parcel (to ensure that parcels with multiple blood lead screens did not overly influence the analysis) and standard errors were corrected for block group-level clustering
*Parcels ranked by expected blood lead level in terms of the top 10%, next 10%, next 40%, and last 40% for priority categories
*Lead exposure risk is also mapped – this is the expected logged blood lead level at the parcel level'

# define model formula
modelFormula <- as.formula(ln_bll ~ year_blt + hh_median_inc +
                             pct_blk + pct_hisp + pct_pub_assis + 
                             season_dummies + cnty_dummies)

leadModeling <- function(dataset, modelFormula) {

  # plm is used for linea model needed clustering
  leadModel <- plm(modelFormula, data = dataset, 
                   weights = 1/parcel_field, model = "pooling")

  # adjust df
  G <- length(unique(dataset$cluster_list))
  N <- length(dataset$cluster_list)
  # note for clustering or not: 
  # coef not changed (point estimate is the same), but covar matrix changed. so CI, PI, t-statistics etc. changed.
  dfa <- (G/(G - 1)) * (N - 1)/leadModel$df.residual
  clusterCVcov <- dfa * vcovHC(leadModel, type = "HC1",
                               cluster = "group", adjust = T)
  coeftestResult <- coeftest(leadModel, vcov = clusterCVcov)
  predictResult <- predict(leadModel)
  predictResultPercentile <- quantile(predictResult, seq(5.5,95.5,5)/100, type=4)
  getPriority <- function(bllLn, bllLnPerc) {
	  if(bllLn > bllLnPerc[15]) {
	  return 1
	  } else if(bllLn > bllLnPerc[10] & bllLn <= bllLnPerc[15]) {
      return 2
	  } else if(bllLn > 0 & bllLn <= bllLnPerc[10]) {
      return 3
	  } else { return NA }
  }
  priorityResult <- sapply(predictResult, getPriority, var2=predictResultPercentile)
  outputList <- list(leadModel, coeftestResult, priorityResult)
  return outputList
}


