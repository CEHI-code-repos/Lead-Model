# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Script name: lead_model.R
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Project name: 
#
#   Program description: Lead Risk Model
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Data: 
#
#   Restrictions: Reasonable year built data and lead sample test year data-- Specifically, 
#                 we made sure not to include kids whose sample test date was earlier than 
#                 the year in which the residence was constructed
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Outline of program/analysis: Prioritize the needed  parcel 
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Author: M.C.(H.)Leong           				Date:  April 2018
#
#   Revisions (by/date): M.C.(H.)Leong/May 2018
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

#assign one new library and keep other libraries
.libPaths(c(.libPaths(), "S:/Admin/IT/R")) 
#check libraries
.libPaths()


# Install the necessary packages. 
# install.packages(c('foreign', 'sandwich', 'plm', 'foreign'))

# install the required packages from a local source
install.packages("S:/Admin/IT/R/plm_1.6-6.zip", repos=NULL, type="source")
install.packages("S:/Admin/IT/R/sandwich_2.4-0.zip", repos=NULL, type="source")

library("sandwich")
library("plm")
library("foreign")
library("dplyr")

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
# # # Step 1.1. Import .dta file # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # #

dataPath <- "S:/Workspaces/Henry/LeadModel/screens_43counties.dta"
dataRead <- read.dta(dataPath)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Step 1.2. Data cleaning & processing # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # #

# Clean up the raw dataset
cleanData <- function(dataset) {
  cleanDataSet <- dataset %>% 
    #select() %>%
    filter(test_yr<=2003,
           yrblt>=1500,
           yrblt<=test_yr,
           test_age>=0.75,
           test_age<=2.99,
           lead!=0,
           lead<=300,
           maxchpar!=0) %>%
    mutate(test_mon=as.numeric(test_mon)) 
  return(cleanDataSet)
}

# you can use range(dataReadClean${var_name}) to check the data
dataReadClean <- cleanData(dataRead)

# New variable as season
monthToSeason <- function(month) {
  if(month %in% c(12, 1:2)) {
    return("Winter")
  } else if(month %in% c(3:5)) {
    return("Spring")
  } else if(month %in% c(6:8)) {
    return("Summer")
  } else if(month %in% c(9:11)) {
    return("Fall")
  } else {
    print("The wrong value for month!")
    return(NA)
  } 
}

dataReadClean$season <- unlist(lapply(as.numeric(dataReadClean$test_mon), monthToSeason))

# recode multiple variables (county) into one variable
# elegant and fast way
dataReadClean$county <- names(dataReadClean[,c(29:71)])[apply(dataReadClean[,c(29:71)] == 1, 1, which)]

# change all the beginning of county name as captial, it will take a while
dataReadClean$county <- unlist(lapply(dataReadClean$county, tools::toTitleCase))

# function to mark the individual whose is bll>=10
leadToEbll <- function(leadIndi) {
  if(leadIndi>=10) {
    return(1)
  } else {
    return(0)
  }
}

dataReadClean$ebll <- unlist(lapply(dataReadClean$lead, leadToEbll))

# get the log(bll) and filter out those smaller than 1
dataReadClean$ln_bll <- unlist(lapply(dataReadClean$lead, function(x) return(log(max(x,1)))))

# see the sample size in each county 
# and how many sample with larger than 1 ebll each county
# can't use tidyverse becasue it is not installed on VRDE
summaryebllDf<- dataReadClean %>%
  group_by(county) %>%
  summarise(countCounty=n(), total_ebll=sum(ebll))

# see sample size for each parc_id
summaryCountLeadPid <- dataReadClean %>%
  group_by(parc_id) %>%
  summarise(countParc_id=n())

# merge these two summerise df back to the original df
dataReadClean <- dataReadClean %>%
  left_join(summaryebllDf, by="county") %>%
  left_join(summaryCountLeadPid, by="parc_id")

# factorize season, month and county
dataReadClean <- dataReadClean %>%
  mutate(test_mon=factor(test_mon),
         season=factor(season),
         county=factor(county)) %>%
  arrange(parc_id)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Step 1.2 Modelling # # # # # # # # # # # # # # # # # # #
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

# relevel variable season and set Winter as reference
dataReadClean$season <- relevel(dataReadClean$season, ref="Winter")


# define model formula without intercept
# Model:
modelFormula <- as.formula(ln_bll ~ yrblt + kg_hmdin +
                             kb_p_blk + kb_p_hisp + kgp_puba + 
                             county + season - 1)

# plm is used for linea model needed clustering
# somehow it can't be used for a large dataset
# becasue it will convert the original dataframe to pdata.frame, which can't handle large sample
leadModel <- lm(modelFormula, data=dataReadClean, weights = 1/countParc_id)
summary(leadModel)
# coefficient is the same as stata

predictResult <- predict(leadModel)
head(predictResult, 20)
# the first 20 rows are the same as the result in stata

tail(predictResult, 20)
# the last 20 rows are the same as the result in stata

# merge the result to the original df
dataReadClean$est_ln_bll <- predictResult
dataReadClean$est_bll <- exp(predictResult)

# used for each county
#predictResultPercentile <- quantile(cra_2003_yb_census6_result_clean$est_ln_bll, seq(5,95,5)/100)
# check if it is the same as stata

prioritizeFromEstlnbll <- function(x, predictResultPercentile) {
  if(x>predictResultPercentile[15]) {
    return(1)
  } else if(x<=predictResultPercentile[15] & x>predictResultPercentile[10]) {
    return(2)
  } else if(x<=predictResultPercentile[10] & x>=0) {
    return(3)
  } else {
    return(NA)
  }
}


# Durham
dataReadCleanDurham <- dataReadClean %>%
  filter(county=="Dur")
predictResultPercentileDurham = quantile(dataReadCleanDurham$est_ln_bll, seq(5,95,5)/100)

dataReadCleanDurham$priority <- unlist(lapply(dataReadCleanDurham$est_ln_bll,
                                                           prioritizeFromEstlnbll,
                                                           predictResultPercentile=predictResultPercentileDurham))

# result for Durham
write.csv(dataReadCleanDurham, file="S:/Workspaces/Henry/LeadModel/result_Durham.csv")

###### Testing the model for cra, june
cra_2003_yb_census6 <- read.dta("S:/Projects/Unfunded/Lead/Replication_NC/LeadModel_IOG_18Co_EHP/LeadModel_Statewide_Replication_43Co/Data/Parcel_43Co_Shapefile/cra_2003_yb_census6.dta")
colnames(cra_2003_yb_census6)

# rename yr_built to yrblt
colnames(cra_2003_yb_census6)[1] <- "yrblt"

# stata style, we don't need it in R
'cra_2003_yb_census6[,names(dataReadClean[,c(29:71)])] <- 0
cra_2003_yb_census6$crav <- 1
cra_2003_yb_census6[,c("spring", "fall")] <- 0
cra_2003_yb_census6[,"summer"] <- 1'

# county name is Carv, season is summer
cra_2003_yb_census6$county <- factor("Crav", levels = levels(dataReadClean$county))
cra_2003_yb_census6$season <- factor("Summer", levels = levels(dataReadClean$season))

cra_2003_yb_census6$est_ln_bll <- predict(leadModel, cra_2003_yb_census6)

cra_2003_yb_census6_result_clean <- cra_2003_yb_census6 %>%
  mutate(est_bll=exp(est_ln_bll), rank_bll=rank(est_ln_bll)) %>%
  filter(!is.na(est_ln_bll), est_ln_bll!=0, yrblt >= 1500, !is.na(yrblt)) 


predictResultPercentile <- quantile(cra_2003_yb_census6_result_clean$est_ln_bll, seq(5,95,5)/100)
# check if it is the same as stata

prioritizeFromEstlnbll <- function(x, predictResultPercentile) {
  #predictResultPercentile <- quantile(est_ln_bll, seq(5,95,5)/100)
  if(x>predictResultPercentile[15]) {
    return(1)
  } else if(x<=predictResultPercentile[15] & x>predictResultPercentile[10]) {
    return(2)
  } else if(x<=predictResultPercentile[10] & x>=0) {
    return(3)
  } else {
    return(NA)
  }
}

cra_2003_yb_census6_result_clean$priority <- unlist(lapply(cra_2003_yb_census6_result_clean$est_ln_bll,
                                                           prioritizeFromEstlnbll,
                                                           predictResultPercentile=predictResultPercentile))

cra_2003_yb_census6_result_clean %>%
  group_by(priority) %>%
  summarise(n())
# correct!

######### Below is for changing the model as cluster
######### However, for the usage of the lead model, it isn't necessary
# adjust df
G <- length(unique(dataset$cluster_list))
N <- length(dataset$cluster_list)
# note for clustering or not: 
# coef not changed (point estimate is the same), but covar matrix changed. so CI, PI, t-statistics etc. changed.
dfa <- (G/(G - 1)) * (N - 1)/leadModel$df.residual
clusterCVcov <- dfa * vcovHC(leadModel, type = "HC1",
                             cluster = "group", adjust = T)
coeftestResult <- coeftest(leadModel, vcov = clusterCVcov)
getPriority <- function(bllLn, bllLnPerc) {
  if(bllLn > bllLnPerc[15]) {
    return(1)
  } else if(bllLn > bllLnPerc[10] & bllLn <= bllLnPerc[15]) {
    return(2)
  } else if(bllLn > 0 & bllLn <= bllLnPerc[10]) {
    return(3)
  } else { return(NA) }
}


priorityResult <- sapply(predictResult, getPriority, var2=predictResultPercentile)
outputList <- list(leadModel, coeftestResult, priorityResult)
return(outputList)



