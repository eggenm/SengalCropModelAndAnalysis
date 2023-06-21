# FUNCTION TO CREATE A DATATABLE WITH ALL MODEL OBSERVATIONS MERGED WITH 
#   WEATHER AND SOIL OBSERVATIONS

# Load necessary libraries
library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)



############################################################################
# Load custom R scripts
source('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\FINAL_code_output\\R_crop_model_analysis\\soil_summary.R')
source('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\FINAL_code_output\\R_crop_model_analysis\\weather_summary.R')
source('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\FINAL_code_output\\R_crop_model_analysis\\yield_resampling_functions.R')
###################################################
# READ AND SUMMARIZE AQUACROP RESULT FILES

# Set working directory
setwd('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\FINAL_code_output\\R_crop_model_analysis\\input\\')

# Read millet crop model results data
millet_summary <- read.csv("millet_FINAL_fullrunV2.csv")

# Convert harvest date column to Date format
millet_summary$Harvest.Date..YYYY.MM.DD. <- as.Date(millet_summary$Harvest.Date..YYYY.MM.DD.)
millet_summary$season_yield<-millet_summary$Yield..tonne.ha.
# Extract year from the date
millet_summary$year <- year(millet_summary$Harvest.Date..YYYY.MM.DD.)


#############################
# MERGE SOIL and Weather DATASETS

# Get soil data
soil <- get_soil()

# Merge millet summary and soil data based on soil ID
millet_factors <- merge(millet_summary, soil, by.x = 'soil_id', by.y = "ID")

month_weather_summary<-collect_weather_inputs()
june_summary<-month_weather_summary%>%filter(month==6)
colnames(june_summary)<-c("june"  , "year"  ,  "zone_id" ,"june_precip" , "june_av_max" , "june_av_min" , "june_av_mean", "zone")

july_summary<-month_weather_summary%>%filter(month==7)
colnames(july_summary)<-c("july"  , "year"  ,  "zone_id" ,"july_precip" , "july_av_max" , "july_av_min" , "july_av_mean", "zone")

august_summary<-month_weather_summary%>%filter(month==8)
colnames(august_summary)<-c("august"  , "year"  ,  "zone_id" ,"august_precip" , "august_av_max" , "august_av_min" , "august_av_mean", "zone")

sept_summary<-month_weather_summary%>%filter(month==9)
colnames(sept_summary)<-c("sept"  , "year"  ,  "zone_id" ,"sept_precip" , "sept_av_max" , "sept_av_min" , "sept_av_mean", "zone")

# Merge millet factors with weather summaries for different months
millet_factors <- merge(millet_factors, june_summary, by.x = c('site_id', 'year'), by.y = c('zone_id', 'year'))
millet_factors <- merge(millet_factors, july_summary, by.x = c('site_id', 'year'), by.y = c('zone_id', 'year'))
millet_factors <- merge(millet_factors, august_summary, by.x = c('site_id', 'year'), by.y = c('zone_id', 'year'))
millet_factors <- millet_factors %>% select(-one_of('zone.x', 'zone.y'))
millet_factors <- merge(millet_factors, sept_summary, by.x = c('site_id', 'year'), by.y = c('zone_id', 'year'))
millet_factors$precip<-millet_factors$june_precip+millet_factors$july_precip+millet_factors$august_precip+millet_factors$sept_precip

# Set working directory
setwd('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\FINAL_code_output\\R_crop_model_analysis\\input\\')

# Write millet factors data to a CSV file
write.csv(millet_factors, file = 'millet_factors_fullrun.csv')

summary<-millet_factors%>%filter(zone %in% c('Bassin arachidier', 'Casamance', 'Senegal oriental') )
summary_median<-get_median_yearly_observations(summary)
summary_median$yield<-round(100*summary_median$season_yield,2)

write.csv(summary_median, file = 'fullrun_summary_median.csv')
