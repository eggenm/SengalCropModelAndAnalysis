library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)

library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(soiltexture)


######################## FUNCTIONS FOR COMMON SOIL OPERATIONS  ##############################
soil_texture_class<-function(data){
  
  ### This gets the USDA texture class using the soiltexture package using the aggregated first 60 cm
  
  my.text <- data.frame(
    "CLAY" = data$upper_60_clay_ave/10,
    "SILT" = data$upper_60_silt_ave/10,
    "SAND" = data$upper_60_sand_ave/10,
    "OC" = data$upper_60_soc_ave/100
  )
  soil_class<-TT.points.in.classes(
    tri.data = my.text[,],
    class.sys = "USDA.TT",
    PiC.type = "t"
  )
  return(soil_class)
}

get_averaged_soil_layers<-function(soil){
  # This function creates a weighted average of soil layers to 60cm
  # It adds these values and returns the altered dataframe, which is a bit frowned upon
  # by object oriented programming standards, but so it is.
  soil<-soil%>%select(c('ID','clay_0_5cm','clay_15_30','clay_30_60','clay_5_15c','clay_60_10','sand_0_5cm','sand_15_30','sand_30_60','sand_5_15c','sand_60_10','silt_0_5cm','silt_15_30','silt_30_60','silt_5_15c','silt_60_10','soc_0_5cm','soc_15_30c','soc_30_60c','soc_5_15cm','soc_60_100','region')
  )
  soil$upper_60_clay_ave<-(5*soil$clay_0_5cm+10*soil$clay_5_15c+15*soil$clay_15_30+30*soil$clay_30_60)/60
  soil$upper_60_silt_ave<-(5*soil$silt_0_5cm+10*soil$silt_5_15c+15*soil$silt_15_30+30*soil$silt_30_60)/60
  soil$upper_60_sand_ave<-(5*soil$sand_0_5cm+10*soil$sand_5_15c+15*soil$sand_15_30+30*soil$sand_30_60)/60
  soil$upper_60_soc_ave<-(5*soil$soc_0_5cm+10*soil$soc_5_15cm+15*soil$soc_15_30c+30*soil$soc_30_60c)/60
  return(soil)
}

get_soil<-function(){
  setwd('C:\\SenegalGIS\\senegal_aquacrop_model\\data') 
  soil<-read.table( 'AEZ_samples_with_soil3.csv', sep = ',', header = TRUE)
  soil<-get_averaged_soil_layers(soil)
  soil<-soil%>%filter(upper_60_clay_ave>0)
  ##There is one profile where the sum of textures = 100.1 which causes an error when estimating texture class
  soil$upper_60_sand_ave[which(soil$ID==3508)]=soil$upper_60_sand_ave[which(soil$ID==3508)]-1
  soil<-soil%>%filter(region %in% c('Bassin arachidier', 'Casamance', 'Senegal oriental'))
  soil$texture_class<-soil_texture_class(soil)
  return(soil)
}

############################################################################################







