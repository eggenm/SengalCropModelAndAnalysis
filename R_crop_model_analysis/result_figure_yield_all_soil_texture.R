library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(soiltexture)


setwd('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\')

###########################################################################

###########       FUNCTIONS   #############################

soil_texture_class<-function(data){
  my.text <- data.frame(
    "CLAY" = data$upper_60_clay_ave/10,
    "SILT" = data$upper_60_silt_ave/10,
    "SAND" = data$upper_60_sand_ave/10,
    "OC" = data$upper_60_soc_ave/100
  )
  temp<-TT.points.in.classes(
    tri.data = my.text[,],
    class.sys = "USDA.TT",
    PiC.type = "t"
  )
  return(temp)
}

get_resample_means=function(mydata, zone){
  means<-NULL
  j=1
  n<-600
  textures<-c('Lo', 'SaClLo', 'ClLo', 'Lo', 'SaClLo', 'ClLo')
  rows<-3600
  if(zone=='Senegal oriental'){
    textures<-c('SaLo', 'SaClLo', 'ClLo','SaLo', 'SaClLo', 'ClLo')
    
  }else if(zone=='Bassin arachidier'){
    textures<-c('SaLo', 'SaClLo', 'LoSa', 'SaLo', 'SaClLo', 'LoSa')
  }else if(zone == 'Zone sylvopastorale'){#ZS only has 2 significant textures in this data set
    n<-400
    rows<-2400
    textures<-c('SaLo', 'SaClLo', 'SaLo', 'SaClLo', 'SaLo', 'SaClLo')
  }
  means <- data.frame(matrix(ncol = 3, nrow = rows))
  #provide column names
  colnames(means) <- c('yield', 'texture',  'site' )
  
  for( x in list(
    mydata%>%filter(texture_class==textures[1] ),
    mydata%>%filter(texture_class==textures[2] ),
    mydata%>%filter(texture_class==textures[3] ),
    mydata%>%filter(texture_class==textures[4] ),
    mydata%>%filter(texture_class==textures[5] ),
    mydata%>%filter(texture_class==textures[6] )
  )
  ){
    for(i in 1:n){
      means$yield[j] = mean(sample(x$yield, 30, replace = FALSE, prob = NULL))
      means$texture[j] = x$texture_class[1]
      means$site  = x$zone[1]
      j=j+1
    }
  }
  return(means)
}

##################################################################################


##C:/SenegalGIS/crop_model_result/fromsharecomputer
summary_median<-read.csv(file = 'fullrun_summary_median.csv')
summary_median$texture_class<-soil_texture_class(summary_median)
summary_median$sand_clay_ratio<-summary_median$upper_60_sand_ave/summary_median$upper_60_clay_ave
summary_median<-summary_median%>%filter( ( zone=='Casamance' & texture_class %in% c('Lo', 'SaClLo', 'ClLo') ) | 
                                           ( zone=='Senegal oriental' & texture_class %in% c('SaLo', 'SaClLo', 'ClLo') ) |
                                           ( zone=='Bassin arachidier' & texture_class %in% c('SaLo', 'SaClLo', 'LoSa') )  )

year_aves<-summary_median%>%group_by(soil_id)%>%summarise(yield=mean(yield), sand_clay_ratio=mean(sand_clay_ratio), zone=min(zone), texture_class=min(texture_class))
mean_sand_clay_ratio<-year_aves%>%group_by(zone)%>%summarise(mean_sand_clay_ratio=mean(sand_clay_ratio))
year_aves<-merge(year_aves, mean_sand_clay_ratio, by='zone')
year_aves$normalized_sand_clay_ratio<-year_aves$sand_clay_ratio/year_aves$mean_sand_clay_ratio
plot(year_aves$sand_clay_ratio, year_aves$yield)
plot(year_aves$normalized_sand_clay_ratio, year_aves$yield)
tile<-ggplot(year_aves, aes(normalized_sand_clay_ratio, yield, shape=zone, color=texture_class)) + geom_point(size = 2) +
  labs(y="Yield(kg/ha)", x='Sand-to-clay ratio normalized by zonal mean')+
  ggtitle("Yield and soil sand-to-clay ratio")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_shape_manual(values=c(15, 16, 17) , name = "Agroecozones" )    +
  scale_color_discrete( name = 'Soil Texture' , labels=c('Clay Loam', 'Loam', 'Loamy Sand', 'Sandy Clay Loam', 'Sandy Loam'))


