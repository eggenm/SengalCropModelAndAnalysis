##############################
##
##  The most important part of this is the creation of monthly summaries for growing season months
##     that are used in other scripts
##  Other than that this can create some figures for weather visualization
##
library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
#########################################################
######## FUNCTIONS

collect_weather_inputs<-function(){
  setwd('C:\\SenegalGIS\\senegal_aquacrop_model\\weather\\aquacrop_python')
  basin_arach__weather<-read.csv('BassinArachidier_weather_from_python.csv')
  basin_arach__weather$zone_id<-6
  basin_arach__weather$year<-year(basin_arach__weather$Date)
  basin_arach__weather$month<-month(basin_arach__weather$Date)
  basin_arach__weather$mean_temp=(basin_arach__weather$MinTemp+basin_arach__weather$MaxTemp)/2
  basin_arach__weather_month_summary<-basin_arach__weather%>%filter(month %in% c(6,7,8,9))%>%group_by(month, year, zone_id)%>%summarise(precip=sum(Precipitation),av_max=mean(MaxTemp), av_min=mean(MinTemp), av_mean=mean(mean_temp) )
  basin_arach_weather_year_summary<-basin_arach__weather_month_summary%>%group_by(year, zone_id)%>%summarise(precip=sum(precip),av_max=mean(av_max), av_min=mean(av_min), av_mean=mean(av_mean) )
  
  basin_arach__weather$Week<-week(basin_arach__weather$Date)
  basin_arach__weather_week_summary<-basin_arach__weather%>%group_by(Week, year, zone_id)%>%summarise(precip=sum(Precipitation),av_max=mean(MaxTemp), av_min=mean(MinTemp), av_mean=mean(mean_temp), refET=sum(ReferenceET) )
  basin_arach__weather_averaged_week<-basin_arach__weather_week_summary%>%group_by(Week, zone_id)%>%summarise(precip=mean(precip),av_max=mean(av_max), av_min=mean(av_min), av_mean=mean(av_mean), refET=mean(refET) )
  
  casamance_weather<-read.csv('Casamance_weather_from_python.csv')
  casamance_weather$zone_id<-3
  casamance_weather$year<-year(casamance_weather$Date)
  casamance_weather$month<-month(casamance_weather$Date)
  casamance_weather$mean_temp=(casamance_weather$MinTemp+casamance_weather$MaxTemp)/2
  casamance_weather_month_summary<-casamance_weather%>%filter(month %in% c(6,7,8,9))%>%group_by(month, year, zone_id)%>%summarise(precip=sum(Precipitation),av_max=mean(MaxTemp), av_min=mean(MinTemp), av_mean=mean(mean_temp) )
  casamance_weather_year_summary<-casamance_weather_month_summary%>%group_by(year, zone_id)%>%summarise(precip=sum(precip),av_max=mean(av_max), av_min=mean(av_min), av_mean=mean(av_mean) )
  casamance_weather$Week<-week(casamance_weather$Date)
  casamance_weather_week_summary<-casamance_weather%>%group_by(Week, year, zone_id)%>%summarise(precip=sum(Precipitation),av_max=mean(MaxTemp), av_min=mean(MinTemp), av_mean=mean(mean_temp), refET=sum(ReferenceET) )
  casamance_weather_averaged_week<-casamance_weather_week_summary%>%group_by(Week, zone_id)%>%summarise(precip=mean(precip),av_max=mean(av_max), av_min=mean(av_min), av_mean=mean(av_mean), refET=mean(refET) )
  zone_sylvopast_weather<-read.csv('ZoneSylvopastorale_weather_from_python.csv')
  zone_sylvopast_weather$zone_id<-0
  zone_sylvopast_weather$year<-year(zone_sylvopast_weather$Date)
  zone_sylvopast_weather$month<-month(zone_sylvopast_weather$Date)
  zone_sylvopast_weather$mean_temp=(zone_sylvopast_weather$MinTemp+zone_sylvopast_weather$MaxTemp)/2
  zone_sylvopast_weather_month_summary<-zone_sylvopast_weather%>%filter(month %in% c(6,7,8,9))%>%group_by(month, year, zone_id)%>%summarise(precip=sum(Precipitation),av_max=mean(MaxTemp), av_min=mean(MinTemp), av_mean=mean(mean_temp) )
  zone_sylvopast_weather_year_summary<-zone_sylvopast_weather_month_summary%>%group_by(year, zone_id)%>%summarise(precip=sum(precip),av_max=mean(av_max), av_min=mean(av_min), av_mean=mean(av_mean) )
  zone_sylvopast_weather$Week<-week(zone_sylvopast_weather$Date)
  zone_sylvopast_weather_week_summary<-zone_sylvopast_weather%>%group_by(Week, year, zone_id)%>%summarise(precip=sum(Precipitation),av_max=mean(MaxTemp), av_min=mean(MinTemp), av_mean=mean(mean_temp), refET=sum(ReferenceET) )
  zone_sylvopast_weather_averaged_week<-zone_sylvopast_weather_week_summary%>%group_by(Week, zone_id)%>%summarise(precip=mean(precip),av_max=mean(av_max), av_min=mean(av_min), av_mean=mean(av_mean), refET=mean(refET) )
  
  senegal_orient_weather<-read.csv('SenegalOriental_weather_from_python.csv')
  senegal_orient_weather$zone_id<-4
  senegal_orient_weather$Date<-as.Date(senegal_orient_weather$Date)
  
  senegal_orient_weather$year<-year(senegal_orient_weather$Date)
  senegal_orient_weather$month<-month(senegal_orient_weather$Date)
  senegal_orient_weather$mean_temp=(senegal_orient_weather$MinTemp+senegal_orient_weather$MaxTemp)/2
  senegal_orient_weather_month_summary<-senegal_orient_weather%>%filter(month %in% c(6,7,8,9))%>%group_by(month, year, zone_id)%>%summarise(precip=sum(Precipitation),av_max=mean(MaxTemp), av_min=mean(MinTemp), av_mean=mean(mean_temp) )
  senegal_orient_weather$Week<-week(senegal_orient_weather$Date)
  senegal_orient_weather_week_summary<-senegal_orient_weather%>%group_by(Week, year, zone_id)%>%summarise(precip=sum(Precipitation),av_max=mean(MaxTemp), av_min=mean(MinTemp), av_mean=mean(mean_temp), refET=sum(ReferenceET) )
  senegal_orient_weather_averaged_week<-senegal_orient_weather_week_summary%>%group_by(Week, zone_id)%>%summarise(precip=mean(precip),av_max=mean(av_max), av_min=mean(av_min), av_mean=mean(av_mean), refET=mean(refET) )
  
  month_weather_summary<-rbind(rbind(rbind(zone_sylvopast_weather_month_summary, basin_arach__weather_month_summary), casamance_weather_month_summary), senegal_orient_weather_month_summary)
  av_weather_20year<-month_weather_summary%>%filter(year>2000)%>%group_by(month,zone_id)%>%summarise(av_pcp=mean(precip), av_temp = mean(av_mean) )
  month_weather_summary$zone<-factor(month_weather_summary$zone_id, levels = c('Casamance','Bassin arachidier','Zone sylvopastorale', 'Senegal oriental') )
  month_weather_summary$zone<-'Bassin arachidier'
  month_weather_summary$zone[month_weather_summary$zone_id==0]<-'Zone sylvopastorale'
  month_weather_summary$zone[month_weather_summary$zone_id==3]<-'Casamance'
  month_weather_summary$zone[month_weather_summary$zone_id==4]<-'Senegal oriental'
  return(month_weather_summary)
}



make_weather_figures_by_zone<-function(){
  
  #############FIGURES################
  BA_weekly_precip<-ggplot(data=basin_arach__weather_averaged_week,
                           aes(x=Week, y=precip)) + geom_line() +
    labs(y= "Precip", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
    ggtitle("Precip by Week: Bassin Arachdier") 
  BA_weekly_ET<-ggplot(data=basin_arach__weather_averaged_week,
                       aes(x=Week, y=refET)) + geom_line() +
    labs(y= "ET", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
    ggtitle("Ref ET by Week: Bassin Arachdier")
  
  
  cs_weekly_precip<-ggplot(data=casamance_weather_averaged_week,
                           aes(x=Week, y=precip)) + geom_line() +
    labs(y= "Precip", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
    ggtitle("Precip by Week: Casamance") 
  CS_weekly_ET<-ggplot(data=casamance_weather_averaged_week,
                       aes(x=Week, y=refET)) + geom_line() +
    labs(y= "ET", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
    ggtitle("Ref ET by Week: Casamance")
  
  ZS_weekly_precip<-ggplot(data=zone_sylvopast_weather_averaged_week,
                           aes(x=Week, y=precip)) + geom_line() +
    labs(y= "Precip", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
    ggtitle("Precip by Week: Sylvopasture") 
  ZS_weekly_ET<-ggplot(data=zone_sylvopast_weather_averaged_week,
                       aes(x=Week, y=refET)) + geom_line() +
    labs(y= "ET", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
    ggtitle("Ref ET by Week: Sylvopasture")
  
  east_weekly_precip<-ggplot(data=senegal_orient_weather_averaged_week,
                             aes(x=Week, y=precip)) + geom_line() +
    labs(y= "Precip", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
    ggtitle("Precip by Week: East") 
  east_weekly_ET<-ggplot(data=senegal_orient_weather_averaged_week,
                         aes(x=Week, y=refET)) + geom_line() +
    labs(y= "ET", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
    ggtitle("Ref ET by Week: East")
}






