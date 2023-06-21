library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(gridExtra)


###################################################
#READ AND SUMMARIZE AQUACROP RESULT FILES
setwd('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\')

millet_summary<-read.csv("millet_FINAL_multiple_PlantingDates.csv")

millet_growth<-read.csv("millet_GROWTH_multiple_PlantingDates.csv")
millet_growth$year<-millet_growth$SeasonCounter+1981
millet_growth<-millet_growth%>%filter(DAP>0)
millet_growth_summary<-millet_growth%>%group_by(year, site_id, soil_id, zone, planting_date)%>%summarise(biomass=max(B), yield=max(Y), harvest_DOY = max(DAP)+183 )
millet_growth<-NULL
gc()

millet_flux<-read.csv("millet_FLUX_multiple_PlantingDates.csv")
millet_flux$year<-millet_flux$SeasonCounter+1981
millet_flux<-millet_flux%>%filter(DAP>0)
millet_flux_summary<-millet_flux%>%group_by(year, site_id, soil_id, zone, planting_date)%>%summarise(precip=sum(P))
millet_flux<-millet_flux%>%select(year, site_id, soil_id, zone, planting_date, DAP, Wr, Tr, P, Es)
millet_flux<-NULL
gc()
###############################################
#WEATHER INPUT FILES
setwd('C:\\senegal_aquacrop_model\\weather\\aquacrop_python')
basin_arach__weather<-read.csv('BassinArachidier_weather_from_python.csv')
basin_arach__weather$zone_id<-6
basin_arach__weather$year<-year(basin_arach__weather$Date)
basin_arach__weather$month<-month(basin_arach__weather$Date)
basin_arach__weather$mean_temp=(basin_arach__weather$MinTemp+basin_arach__weather$MaxTemp)/2
basin_arach__weather_month_summary<-basin_arach__weather%>%filter(month %in% c(6,7,8,9))%>%group_by(month, year, zone_id)%>%summarise(precip=sum(Precipitation),av_max=mean(MaxTemp), av_min=mean(MinTemp), av_mean=mean(mean_temp) )
basin_arach_weather_year_summary<-basin_arach__weather_month_summary%>%group_by(year, zone_id)%>%summarise(precip=sum(precip),av_max=mean(av_max), av_min=mean(av_min), av_mean=mean(av_mean) )
mean(basin_arach_weather_year_summary$precip)
median(basin_arach_weather_year_summary$precip)
sd(basin_arach_weather_year_summary$precip)
hist(basin_arach_weather_year_summary$precip)

basin_arach__weather$Week<-week(basin_arach__weather$Date)
basin_arach__weather_week_summary<-basin_arach__weather%>%group_by(Week, year, zone_id)%>%summarise(precip=sum(Precipitation),av_max=mean(MaxTemp), av_min=mean(MinTemp), av_mean=mean(mean_temp), refET=sum(ReferenceET) )
basin_arach__weather_averaged_week<-basin_arach__weather_week_summary%>%group_by(Week, zone_id)%>%summarise(precip=mean(precip),av_max=mean(av_max), av_min=mean(av_min), av_mean=mean(av_mean), refET=mean(refET) )
BA_weekly_precip<-ggplot(data=basin_arach__weather_averaged_week,
                           aes(x=Week, y=precip)) + geom_line() +
  labs(y= "Precip", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Precip by Week: Bassin Arachdier") 
BA_weekly_ET<-ggplot(data=basin_arach__weather_averaged_week,
                         aes(x=Week, y=refET)) + geom_line() +
  labs(y= "ET", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Ref ET by Week: Bassin Arachdier")


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
cs_weekly_precip<-ggplot(data=casamance_weather_averaged_week,
                           aes(x=Week, y=precip)) + geom_line() +
  labs(y= "Precip", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Precip by Week: Casamance") 
CS_weekly_ET<-ggplot(data=casamance_weather_averaged_week,
                     aes(x=Week, y=refET)) + geom_line() +
  labs(y= "ET", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Ref ET by Week: Casamance")


zone_sylvopast_weather<-read.csv('ZoneSylvopastorale_weather_from_python.csv')
zone_sylvopast_weather$zone_id<-0
zone_sylvopast_weather$year<-year(zone_sylvopast_weather$Date)
zone_sylvopast_weather$month<-month(zone_sylvopast_weather$Date)
zone_sylvopast_weather$mean_temp=(zone_sylvopast_weather$MinTemp+zone_sylvopast_weather$MaxTemp)/2
zone_sylvopast_weather_month_summary<-zone_sylvopast_weather%>%filter(month %in% c(6,7,8,9))%>%group_by(month, year, zone_id)%>%summarise(precip=sum(Precipitation),av_max=mean(MaxTemp), av_min=mean(MinTemp), av_mean=mean(mean_temp) )
zone_sylvopast_weather_year_summary<-zone_sylvopast_weather_month_summary%>%group_by(year, zone_id)%>%summarise(precip=sum(precip),av_max=mean(av_max), av_min=mean(av_min), av_mean=mean(av_mean) )
mean(zone_sylvopast_weather_year_summary$precip)
median(zone_sylvopast_weather_year_summary$precip)
sd(zone_sylvopast_weather_year_summary$precip)
hist(zone_sylvopast_weather_year_summary$precip)

zone_sylvopast_weather$Week<-week(zone_sylvopast_weather$Date)
zone_sylvopast_weather_week_summary<-zone_sylvopast_weather%>%group_by(Week, year, zone_id)%>%summarise(precip=sum(Precipitation),av_max=mean(MaxTemp), av_min=mean(MinTemp), av_mean=mean(mean_temp), refET=sum(ReferenceET) )
zone_sylvopast_weather_averaged_week<-zone_sylvopast_weather_week_summary%>%group_by(Week, zone_id)%>%summarise(precip=mean(precip),av_max=mean(av_max), av_min=mean(av_min), av_mean=mean(av_mean), refET=mean(refET) )
ZS_weekly_precip<-ggplot(data=zone_sylvopast_weather_averaged_week,
                           aes(x=Week, y=precip)) + geom_line() +
  labs(y= "Precip", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Precip by Week: Sylvopasture") 
ZS_weekly_ET<-ggplot(data=zone_sylvopast_weather_averaged_week,
                     aes(x=Week, y=refET)) + geom_line() +
  labs(y= "ET", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Ref ET by Week: Sylvopasture")

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
east_weekly_precip<-ggplot(data=senegal_orient_weather_averaged_week,
       aes(x=Week, y=precip)) + geom_line() +
  labs(y= "Precip", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Precip by Week: East") 
east_weekly_ET<-ggplot(data=senegal_orient_weather_averaged_week,
                     aes(x=Week, y=refET)) + geom_line() +
  labs(y= "ET", x = "Week of Year")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Ref ET by Week: East")


month_weather_summary<-rbind(rbind(rbind(zone_sylvopast_weather_month_summary, basin_arach__weather_month_summary), casamance_weather_month_summary), senegal_orient_weather_month_summary)
av_weather_20year<-month_weather_summary%>%filter(year>2000)%>%group_by(month,zone_id)%>%summarise(av_pcp=mean(precip), av_temp = mean(av_mean) )
month_weather_summary$zone<-factor(month_weather_summary$zone_id, levels = c('Casamance','Bassin arachidier','Zone sylvopastorale', 'Senegal oriental') )
month_weather_summary$zone<-'Bassin arachidier'
month_weather_summary$zone[month_weather_summary$zone_id==0]<-'Zone sylvopastorale'
month_weather_summary$zone[month_weather_summary$zone_id==3]<-'Casamance'
month_weather_summary$zone[month_weather_summary$zone_id==4]<-'Senegal oriental'


annual_summary<-month_weather_summary%>%filter(year>1980)%>%group_by(year,zone_id, zone)%>%summarise(av_pcp=sum(precip), av_temp = mean(av_mean) )


ggplot(annual_summary%>%filter(year>1980 & zone %in% c('Casamance','Bassin arachidier','Zone sylvopastorale', 'Senegal oriental')), aes(x = zone, y = av_pcp))+
  geom_boxplot()

june_summary<-month_weather_summary%>%filter(month==6)
colnames(june_summary)<-c("june"  , "year"  ,  "zone_id" ,"june_precip" , "june_av_max" , "june_av_min" , "june_av_mean", "zone")

july_summary<-month_weather_summary%>%filter(month==7)
colnames(july_summary)<-c("july"  , "year"  ,  "zone_id" ,"july_precip" , "july_av_max" , "july_av_min" , "july_av_mean", "zone")

august_summary<-month_weather_summary%>%filter(month==8)
colnames(august_summary)<-c("august"  , "year"  ,  "zone_id" ,"august_precip" , "august_av_max" , "august_av_min" , "august_av_mean", "zone")

sept_summary<-month_weather_summary%>%filter(month==9)
colnames(sept_summary)<-c("sept"  , "year"  ,  "zone_id" ,"sept_precip" , "sept_av_max" , "sept_av_min" , "sept_av_mean", "zone")


month_weather_summary<-month_weather_summary%>%filter(year>2000)
month_weather_summary<-merge(month_weather_summary, av_weather_20year, by=c('month', 'zone_id'))

month_weather_summary$precip_prop<-month_weather_summary$precip/month_weather_summary$av_pcp
month_weather_summary$temp_prop<-month_weather_summary$av_mean/month_weather_summary$av_temp




df_nioro<-month_weather_summary%>%filter(zone_id %in% c(6) & year>2000)
plot_nioro<-ggplot(df_nioro, aes(x = year, y= precip_prop, fill = as.factor(month)), xlab="Year") +
  geom_bar(stat="identity", width=.5, position = "dodge")  +
  ggtitle("Middle") +
  theme(plot.title = element_text(hjust = 0.5))
df_samyoro<-month_weather_summary%>%filter(zone_id %in% c(3) & year>2000)
plot_sam<-ggplot(df_samyoro, aes(x = year, y= precip_prop, fill = as.factor(month)), xlab="Year") +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  ggtitle("South") +
  theme(plot.title = element_text(hjust = 0.5)) 
df_louga<-month_weather_summary%>%filter(zone_id %in% c(0) & year>2000)
plot_louga<-ggplot(df_louga, aes(x = year, y= precip_prop, fill = as.factor(month)), xlab="Year") +
  geom_bar(stat="identity", width=.5, position = "dodge")  +
  ggtitle("North") +
  theme(plot.title = element_text(hjust = 0.5))

df_east<-month_weather_summary%>%filter(zone_id %in% c(4) & year>2000)
plot_east<-ggplot(df_east, aes(x = year, y= precip_prop, fill = as.factor(month)), xlab="Year") +
  geom_bar(stat="identity", width=.5, position = "dodge")  +
  ggtitle("East") +
  theme(plot.title = element_text(hjust = 0.5))

gridExtra::grid.arrange(plot_sam, plot_nioro,plot_louga, plot_east, ncol = 2) 





df_nioro<-month_weather_summary%>%filter(zone_id %in% c(6) & year>2000)
plot_nioro<-ggplot(df_nioro, aes(x = year, y= precip, fill = as.factor(month)), xlab="Year") +
  geom_bar(stat="identity", width=.5, position = "dodge")  +
  ggtitle("Middle") +
  theme(plot.title = element_text(hjust = 0.5))
df_samyoro<-month_weather_summary%>%filter(zone_id %in% c(3) & year>2000)
plot_sam<-ggplot(df_samyoro, aes(x = year, y= precip, fill = as.factor(month)), xlab="Year") +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  ggtitle("South") +
  theme(plot.title = element_text(hjust = 0.5)) 
df_louga<-month_weather_summary%>%filter(zone_id %in% c(0) & year>2000)
plot_louga<-ggplot(df_louga, aes(x = year, y= precip, fill = as.factor(month)), xlab="Year") +
  geom_bar(stat="identity", width=.5, position = "dodge")  +
  ggtitle("North") +
  theme(plot.title = element_text(hjust = 0.5))

df_east<-month_weather_summary%>%filter(zone_id %in% c(4) & year>2000)
plot_east<-ggplot(df_east, aes(x = year, y= precip, fill = as.factor(month)), xlab="Year") +
  geom_bar(stat="identity", width=.5, position = "dodge")  +
  ggtitle("East") +
  theme(plot.title = element_text(hjust = 0.5))

gridExtra::grid.arrange(plot_sam, plot_nioro,plot_louga, plot_east, ncol = 2) 



df_nioro<-month_weather_summary%>%filter(zone_id %in% c(6) & year>2015)
plot_nioro<-ggplot(df_nioro, aes(x = year, y= av_mean, fill = as.factor(month)), xlab="Year") +
  geom_bar(stat="identity", width=.5, position = "dodge")  +
  ggtitle("Nioro") +
  theme(plot.title = element_text(hjust = 0.5))
df_samyoro<-month_weather_summary%>%filter(zone_id %in% c(3) & year>2015)
plot_sam<-ggplot(df_samyoro, aes(x = year, y= av_mean, fill = as.factor(month)), xlab="Year") +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  ggtitle("Sam Yoro") +
  theme(plot.title = element_text(hjust = 0.5)) 
df_louga<-month_weather_summary%>%filter(zone_id %in% c(0) & year>2015)
plot_louga<-ggplot(df_louga, aes(x = year, y= av_mean, fill = as.factor(month)), xlab="Year") +
  geom_bar(stat="identity", width=.5, position = "dodge")  +
  ggtitle("Louga") +
  theme(plot.title = element_text(hjust = 0.5))

gridExtra::grid.arrange(plot_sam, plot_nioro,plot_louga, ncol = 2) 

#####################################################################



setwd('C:\\senegal_aquacrop_model\\data')
soil<-read.table( 'AEZ_samples_with_soil2.txt', sep = ',', header = TRUE)
soil<-soil%>%select(c('ID','clay_0_5cm','clay_15_30','clay_30_60','clay_5_15c','clay_60_10','sand_0_5cm','sand_15_30','sand_30_60','sand_5_15c','sand_60_10','silt_0_5cm','silt_15_30','silt_30_60','silt_5_15c','silt_60_10','soc_0_5cm','soc_15_30c','soc_30_60c','soc_5_15cm','soc_60_100','region')
)
soil$upper_60_clay_ave<-(5*soil$clay_0_5cm+10*soil$clay_5_15c+15*soil$clay_15_30+30*soil$clay_30_60)/60
soil$upper_60_silt_ave<-(5*soil$silt_0_5cm+10*soil$silt_5_15c+15*soil$silt_15_30+30*soil$silt_30_60)/60
soil$upper_60_sand_ave<-(5*soil$sand_0_5cm+10*soil$sand_5_15c+15*soil$sand_15_30+30*soil$sand_30_60)/60
soil$upper_60_soc_ave<-(5*soil$soc_0_5cm+10*soil$soc_5_15cm+15*soil$soc_15_30c+30*soil$soc_30_60c)/60

#############################
#MERGE DATASETS
millet_factors<-merge(millet_flux_summary, millet_growth_summary, by=c('site_id', 'soil_id', 'year', 'planting_date'))
millet_factors<-merge(millet_factors, soil, by.x='soil_id', by.y = "ID")
millet_factors<-millet_factors%>%select(-one_of('zone.x','zone.y', 'region'))

millet_factors<-merge(millet_factors, june_summary, by.x=c('site_id', 'year'), by.y = c('zone_id', 'year'))
millet_factors<-merge(millet_factors, july_summary, by.x=c('site_id', 'year'), by.y = c('zone_id', 'year'))
millet_factors<-merge(millet_factors, august_summary, by.x=c('site_id', 'year'), by.y = c('zone_id', 'year'))
millet_factors<-merge(millet_factors, sept_summary, by.x=c('site_id', 'year'), by.y = c('zone_id', 'year'))




correlations<-cor(millet_factors)
setwd('C:\\SenegalGIS\\crop_model_result\\table_figures\\millet\\')
write.csv(correlations, file = 'millet_factors_correlations.csv')

millet_soil_factors_basin_arach_<-millet_factors%>%filter(site_id==6)%>%select(yield,upper_60_clay_ave, upper_60_silt_ave, upper_60_sand_ave, upper_60_soc_ave)
correlations<-cor(millet_soil_factors_basin_arach_)
write.csv(correlations, file = 'millet_factors_soil_basin_arach__correlations.csv')

millet_soil_factors_samy<-millet_factors%>%filter(site_id==3)%>%select(yield,upper_60_clay_ave, upper_60_silt_ave, upper_60_sand_ave, upper_60_soc_ave)
correlations<-cor(millet_soil_factors_samy)
write.csv(correlations, file = 'millet_factors_soil_casamance_correlations.csv')

millet_soil_factors_zone_sylvopast<-millet_factors%>%filter(site_id==0)%>%select(yield,upper_60_clay_ave, upper_60_silt_ave, upper_60_sand_ave, upper_60_soc_ave)
correlations<-cor(millet_soil_factors_zone_sylvopast)
write.csv(correlations, file = 'millet_factors_soil_zone_sylvopast_correlations.csv')


millet_soil_factors_orient<-millet_factors%>%filter(site_id==4)%>%select(yield,upper_60_clay_ave, upper_60_silt_ave, upper_60_sand_ave, upper_60_soc_ave)
correlations<-cor(millet_soil_factors_orient)
write.csv(correlations, file = 'millet_factors_soil_orient_correlations.csv')

#######################################################
##########YIELD SUMMARY#######################
millet_summary$zone<-factor(millet_summary$zone, levels = c('Casamance','Bassin arachidier','Zone sylvopastorale', 'Senegal oriental') )
millet_summary$Harvest.Date..YYYY.MM.DD.<-as.Date(millet_summary$Harvest.Date..YYYY.MM.DD.)
millet_summary$year<-lubridate::year(millet_summary$Harvest.Date..YYYY.MM.DD.)

ggplot(millet_summary%>%filter(year>1984 & Yield..tonne.ha.>-1 & zone %in% c('Casamance','Bassin arachidier','Zone sylvopastorale', 'Senegal oriental')), aes(x = zone, y = Yield..tonne.ha.))+
  geom_boxplot()


########VARIANCE OF SOIL##############
#summarise each row by the av, soilc, silt, sand, clay, 0-60
#plot each by site, show variance

soil$region<-factor(soil$region, levels = c('Casamance','Bassin arachidier','Zone sylvopastorale', 'Senegal oriental') )

ggplot(soil%>%filter(upper_60_silt_ave>0 & region %in% c('Casamance','Bassin arachidier','Zone sylvopastorale', 'Senegal oriental')), aes(x = region, y = upper_60_silt_ave/10)) +
  #geom_point(aes(color = as.factor(region)), size = 1) +
  geom_boxplot()

ggplot(soil%>%filter(upper_60_clay_ave>0 & region %in% c('Casamance','Bassin arachidier','Zone sylvopastorale', 'Senegal oriental')), aes(x = region, y = upper_60_clay_ave/10)) +
  #geom_point(aes(color = as.factor(region)), size = 1) +
  geom_boxplot() 

ggplot(soil%>%filter(upper_60_sand_ave>0 & region %in% c('Casamance','Bassin arachidier','Zone sylvopastorale', 'Senegal oriental')), aes(x = region, y = upper_60_sand_ave/10)) +
  #geom_point(aes(color = as.factor(region)), size = 1) +
  geom_boxplot() 

ggplot(soil%>%filter(upper_60_soc_ave>0 & region %in% c('Casamance','Bassin arachidier','Zone sylvopastorale', 'Senegal oriental')), aes(x = region, y = upper_60_soc_ave/100)) +
  #geom_point(aes(color = as.factor(region)), size = 1) +
  geom_boxplot() 
#######################################



get_thirds = function(my_vector){
  thirds<-quantile(my_vector, probs=c(.333,.666))
  return_vector<-vector(mode="character", length=length(my_vector))
  return_vector[which(my_vector>-9999)]<-'MIDDLE'
  return_vector[which(my_vector>thirds[2])]<-'HIGHEST'
  return_vector[which(my_vector<thirds[1])]<-'LOWEST'
  return_vector = factor(return_vector, levels = c('HIGHEST', 'MIDDLE', 'LOWEST'))
  return(return_vector)
}

get_precip_cat<-function(my_vector){
  return_vector<-vector(mode="character", length=length(my_vector))
  return_vector[which(my_vector>400)]<-'>400mm'
  return_vector[which(my_vector>=200 & my_vector<=400)]<-'200-400mm'
  return_vector[which(my_vector<200)]<-'<200mm'
  return_vector = factor(return_vector, levels = c('>400mm', '200-400mm', '<200mm'))
  return(return_vector)
}


millet_factors_wc_join<-millet_factors%>%select(year, site_id, soil_id,  yield, upper_60_clay_ave, upper_60_sand_ave,  upper_60_silt_ave, upper_60_soc_ave, planting_date, precip)
millet_factors_wc_join$sandclayratio<-millet_factors_wc_join$upper_60_sand_ave/millet_factors_wc_join$upper_60_clay_ave

millet_flux<-millet_flux%>%filter(Wr<200)
millet_flux_join_yield<-merge(millet_flux, millet_factors_wc_join, by=c('year', 'site_id', 'soil_id', 'planting_date'))
millet_flux_join_yield$Yield_third<-factor("NULL", levels = c('HIGHEST', 'MIDDLE', 'LOWEST'))
millet_flux_join_yield$Yield_third[millet_flux_join_yield$site_id==0]<-get_thirds(millet_flux_join_yield$yield[millet_flux_join_yield$site_id==0])
millet_flux_join_yield$Yield_third[millet_flux_join_yield$site_id==3]<-get_thirds(millet_flux_join_yield$yield[millet_flux_join_yield$site_id==3])
millet_flux_join_yield$Yield_third[millet_flux_join_yield$site_id==6]<-get_thirds(millet_flux_join_yield$yield[millet_flux_join_yield$site_id==6])
millet_flux_join_yield$Yield_third[millet_flux_join_yield$site_id==4]<-get_thirds(millet_flux_join_yield$yield[millet_flux_join_yield$site_id==4])


####################### PLOTS OF WATER CONTENT BY YIELD ##########################
plot_data<-data.frame(time_series_water_content%>%filter(site_id==6))
yield1<-plot_data%>%filter(Yield_third=='HIGHEST')%>%select(yield)
precip1<-plot_data%>%filter(Yield_third=='HIGHEST')%>%select(precip)
clay1<-plot_data%>%filter(Yield_third=='HIGHEST')%>%select(clay)
yield2<-plot_data%>%filter(Yield_third=='MIDDLE')%>%select(yield)
precip2<-plot_data%>%filter(Yield_third=='MIDDLE')%>%select(precip)
clay2<-plot_data%>%filter(Yield_third=='MIDDLE')%>%select(clay)
yield3<-plot_data%>%filter(Yield_third=='LOWEST')%>%select(yield)
precip3<-plot_data%>%filter(Yield_third=='LOWEST')%>%select(precip)
clay3<-plot_data%>%filter(Yield_third=='LOWEST')%>%select(clay)
label_text1<-paste("Avg Yield:" , round(yield1[1,1], 2) * 100 ,  "kg" , "\nClay:" ,round(clay1[1,1], 0) / 10 ,  "%", "\nPrecip:" ,round(precip1[1,1], 0)  ,  "mm")
label_text2<-paste("Avg Yield:" , round(yield2[1,1], 2) * 100 ,  "kg" , "\nClay:" ,round(clay2[1,1], 0) / 10 ,  "%", "\nPrecip:" ,round(precip2[1,1], 0)  ,  "mm")
label_text3<-paste("Avg Yield:" , round(yield3[1,1], 2) * 100 ,  "kg" , "\nClay:" ,round(clay3[1,1], 0) / 10 ,  "%", "\nPrecip:" ,round(precip3[1,1], 0)  ,  "mm")

BA_plot <- ggplot(data=plot_data,
                             aes(x=DAP, y=water_content, group = Yield_third, colour = Yield_third)) + geom_line() +
  labs(y= "Water content in soil column (mm)", x = "DAP")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Water content by yield quantiles: Bassin Arachdier") +
  annotate("text", x=17, y=43, label= label_text1)+
  annotate("text", x=22, y=35, label= label_text2)+
  annotate("text", x=30, y=25, label= label_text3)

plot_data<-data.frame(time_series_water_content%>%filter(site_id==0))
yield1<-plot_data%>%filter(Yield_third=='HIGHEST')%>%select(yield)
precip1<-plot_data%>%filter(Yield_third=='HIGHEST')%>%select(precip)
clay1<-plot_data%>%filter(Yield_third=='HIGHEST')%>%select(clay)
yield2<-plot_data%>%filter(Yield_third=='MIDDLE')%>%select(yield)
precip2<-plot_data%>%filter(Yield_third=='MIDDLE')%>%select(precip)
clay2<-plot_data%>%filter(Yield_third=='MIDDLE')%>%select(clay)
label_text1<-paste("Avg Yield:" , round(yield1[1,1], 2) * 100 ,  "kg" , "\nClay:" ,round(clay1[1,1], 0) / 10 ,  "%", "\nPrecip:" ,round(precip1[1,1], 0)  ,  "mm")
label_text2<-paste("Avg Yield:" , round(yield2[1,1], 2) * 100 ,  "kg" , "\nClay:" ,round(clay2[1,1], 0) / 10 ,  "%", "\nPrecip:" ,round(precip2[1,1], 0)  ,  "mm")


ZS_plot <- ggplot(data=time_series_water_content%>%filter(site_id==0),
                  aes(x=DAP, y=water_content, group = Yield_third, colour = Yield_third)) + geom_line()+
  labs(y= "Water content in soil column (mm)", x = "DAP")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Water content by yield quantiles: Zone Sylvopasture") +
  annotate("text", x=20, y=40, label= label_text1)+
  annotate("text", x=24, y=27, label= label_text2)

plot_data<-data.frame(time_series_water_content%>%filter(site_id==3))
yield1<-plot_data%>%filter(Yield_third=='HIGHEST')%>%select(yield)
precip1<-plot_data%>%filter(Yield_third=='HIGHEST')%>%select(precip)
clay1<-plot_data%>%filter(Yield_third=='HIGHEST')%>%select(clay)
yield2<-plot_data%>%filter(Yield_third=='MIDDLE')%>%select(yield)
precip2<-plot_data%>%filter(Yield_third=='MIDDLE')%>%select(precip)
clay2<-plot_data%>%filter(Yield_third=='MIDDLE')%>%select(clay)
yield3<-plot_data%>%filter(Yield_third=='LOWEST')%>%select(yield)
precip3<-plot_data%>%filter(Yield_third=='LOWEST')%>%select(precip)
clay3<-plot_data%>%filter(Yield_third=='LOWEST')%>%select(clay)
label_text1<-paste("Avg Yield:" , round(yield1[1,1], 2) * 100 ,  "kg" , "\nClay:" ,round(clay1[1,1], 0) / 10 ,  "%", "\nPrecip:" ,round(precip1[1,1], 0)  ,  "mm")
label_text2<-paste("Avg Yield:" , round(yield2[1,1], 2) * 100 ,  "kg" , "\nClay:" ,round(clay2[1,1], 0) / 10 ,  "%", "\nPrecip:" ,round(precip2[1,1], 0)  ,  "mm")
label_text3<-paste("Avg Yield:" , round(yield3[1,1], 2) * 100 ,  "kg" , "\nClay:" ,round(clay3[1,1], 0) / 10 ,  "%", "\nPrecip:" ,round(precip3[1,1], 0)  ,  "mm")

casa_plot<- ggplot(data=time_series_water_content%>%filter(site_id==3),
                   aes(x=DAP, y=water_content, group = Yield_third, colour = Yield_third)) + geom_line()+
  labs(y= "Water content in soil column (mm)", x = "DAP")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Water content by yield quantiles: Casamance") +
  annotate("text", x=18, y=100, label= label_text1)+
  annotate("text", x=18, y=81, label= label_text2)+
  annotate("text", x=18, y=50, label= label_text3)

plot_data<-data.frame(time_series_water_content%>%filter(site_id==4))
yield1<-plot_data%>%filter(Yield_third=='HIGHEST')%>%select(yield)
precip1<-plot_data%>%filter(Yield_third=='HIGHEST')%>%select(precip)
clay1<-plot_data%>%filter(Yield_third=='HIGHEST')%>%select(clay)
yield2<-plot_data%>%filter(Yield_third=='MIDDLE')%>%select(yield)
precip2<-plot_data%>%filter(Yield_third=='MIDDLE')%>%select(precip)
clay2<-plot_data%>%filter(Yield_third=='MIDDLE')%>%select(clay)
yield3<-plot_data%>%filter(Yield_third=='LOWEST')%>%select(yield)
precip3<-plot_data%>%filter(Yield_third=='LOWEST')%>%select(precip)
clay3<-plot_data%>%filter(Yield_third=='LOWEST')%>%select(clay)
label_text1<-paste("Avg Yield:" , round(yield1[1,1], 2) * 100 ,  "kg" , "\nClay:" ,round(clay1[1,1], 0) / 10 ,  "%", "\nPrecip:" ,round(precip1[1,1], 0)  ,  "mm")
label_text2<-paste("Avg Yield:" , round(yield2[1,1], 2) * 100 ,  "kg" , "\nClay:" ,round(clay2[1,1], 0) / 10 ,  "%", "\nPrecip:" ,round(precip2[1,1], 0)  ,  "mm")
label_text3<-paste("Avg Yield:" , round(yield3[1,1], 2) * 100 ,  "kg" , "\nClay:" ,round(clay3[1,1], 0) / 10 ,  "%", "\nPrecip:" ,round(precip3[1,1], 0)  ,  "mm")

east_plot<- ggplot(data=time_series_water_content%>%filter(site_id==4),
                   aes(x=DAP, y=water_content, group = Yield_third, colour = Yield_third)) + geom_line()+
  labs(y= "Water content in soil column (mm)", x = "DAP")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Water content by yield quantiles: East") +
  annotate("text", x=10, y=90, label= label_text1)+
  annotate("text", x=18, y=70, label= label_text2)+
  annotate("text", x=30, y=60, label= label_text3)
#######################  END PLOTS WATER CONTENT BY YIELD ######################################




###########GET MILLET FACTORS FROMN PLOT-VIs.R################

#############################################
##### make pairwise plots of 
######soil and rain by site
millet_factors$sand_level<-get_thirds(millet_factors$upper_60_sand_ave)
millet_factors$soc_level<-get_thirds(millet_factors$upper_60_soc_ave)
millet_factors$clay_level<-get_thirds(millet_factors$upper_60_clay_ave)
millet_factors$precip_level<-get_precip_cat(millet_factors$precip)
millet_factors$site_id<-factor(millet_factors$site_id, levels = c(0,6,3, 4), labels = c('Drier/North', 'Middle', 'Humid/South', 'East'))
millet_factors$Rain200='HIGH_RAIN_NORTH'
millet_factors$Rain200[which(millet_factors$precip<200)]='LOW_RAIN_NORTH'
millet_factors$Rain400='HIGH_RAIN_MIDSOUTH'
millet_factors$Rain400[which(millet_factors$precip<400)]='LOW_RAIN_MIDSOUTH'

millet_factors$Clay200='HIGH_CLAY'
millet_factors$Clay200[which(millet_factors$upper_60_clay_ave<200)]='LOW_CLAY'
millet_factors$Planting='NONE'
millet_factors$Planting[which(millet_factors$planting_date=='8/16')]='LATE'
millet_factors$Planting[which(millet_factors$planting_date=='7/16')]='EARLY'
millet_factors$Planting[which(millet_factors$planting_date=='8/1')]='MID'
millet_factors$Louga_mode<-paste(millet_factors$Rain200, millet_factors$Clay200, millet_factors$Planting, sep=':')
millet_factors$mode<-paste(millet_factors$Rain400, millet_factors$Clay200, millet_factors$Planting, sep=':')

mill_fact_summ<-millet_factors%>%group_by(upper_60_clay_ave, site_id, precip_level)%>%summarise(yield=mean(yield))
ggplot(mill_fact_summ, aes(x = upper_60_clay_ave, y = yield)) +
  geom_point(size = 1) + 
  theme_minimal()+ facet_grid(site_id ~ precip_level)


mill_fact_summ<-millet_factors%>%group_by(precip, site_id, sand_level)%>%summarise(yield=sd(yield))
ggplot(mill_fact_summ, aes(x = precip, y = yield)) +
  geom_point(size = 1) + 
  theme_minimal()+ facet_grid(site_id ~ sand_level)

ggplot(millet_factors, aes(x = precip, y = yield)) +
  geom_point(aes(color = as.factor(soc_level)), size = 1) +
  scale_color_manual(values = c("red", "orange", "yellow")) +
  theme_minimal()

ggplot(millet_factors, aes(x = precip, y = yield)) +
  geom_point(aes(color = as.factor(clay_level)), size = 1) +
  scale_color_manual(values = c("red", "orange", "yellow")) +
  theme_minimal()




#############################################
#################################################
######### PDFs####################
c('Drier/North', 'Middle', 'Humid/South', 'East')
louga_data<-millet_factors%>%filter(site_id=='Drier/North')
louga<-ggplot(louga_data, aes(x=yield, color=Clay200, fill=Clay200)) +
  # geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)

nioro_data<-millet_factors%>%filter(site_id=='Middle' )
nioro<-ggplot(nioro_data, aes(x=yield, color=Clay200, fill=Clay200)) +
  # geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)

casamance_data<-millet_factors%>%filter(site_id=='Humid/South')
samyo<-ggplot(millet_factors%>%filter(site_id=='Humid/South'), aes(x=yield, color=Clay200, fill=Clay200)) +
  # geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)

east_data<-millet_factors%>%filter(site_id=='East' )
sene_orient<-ggplot(millet_factors%>%filter(site_id=='East' ), aes(x=yield, color=Clay200, fill=Clay200)) +
  # geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)



nioro_means<-NULL
nioro_means <- data.frame(matrix(ncol = 4, nrow = 4000))
nioro_data$precip<-as.numeric(nioro_data$precip)
#provide column names
colnames(nioro_means) <- c('yield', 'condition','precip', 'site')
j=1
n<-1000
for( x in list(nioro_data%>%filter(Clay200=='HIGH_CLAY' & precip>290), 
               nioro_data%>%filter(Clay200=='LOW_CLAY' & precip>290),
               nioro_data%>%filter(Clay200=='HIGH_CLAY' & precip<=290), 
               nioro_data%>%filter(Clay200=='LOW_CLAY' & precip<=290))){
  for(i in 1:n){
    nioro_means$yield[j] = mean(sample(x$biomass, 20, replace = FALSE, prob = NULL))
    nioro_means$condition[j] = x$Clay200[1]
    nioro_means$precip[j] = x$precip[1]
    nioro_means$site  = x$site_id[1]
    j=j+1
  }
}
nioro_means$high_rain<-nioro_means$precip>290
nioro_means$condition<-paste(nioro_means$condition, ', Rain > 290mm: ', nioro_means$high_rain)


louga_means<-NULL
louga_means <- data.frame(matrix(ncol = 4, nrow = 4000))
louga_data$precip<-as.numeric(louga_data$precip)
#provide column names
colnames(louga_means) <- c('yield', 'condition','precip', 'site')
j=1
n<-1000
for( x in list(louga_data%>%filter(Clay200=='HIGH_CLAY' & precip>220), 
               louga_data%>%filter(Clay200=='LOW_CLAY' & precip>220),
               louga_data%>%filter(Clay200=='HIGH_CLAY' & precip<=220), 
               louga_data%>%filter(Clay200=='LOW_CLAY' & precip<=220))){
  for(i in 1:n){
    louga_means$yield[j] = mean(sample(x$biomass, 20, replace = FALSE, prob = NULL))
    louga_means$condition[j] = x$Clay200[1]
    louga_means$precip[j] = x$precip[1]
    louga_means$site  = x$site_id[1]
    j=j+1
  }
}
louga_means$high_rain<-louga_means$precip>220
louga_means$condition<-paste(louga_means$condition, ', Rain > 220mm: ', louga_means$high_rain)

east_means<-NULL
east_means <- data.frame(matrix(ncol = 4, nrow = 4000))
east_data$precip<-as.numeric(east_data$precip)
#provide column names
colnames(east_means) <- c('yield', 'condition','precip', 'site')
j=1
n<-1000
for( x in list(east_data%>%filter(Clay200=='HIGH_CLAY' & precip>440), 
               east_data%>%filter(Clay200=='LOW_CLAY' & precip>440),
               east_data%>%filter(Clay200=='HIGH_CLAY' & precip<=440), 
               east_data%>%filter(Clay200=='LOW_CLAY' & precip<=440))){
  for(i in 1:n){
    east_means$yield[j] = mean(sample(x$biomass, 20, replace = FALSE, prob = NULL))
    east_means$condition[j] = x$Clay200[1]
    east_means$precip[j] = x$precip[1]
    east_means$site  = x$site_id[1]
    j=j+1
  }
}
east_means$high_rain<-east_means$precip>440
east_means$condition<-paste(east_means$condition, ', Rain > 440mm: ', east_means$high_rain)


casamance_means<-NULL
casamance_means <- data.frame(matrix(ncol = 4, nrow = 4000))
casamance_data$precip<-as.numeric(casamance_data$precip)
#provide column names
colnames(casamance_means) <- c('yield', 'condition','precip', 'site')
j=1
n<-1000
for( x in list(casamance_data%>%filter(Clay200=='HIGH_CLAY' & precip>560), 
               casamance_data%>%filter(Clay200=='LOW_CLAY' & precip>560),
               casamance_data%>%filter(Clay200=='HIGH_CLAY' & precip<=560), 
               casamance_data%>%filter(Clay200=='LOW_CLAY' & precip<=560))){
  for(i in 1:n){
    casamance_means$yield[j] = mean(sample(x$biomass, 20, replace = FALSE, prob = NULL))
    casamance_means$condition[j] = x$Clay200[1]
    casamance_means$precip[j] = x$precip[1]
    casamance_means$site  = x$site_id[1]
    j=j+1
  }
}
casamance_means$high_rain<-casamance_means$precip>560
casamance_means$condition<-paste(casamance_means$condition, ', Rain > 560mm: ', casamance_means$high_rain)


louga_clay_means_plot<-ggplot(louga_means, aes(x=yield, color=condition, fill=condition)) +
  # geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  ggtitle("Zone Sylvopasture: Semi-arid") +
  theme(plot.title = element_text(hjust = 0.5))

nioro_clay_means_plot<-ggplot(nioro_means, aes(x=yield, color=condition, fill=condition)) +
  # geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  ggtitle("Bassin Arachdier: Semi-arid") +
  theme(plot.title = element_text(hjust = 0.5))


casamance_clay_means_plot<-ggplot(casamance_means, aes(x=yield, color=condition, fill=condition)) +
  # geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  ggtitle("Casamance: Humid/Coastal") +
  theme(plot.title = element_text(hjust = 0.5))

east_clay_means_plot<-ggplot(east_means, aes(x=yield, color=condition, fill=condition)) +
  # geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  ggtitle("Senegal Oriental: Humid Inland") +
  theme(plot.title = element_text(hjust = 0.5))

aggregate_risk_louga = 100*length(louga_means$yield[which(louga_means$yield<600)])/length(louga_means$yield)
high_clay_risk_Louga = 100*length(louga_means$yield[which(louga_means$yield<600 & louga_means$condition=='HIGH_CLAY')])/length(louga_means$yield[which(louga_means$condition=='HIGH_CLAY')])
low_clay_risk_Louga = 100*length(louga_means$yield[which(louga_means$yield<600 & louga_means$condition=='LOW_CLAY')])/length(louga_means$yield[which(louga_means$condition=='LOW_CLAY')])

aggregate_risk_louga2 = 100*length(louga_data$biomass[which(louga_data$biomass<500 & louga_data$biomass>50)])/length(louga_data$biomass[which(louga_data$biomass>50)])
high_clay_risk_Louga2 = 100*length(louga_data$biomass[which(louga_data$biomass<500 & louga_data$biomass>50 & louga_data$Clay200=='HIGH_CLAY')])/length(louga_data$biomass[which(louga_data$Clay200=='HIGH_CLAY' & louga_data$biomass>50)])
low_clay_risk_Louga2 = 100*length(louga_data$biomass[which(louga_data$biomass<500 & louga_data$biomass>50 & louga_data$Clay200=='LOW_CLAY')])/length(louga_data$biomass[which(louga_data$Clay200=='LOW_CLAY' & louga_data$biomass>50)])


gridExtra::grid.arrange(louga_clay_means_plot,nioro_clay_means_plot, ncol = 1) 



aggregate_risk_nioro = 100*length(sample_means$yield[which(sample_means$yield<600)])/length(sample_means$yield)
high_clay_risk_nioro = 100*length(sample_means$yield[which(sample_means$yield<600 & sample_means$condition=='HIGH_CLAY')])/length(sample_means$yield[which(sample_means$condition=='HIGH_CLAY')])
low_clay_risk_nioro = 100*length(sample_means$yield[which(sample_means$yield<600 & sample_means$condition=='LOW_CLAY')])/length(sample_means$yield[which(sample_means$condition=='LOW_CLAY')])

