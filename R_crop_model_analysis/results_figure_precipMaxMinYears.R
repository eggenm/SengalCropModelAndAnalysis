#########################################
#
#    This will resample millet yields from years with high and low rain 
#     and create figures for each zone
#     
#    Assumes a file called fullrun_summary_median.csv exists
#
#
#############################
library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(soiltexture)


############### CHANGE THIS TO RUN LOCALLY ##########################
setwd('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\FINAL_code_output\\R_crop_model_analysis\\input\\')
source('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\FINAL_code_output\\R_crop_model_analysis\\yield_resampling_functions.R')
source('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\FINAL_code_output\\R_crop_model_analysis\\soil_summary.R')
summary_median<-read.csv('fullrun_summary_median.csv')
######################################################################################

precip_by_year<-summary_median%>%group_by(zone, year)%>%summarise(average_precip=mean(precip))

high_rainfall_years<-precip_by_year %>%
  group_by(zone) %>%
  arrange(-average_precip) %>%
  slice(c(1:5))

low_rainfall_years<-precip_by_year %>%
  group_by(zone) %>%
  arrange(average_precip) %>%
  slice(c(1:5))

temp<-summary_median%>%group_by(zone, year)%>%summarise(count=n())
table_summary_yield<-summary_median%>%group_by(zone)%>%summarise(AverageYield = mean(yield), StdDev = sd(yield))
write.csv(table_summary_yield, file = "summary_yield_by_zone2.csv")

summary_median_high_rainfall_years<-semi_join(summary_median, high_rainfall_years,  by = c("zone", "year") )   
summary_median_high_rainfall_years$Rainfall<-'High'
summary_median_low_rainfall_years<-semi_join(summary_median, low_rainfall_years,  by = c("zone", "year") )   
summary_median_low_rainfall_years$Rainfall<-'Low'


#######################################################
##########    'Bassin arachidier'   #########################

MY_ZONE<-'Bassin arachidier'
combined_dataBA<-get_resampled_data(summary_median_high_rainfall_years, summary_median_low_rainfall_years, MY_ZONE)

BA_tile<-ggplot(combined_dataBA, aes(yield, color = Rainfall, fill=texture)) + geom_density(alpha = 0.2) +
  theme_classic() + ggtitle("Bassin Arachidier")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Yield(kg/ha)")+
  scale_fill_manual(name="Texture", labels=c("Loamy Sand", 'Sandy Clay Loam', 'Sandy Loam'), values=c("red", 'green', 'blue'))+
  scale_color_manual(name="Rainfall", values=c('red', 'blue'), labels = c("Highest 5 years", "Lowest 5 years"))


quantiledata<-combined_dataBA%>%filter(texture=='LoSa' & Rainfall=='Low')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)
quantiledata<-combined_dataBA%>%filter(texture=='LoSa' & Rainfall=='High')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)
quantiledata<-combined_dataBA%>%filter(texture=='SaLo' & Rainfall=='Low')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)
quantiledata<-combined_dataBA%>%filter(texture=='SaLo' & Rainfall=='High')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)
quantiledata<-combined_dataBA%>%filter(texture=='SaClLo' & Rainfall=='Low')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)
quantiledata<-combined_dataBA%>%filter(texture=='SaClLo' & Rainfall=='High')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)


###############################################################
########## SENEGAL ORIENTAL #####################
MY_ZONE<-'Senegal oriental'
combined_dataSO<-get_resampled_data(summary_median_high_rainfall_years, summary_median_low_rainfall_years, MY_ZONE)

SO_tile<-ggplot(combined_dataSO, aes(yield, color = Rainfall, fill=texture)) + geom_density(alpha = 0.2) +
  theme_classic() + ggtitle("Senegal Oriental")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Yield(kg/ha)")+
  scale_fill_manual(name="Texture", labels=c("Clay Loam", 'Sandy Clay Loam'), values=c("red", 'green'))+
  scale_color_manual(name="Rainfall", values=c('red', 'blue'), labels = c("Highest 5 years", "Lowest 5 years"))


quantiledata<-combined_dataSO%>%filter(texture=='ClLo' & Rainfall=='Low')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)
quantiledata<-combined_dataSO%>%filter(texture=='ClLo' & Rainfall=='High')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)

quantiledata<-combined_dataSO%>%filter(texture=='SaClLo' & Rainfall=='Low')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)
quantiledata<-combined_dataSO%>%filter(texture=='SaClLo' & Rainfall=='High')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)


###############################################################
########## CASAMANCE #####################
MY_ZONE<-'Casamance'
combined_dataCA<-get_resampled_data(summary_median_high_rainfall_years, summary_median_low_rainfall_years, MY_ZONE)
CA_tile<-ggplot(combined_dataCA, aes(yield, color = Rainfall, fill=texture)) + geom_density(alpha = 0.2) +
  theme_classic() + ggtitle("The Casamance")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Yield(kg/ha)")+
  scale_fill_manual(name="Texture", labels=c("Clay Loam",'Loam', 'Sandy Clay Loam'), values=c("red", 'green', 'blue'))+
  scale_color_manual(name="Rainfall", values=c('red', 'blue'), labels = c("Highest 5 years", "Lowest 5 years"))

quantiledata<-combined_dataCA%>%filter(texture=='ClLo' & Rainfall=='Low')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)
quantiledata<-combined_dataCA%>%filter(texture=='ClLo' & Rainfall=='High')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)
quantiledata<-combined_dataCA%>%filter(texture=='Lo' & Rainfall=='Low')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)
quantiledata<-combined_dataCA%>%filter(texture=='Lo' & Rainfall=='High')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)
quantiledata<-combined_dataCA%>%filter(texture=='SaClLo' & Rainfall=='Low')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)
quantiledata<-combined_dataCA%>%filter(texture=='SaClLo' & Rainfall=='High')
quantile(quantiledata$yield, probs=c(0.05, 0.5, 0.95))
sd(quantiledata$yield)



##############################################################################################
##  The following creates per zone plots without filtering out some weather years
### All weather years############

MY_ZONE<-'Bassin arachidier'
temp<-summary_median%>%filter(zone==MY_ZONE)
temp$Rainfall<-'All'
combined_dataBA<-get_resample_means(temp, MY_ZONE)

combined_dataBA$yield<-combined_dataBA$yield/median(summary_median$yield)
quantile(combined_dataBA$yield, probs=c(0.05, 0.95) )
BA_tile<-ggplot(combined_dataBA, aes(yield,  fill=texture)) + geom_density(alpha = 0.2) +
  theme_minimal()+ facet_wrap(c('site') )+
  labs(x="Yield(kg/ha)")+
  scale_fill_manual(name="Texture", labels=c("Loamy Sand", 'Sandy Clay Loam', 'Sandy Loam'), values=c("red", 'green', 'blue'))
  

MY_ZONE<-'Senegal oriental'
temp<-summary_median%>%filter(zone==MY_ZONE)
temp$Rainfall<-'All'
combined_dataSO<-get_resample_means(temp, MY_ZONE)
#quantile(combined_dataSO$yield, probs=c(0.05, 0.95) )
SO_tile<-ggplot(combined_dataSO, aes(yield,  fill=texture)) + geom_density(alpha = 0.2) +
  theme_minimal()+ facet_wrap(c('site') )+
  labs(x="Yield(kg/ha)")+
  scale_fill_manual(name="Texture", labels=c("Clay Loam", 'Sandy Clay Loam', 'Sandy Loam'), values=c("red", 'green', 'blue'))
  


MY_ZONE<-'Casamance'
temp<-summary_median%>%filter(zone==MY_ZONE)
temp$Rainfall<-'All'
combined_dataCA<-get_resample_means(temp, MY_ZONE)#combined_dataCA$yield<-combined_dataCA$yield/median(summary_median$yield)
CA_tile<-ggplot(combined_dataCA, aes(yield,  fill=texture)) + geom_density(alpha = 0.2) +
  theme_minimal()+ facet_wrap(c('site') )+
  labs(x="Yield(kg/ha)")+
  scale_fill_manual(name="Texture", labels=c("Clay Loam",'Loam', 'Sandy Clay Loam'), values=c("red", 'green', 'blue'))
  

quantile(combined_dataCA$yield, probs=c(0.05, 0.95) )

combined_data<-rbind(combined_dataBA, combined_dataCA, combined_dataSO)
temp<-summary_median%>%filter(zone %in% c('Bassin arachidier', 'Casamance', 'Senegal oriental'))
combined_data$yield<-combined_data$yield/mean(combined_data$yield)

Combine_tile<-ggplot(combined_data, aes(yield, fill=site)) + geom_density(alpha = 0.2) +
  theme_minimal()+
  labs(x="Yield(as % of sample mean)")
quantile(combined_dataCA$yield, probs=c(0.05, 0.95) )
precip_by_year=precip_by_year%>%filter(zone %in% c('Bassin arachidier', 'Casamance', 'Senegal oriental'))%>%mutate(zone = as.character(zone, label = TRUE))
ggplot(precip_by_year, 
       aes(y=average_precip, x=zone , group=zone)) +geom_boxplot()
precip_by_year%>%group_by(zone)%>%summarise(precip=mean(average_precip), sd=sd(average_precip))



###########################################
###  Create table of soil textures by zone
soil_texture_summaryByzone<-summary_median%>%group_by(zone, texture_class)%>%summarise(count=n(), clay=mean(upper_60_clay_ave), sand=mean(upper_60_sand_ave), silt=mean(upper_60_silt_ave), carbon=mean(upper_60_soc_ave))
write.csv(soil_texture_summaryByzone, file='soil_texture_summaryByzone.csv')
