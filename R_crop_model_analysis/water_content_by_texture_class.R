library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(soiltexture)

textures_all<-c("LoSa" , "SaClLo" ,  "SaLo",   "Lo"   ,  "ClLo"  , "Cl"  ,   "SiCl" )
textures_all_long<-c( "Loamy Sand" , "Sandy Clay Loam" , "Sandy Loam",   "Loam"   ,  "Clay Loam"  , "Clay"  ,   "Silty Clay" )
textures_CS<-c( 'ClLo', 'Lo', 'SaClLo')
textures_CS_long<-c( 'Clay Loam','Loam', 'Sandy Clay Loam')
textures_BA<-c( 'LoSa', 'SaClLo','SaLo')
textures_BA_long<-c('Loamy Sand', 'Sandy Clay Loam','Sandy Loam' )
textures_SO<-c('ClLo','SaClLo' )
textures_SO_long<-c('Clay Loam', 'Sandy Clay Loam')

setwd('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\FINAL_code_output\\R_crop_model_analysis\\input\\')
summary2<-read.csv('fullrun_summary_median.csv')

zone_texture_count<-summary2%>%filter(year==2010)%>%group_by(zone, texture_class)%>%summarise(count=n())
colnames(summary2)<-c('X' , "soil_id","year" ,"zone","texture_class","season_yield", "clay" ,"sand" ,"soc" , "silt", "precip" , "planting_date" , "yield" )


summary_no_year<-summary2%>%group_by(soil_id, zone, texture_class)%>%summarise(yield=mean(yield), clay=mean(clay), sand=mean(sand ), soc=mean(soc ), silt=mean(silt),precip=mean(precip) )

millet_flux<-read.csv('millet_FLUX_fullrunV2.csv')
millet_growth<-read.csv('millet_GROWTH_fullrunV2.csv')
millet_growth$yield<-round(100*millet_growth$season_yield,2)
growth_join<-merge(millet_growth, summary2, by = c('soil_id', 'year', 'planting_date'))
growth_join<-growth_join%>%filter(biomass_nostress>0)
growth_join$pct_biomass<-growth_join$biomass/growth_join$biomass_nostress
#Remove some erron
millet_flux<-millet_flux%>%filter(water_content<1800)

time_series_water_by_texture<-merge(millet_flux, summary_no_year, by = c('soil_id', 'zone'))
time_series_water_by_texture$pct_soil_evap=time_series_water_by_texture$SoilEvap/time_series_water_by_texture$PotSoilEv

time_series_water_by_texture_dailyagg<-time_series_water_by_texture%>%group_by(site_id, DAP, texture_class)%>%summarise(yield=mean(yield), clay=mean(clay), silt=mean(silt),soc=mean(soc),sand=mean(sand), water_content=mean(water_content), DailyPrecip=mean(DailyPrecip),DailyTrans=mean(DailyTrans), SoilEvap=mean(SoilEvap),PctSoilEvap=mean(pct_soil_evap), precip=mean(precip))

####################### PLOTS OF WATER CONTENT BY SAND RATIO ##########################
plot_data<-data.frame(time_series_water_by_texture_dailyagg%>%filter(site_id==6 & texture_class %in% textures_BA))

yield1<-plot_data%>%filter(texture_class=='LoSa')%>%select(yield)
clay1<-plot_data%>%filter(texture_class=='LoSa')%>%select(clay)
yield2<-plot_data%>%filter(texture_class=='SaLo')%>%select(yield)
clay2<-plot_data%>%filter(texture_class=='SaLo')%>%select(clay)
yield3<-plot_data%>%filter(texture_class=='SaClLo')%>%select(yield)
clay3<-plot_data%>%filter(texture_class=='SaClLo')%>%select(clay)
label_text1<-paste("Avg Yield:" , round(yield1[1,1], 2) ,  "kg" , "\nAvg.Clay:" ,round(clay1[1,1], 0) / 10 ,  "%")
label_text2<-paste("Avg Yield:" , round(yield2[1,1], 2) ,  "kg" , "\nClay:" ,round(clay2[1,1], 0) / 10 ,  "%")
label_text3<-paste("Avg Yield:" , round(yield3[1,1], 2) ,  "kg" , "\nClay:" ,round(clay3[1,1], 0) / 10 ,  "%")

BA_plot <- ggplot(data=plot_data,
                  aes(x=DAP, y=water_content, group = texture_class, colour = texture_class)) + geom_line() +
  labs(y= "Water content in soil column (mm)", x = "Days after planting")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Water content by texture: Bassin Arachdier") +
  scale_color_manual(labels = ordered(textures_BA_long), values = c("red", "blue", "green"), name = "Texture")+
  annotate("text", x=47, y=45, label= label_text3, size=3)+
  annotate("text", x=35, y=32, label= label_text2, size=3)+
  annotate("text", x=25, y=20, label= label_text1, size=3)


trans_plot_data<-data.frame(time_series_water_by_texture%>%filter(site_id==6 & texture_class %in% textures_BA))%>%
  group_by(soil_id, texture_class, clay)%>%summarise(trans=sum(DailyTrans))

BA_Trans_plot<-ggplot(data=trans_plot_data,
                     aes(x=clay/10, y=trans, colour = texture_class)) + geom_point() +
  scale_color_manual(labels = textures_BA_long, values = c("red", "blue", "green"), name = "Texture")+
  labs(y= "Seasonal soil evaporation (mm)", x = "Clay percentage in upper 60 cm")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Seasonal transpiration and clay content") 

biomass_plot_data<-data.frame(growth_join%>%filter(site_id==6 & texture_class %in% textures_BA))%>%
  group_by(soil_id, texture_class, clay)%>%summarise(pct_biomass=mean(pct_biomass))

BA_Biomass_plot<-ggplot(data=biomass_plot_data,
                        aes(x=clay/10, y=pct_biomass, colour = texture_class)) + geom_point() +
  scale_color_manual(labels = textures_BA_long, values = c("red", "blue", "green"), name = "Texture")+
  labs(y= "Biomass percent of potential", x = "Clay percentage in upper 60 cm")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Biomass and clay content") 

########Soil Evap BA############
BA_plot <- ggplot(data=plot_data, aes(x=DAP)) + 
  geom_line(aes(y=SoilEvap, group = texture_class, colour = texture_class)) +
  labs( x = "Days after planting")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  scale_y_continuous(
    name = "Soil evaporation (mm)")+
  ggtitle("Soil Evaporation by texture: Bassin Arachidier") +
  scale_color_manual(labels = textures_BA_long, values = c("red", "blue", "green"), name = "Texture")

BA_plot <- ggplot(data=plot_data, aes(x=DAP)) + 
  geom_line(aes(y=DailyTrans, group = texture_class, colour = texture_class)) +
  labs( x = "Days after planting")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  scale_y_continuous(
    name = "Transpiration (mm)")+
  ggtitle("Crop Transpiration  by texture: Bassin Arachidier") +
  scale_color_manual(labels = textures_BA_long, values = c("red", "blue", "green"), name = "Texture")




plot_data<-data.frame(time_series_water_by_texture_dailyagg%>%filter(site_id==3 & texture_class %in% textures_CS))

yield1<-plot_data%>%filter(texture_class==textures_CS[1])%>%select(yield)
clay1<-plot_data%>%filter(texture_class==textures_CS[1])%>%select(clay)
yield2<-plot_data%>%filter(texture_class==textures_CS[2])%>%select(yield)
clay2<-plot_data%>%filter(texture_class==textures_CS[2])%>%select(clay)
yield3<-plot_data%>%filter(texture_class==textures_CS[3])%>%select(yield)
clay3<-plot_data%>%filter(texture_class==textures_CS[3])%>%select(clay)
label_text1<-paste("Avg Yield:" , round(yield1[1,1], 2) ,  "kg" , "\nAvg.Clay:" ,round(clay1[1,1], 0) / 10 ,  "%")
label_text2<-paste("Avg Yield:" , round(yield2[1,1], 2) ,  "kg" , "\nClay:" ,round(clay2[1,1], 0) / 10 ,  "%")
label_text3<-paste("Avg Yield:" , round(yield3[1,1], 2) ,  "kg" , "\nClay:" ,round(clay3[1,1], 0) / 10 ,  "%")

CS_plot <- ggplot(data=plot_data,
                  aes(x=DAP, y=water_content, group = texture_class, colour = texture_class)) + geom_line() +
  labs(y= "Water content in soil column (mm)", x = "Days after planting")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Water content by texture: Casamance") +
  scale_color_manual(labels = textures_CS_long, values = c("red", "blue", "green"), name = "Texture")+
  annotate("text", x=10, y=90, label= label_text2, size=3)+
  annotate("text", x=65, y=107, label= label_text1, size=3)+
  annotate("text", x=40, y=77, label= label_text3, size=3)

biomass_plot_data<-data.frame(growth_join%>%filter(site_id==3 & texture_class %in% textures_CS))%>%
  group_by(soil_id, texture_class, clay)%>%summarise(pct_biomass=mean(pct_biomass))

CS_Biomass_plot<-ggplot(data=biomass_plot_data,
                        aes(x=clay/10, y=pct_biomass, colour = texture_class)) + geom_point() +
  scale_color_manual(labels = textures_CS_long, values = c("red", "blue", "green"), name = "Texture")+
  labs(y= "Biomass percent of potential", x = "Clay percentage in upper 60 cm")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Biomass and clay content") 


evap_plot_data<-data.frame(time_series_water_by_texture%>%filter(site_id==3 & texture_class %in% textures_CS))%>%
  group_by(soil_id, texture_class, clay)%>%summarise(soil_evap=sum(SoilEvap))

CS_Evap_plot<-ggplot(data=evap_plot_data,
                     aes(x=clay/10, y=soil_evap/10, colour = texture_class)) + geom_point() +
  scale_color_manual(labels = textures_CS_long, values = c("red", "blue", "green"), name = "Texture")+
  labs(y= "Seasonal soil evaporation (mm)", x = "Clay percentage in upper 60 cm")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Soil evaporation and clay content") 


plot_data<-data.frame(time_series_water_by_texture_dailyagg%>%filter(site_id==4 & texture_class %in% textures_SO))

yield1<-plot_data%>%filter(texture_class==textures_SO[1])%>%select(yield)
clay1<-plot_data%>%filter(texture_class==textures_SO[1])%>%select(clay)
yield2<-plot_data%>%filter(texture_class==textures_SO[2])%>%select(yield)
clay2<-plot_data%>%filter(texture_class==textures_SO[2])%>%select(clay)
label_text1<-paste("Avg Yield:" , round(yield1[1,1], 2) ,  "kg" , "\nAvg.Clay:" ,round(clay1[1,1], 0) / 10 ,  "%")
label_text2<-paste("Avg Yield:" , round(yield2[1,1], 2) ,  "kg" , "\nClay:" ,round(clay2[1,1], 0) / 10 ,  "%")

SO_plot <- ggplot(data=plot_data,
                  aes(x=DAP, y=water_content, group = texture_class, colour = texture_class)) + geom_line() +
  labs(y= "Water content in soil column (mm)", x = "Days after planting")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Water content by texture: Senegal Oriental") +
  scale_color_manual(labels = textures_SO_long, values = c("red", "blue", "green"), name = "Texture")+
  annotate("text", x=7, y=70, label= label_text1, size=3)+
  annotate("text", x=25, y=50, label= label_text2, size=3)

evap_plot_data<-data.frame(time_series_water_by_texture%>%filter(site_id==4 & texture_class %in% textures_SO))%>%
  group_by(soil_id, texture_class, clay)%>%summarise(soil_evap=sum(SoilEvap))
  
SO_Evap_plot<-ggplot(data=evap_plot_data,
               aes(x=clay/10, y=soil_evap, colour = texture_class)) + geom_point() +
  scale_color_manual(labels = textures_SO_long, values = c("red", "blue", "green"), name = "Texture")+
  labs(y= "Seasonal soil evaporation (mm)", x = "Clay percentage in upper 60 cm")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Soil evaporation and clay content") 

biomass_plot_data<-data.frame(growth_join%>%filter(site_id==4 & texture_class %in% textures_SO))%>%
  group_by(soil_id, texture_class, clay)%>%summarise(pct_biomass=mean(pct_biomass))

SO_Biomass_plot<-ggplot(data=biomass_plot_data,
                     aes(x=clay/10, y=pct_biomass, colour = texture_class)) + geom_point() +
  scale_color_manual(labels = textures_SO_long, values = c("red", "blue", "green"), name = "Texture")+
  labs(y= "Biomass percent of potential", x = "Clay percentage in upper 60 cm")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Biomass and clay content") 

####################################################
###PLOT transpiration with soil evaporation

SO_plot <- ggplot(data=plot_data, aes(x=DAP)) + 
  geom_line(aes(y=SoilEvap, group = texture_class, colour = texture_class)) +
  labs( x = "Days after planting")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  scale_y_continuous(
    name = "Soil evaporation (mm)")+
  ggtitle("Soil Evaporation by texture: Senegal Oriental") +
  scale_color_manual(labels = textures_SO_long, values = c("red", "blue", "green"), name = "Texture")


#######################  END PLOTS Water content by sand######################################


######################PLOT all flux on one plot########################
plot_data_all<-melt(time_series_water_by_texture%>%select(site_id, DAP, DailyPrecip, DailyTrans, SoilEvap, water_content, texture_class), id.vars=c("site_id", "DAP", "texture_class") )
plot_data_all<-plot_data_all%>%filter(value<101)
plot_data_all<-plot_data_all%>%filter(value>-101)
plot_data_all$Variable_wSandLevel<-paste(plot_data_all$variable, ":", plot_data_all$texture_class)
plot_data<-data.frame(plot_data_all%>%filter(site_id==0 & DAP<60 & variable!='water_content'))

ZS_flux_plot <- ggplot(data=plot_data,
                  aes(x=DAP, y=value,  colour = Variable_wSandLevel )) + geom_line() +
  labs(y= "Water flux (mm)", x = "DAP")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Water Flux Growing Season:Zone Sylvopasture") 

plot_data<-data.frame(plot_data_all%>%filter(site_id==6 & DAP<60 & variable!='DailyPrecip' & texture_class %in% textures_BA))

BA_flux_plot <- ggplot(data=plot_data,
                       aes(x=DAP, y=value,  colour = Variable_wSandLevel)) + geom_line() +
  labs(y= "Water flux (mm)", x = "DAP")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Water Flux Growing Season:Bassin Arachdier") 

plot_data<-data.frame(plot_data_all%>%filter(site_id==3 & DAP<60 & variable!='DailyPrecip' & texture_class %in% textures_CS))

CS_flux_plot <- ggplot(data=plot_data,
                       aes(x=DAP, y=value,  colour = Variable_wSandLevel)) + geom_line() +
  labs(y= "Water flux (mm)", x = "DAP")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Water Flux Growing Season:Casamance") 

plot_data<-data.frame(plot_data_all%>%filter(site_id==4 & DAP<60 & variable!='DailyPrecip' & texture_class %in% textures_SO))

East_flux_plot <- ggplot(data=plot_data,
                       aes(x=DAP, y=value,  colour = Variable_wSandLevel)) + geom_line() +
  labs(y= "Water flux (mm)", x = "DAP")  + theme_bw(base_size = 16) + theme(axis.text.x=element_text(angle=60,hjust=1)) +
  ggtitle("Water Flux Growing Season:East") 
