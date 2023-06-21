######################################################
#######FIRST RESULT FIGURE FOR SENEGAL CROP MODEL
#########  COMBINED YIELD DISTRIBUTION FRO ALL 3 ZONES
########################################################
################

############################################################

#############FUNCTIONS#########################


get_resample_means_all_years=function(mydata, zone){
  means<-NULL
  rows<-3000
  means <- data.frame(matrix(ncol = 2, nrow = rows))
  #provide column names
  colnames(means) <- c('yield',  'site' )
  
  for(i in 1:rows){
    means$yield[i] = mean(sample(mydata$yield, 30, replace = FALSE, prob = NULL))
    means$site  = zone
  }
  return(means)
}

####################################################
####### Start with summary_median as from precipMinMaxYears.R

##C:/SenegalGIS/crop_model_result/fromsharecomputer
summary_median<-read.csv(file = 'fullrun_summary_median.csv')

zones=c('Bassin arachidier', 'Senegal oriental', 'The Casamance'  )

MY_ZONE<-'Bassin arachidier'
summary_zone<-summary_median%>%filter(zone==MY_ZONE)
combined_data<-get_resample_means_all_years(summary_zone, MY_ZONE)
MY_ZONE<-'Senegal oriental'
summary_zone<-summary_median%>%filter(zone==MY_ZONE)
combined_data<-rbind( combined_data, get_resample_means_all_years(summary_zone, MY_ZONE) )
MY_ZONE<-'Casamance'
summary_zone<-summary_median%>%filter(zone==MY_ZONE)
summary_zone$zone<-'The Casamance'
MY_ZONE<-'The Casamance'
combined_data<-rbind( combined_data, get_resample_means_all_years(summary_zone, MY_ZONE) )
tile<-ggplot(combined_data, aes(yield, fill=site)) + geom_density(alpha = 0.2) +
  labs(x="Yield(kg/ha)")+
  ggtitle("Simulated yield distributions by agroecozone")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(labels = ordered(zones), values = c("yellow",'green',  "blue"), name = "Agroecozones")

MY_ZONE<-''