library(terra)
library (sf)
library(tmap)
library(tidyverse)
library(openxlsx)
install.packages('rgdal')
library(rgdal)

#Function
quick_mask_raster <- function(raster_data, masking_vector){
  masking_vector <- st_transform(masking_vector, st_crs(raster_data))
  masked_raster_data <- mask(raster_data, masking_vector)
  return(masked_raster_data)
}

calc_turbine_speed <- function(speed, turbine_height=70, data_height=10, hellman_exponent=1/7){
  turbine_speed <-  speed*(turbine_height/data_height)^hellman_exponent
  return(turbine_speed)
}

#define a curve that indicate the relationship between speed and wind
speed_to_power <- function(speed){ 
  wind_speed= seq(from=0, to=24, by=1)
  power = c(0,  0,  0,  102,  257,  471,  750,  1194, 1713,  2227,  2465,  2500,  2500,  2500,  2500,  2500,  2500,  2500,2500,  2500,  2500,  2500,  2500, 2400,  2250)
  wind_convert = data.frame(wind_speed, power)
  speed= round(speed) +1
  power=wind_convert[speed, "power"]
  return(power)
}

#Load Data
ind_wind<-rast('wind_unmasked.tif')
plot(ind_wind)
sea_bond<-read_sf('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Sea Boundary\\eez.shp')
indonesia_bond<-st_read('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Indonesia Boundary\\idn_admbnda_adm1_bps_20200401.shp')
raster_template = rast( resolution = 0.005,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia_bond)$wkt)
ind_grid_line<-rast('Grid_network.tif')
ind_road<-rast('Indonesia_road_classify.tif')
ind_water<-rast('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Water Depth\\1.tif')
constrained<-rast('constrained_mask.tif')

#Wind Classification
ind_wind
plot(ind_wind)
wind_m<- matrix(c(0, 4,  1,
                   4,5, 2,
                   5,Inf,3), ncol=3, byrow=TRUE)
wind_rc<- classify(ind_wind, wind_m, include.lowest=TRUE)
plot(wind_rc)
wind_rc_offshore<-quick_mask_raster(wind_rc,sea_bond)
plot(wind_rc_offshore)
wind_rc_onshore<-quick_mask_raster(wind_rc,indonesia_bond)

#Water Depth
plot(ind_water)
names(ind_water)<-'water_depth'
ind_water_mask<-quick_mask_raster(ind_water,sea_bond)
ind_water_resample<-resample(ind_water_mask,raster_template)
plot(ind_water_resample)
ind_water_resample

ind_water_mask_df<-as.data.frame(ind_water_resample,XY=TRUE)
# Set the desired depth range
a = -50  # 50 meters below sea level
b = -10  # 10 meters below sea level

# Find the cells within the desired depth range
depth <- which(ind_water_mask_df$water_depth >= a & ind_water_mask_df$water_depth <= b)

water_suit <- ind_water_resample
values(water_suit)<- NA
water_suit[depth]<- 1
plot(water_suit)

tmap_mode('plot')

#WIND Before Classification
tm_shape(ind_wind)+tm_raster(style='cont',alpha=0.7,palette = 'BuPu')+
  tm_shape(indonesia_bond)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.03'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size = 1)


#WIND
tm_shape(wind_rc_onshore)+tm_raster(style='cat',alpha=0.7,palette = 'viridis')+
  tm_shape(indonesia_bond)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.03'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size = 1)

#BATTERY DEPTH
tm_shape(water_suit)+tm_raster(style='cat',alpha=0.7,palette = 'magma')+
  tm_shape(indonesia_bond)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.03'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size = 1)

#Wind suitability Area
suitability<-c(ind_grid_line,ind_road,wind_rc,water_suit,constrained)
names(suitability)<-c('grid','road','wind','depth','constrained')
plot(suitability)

suitability_df<-as.data.frame(suitability,XY=TRUE)
suitability_df=mutate_all(suitability_df,~ifelse(is.na(.),0,.))
suitability_df$suitability<-round(((((suitability_df$grid*0.14)+(suitability_df$road*0.06)+(suitability_df$wind*0.49)+(suitability_df$depth*0.31))/2.31)*100),0)

cons=which(suitability_df$constrained==1)
suit=which(suitability_df$suitability>0)

suit_map=water_suit
values(suit_map)=NA
suit_map[suit] <- suitability_df$suitability[suit]
suit_map[cons]=NA

#Wind for offshore and onshore
wind_offshore<-quick_mask_raster(suit_map,sea_bond)
plot(wind_offshore)
wind_onshore<-quick_mask_raster(suit_map,indonesia_bond)

#Plot suitability map
suit_map
tmap_mode('view')
tm_shape(wind_offshore)+tm_raster(palette = 'viridis',style = 'pretty')+
  tm_shape(sea_bond)+tm_borders(col='black')+
  tm_shape(wind_onshore)+tm_raster(palette = 'viridis',style = 'pretty')+
  tm_shape(indonesia_bond)+tm_borders(col='black')

#Convert wind speed into power
wind_power_df<-as.data.frame(ind_wind,XY=TRUE)
wind_power_df$speed<-calc_turbine_speed(wind_power_df$var1.pred)
wind_power_df$power<-speed_to_power(wind_power_df$speed)

#make a new map consist of wind power
convert=which(wind_power_df$power>0)
power<-raster_template
values(power)<-NA
power[convert]<-wind_power_df$power[convert]
plot(power)

#separate of wind power for onshore and offshore
onshore<-quick_mask_raster(power,indonesia_bond)
plot(onshore)
offshore<-quick_mask_raster(power,sea_bond)
plot(offshore)

plot(wind_offshore)
wind_offshore_df<-as.data.frame(wind_offshore,XY=TRUE)

#Find the power of wind offshore based on suitability map 
suit1<-which(wind_offshore_df$water_depth>60)
offshore_final<-raster_template
values(offshore_final)<-NA
offshore_final[suit1]<-wind_power_df$power[suit1]
plot(offshore_final)

#Find the power of wind onshore based on suitability map
wind_onshore_df<-as.data.frame(wind_onshore,XY=TRUE)
suit2<-which(wind_onshore_df$water_depth>60)
onshore_final<-raster_template
values(onshore_final)<-NA
onshore_final[suit2]<-wind_power_df$power[suit2]
plot(onshore_final)

#Find the average offshore wind power and total area
offshore_final_df<-as.data.frame(offshore_final,xy=TRUE)
average_wind_capacity=mean(offshore_final_df$lyr.1,na.rm=TRUE)
Total_Capacity=sum(offshore_final_df$lyr.1,na.rm=TRUE)
Total_Capacity
average_wind_capacity
count_offshore<- sum(!is.na(offshore_final_df$lyr.1))
print(count_offshore/sum(!is.na(values(wind_rc_offshore))))

#Find the average onshore wind power and total area
onshore_final_df<-as.data.frame(onshore_final,xy=TRUE)
Total_Capacity_onshore=sum(onshore_final_df$lyr.1,na.rm=TRUE)
wind_average_onshore=mean(onshore_final_df$lyr.1,na.rm=TRUE)
Total_Capacity_onshore
wind_average_onshore
count_onshore<- sum(!is.na(onshore_final_df$lyr.1))
print(count_onshore/sum(!is.na(values(wind_rc_onshore))))

#save the file
writeRaster(offshore_final,'offshore_final.tif')
writeRaster(onshore_final,'onshore_final.tif')

offshore_final<-rast('offshore_final.tif')
onshore_final<-rast('onshore_final.tif')

plot(offshore_final)

#save the file of dataframe
write.xlsx(offshore_final_df, "C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Exercise 2\\wind_plant1.xlsx")
write.xlsx(onshore_final_df,"C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Exercise 2\\wind_plant_onshore.xlsx")

#Plot map
tmap_mode('view')
tm_shape(offshore_final)+tm_raster(palette = "BuPu",style = 'pretty')+
  tm_shape(sea_bond)+tm_borders(col='black')+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)

tm_shape(onshore_final)+tm_raster(palette = "BuPu",style = 'pretty')+
  tm_shape(sea_bond)+tm_borders(col='black')+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)

tm_shape(onshore_final)+tm_raster(palette = 'BuPu',style = 'pretty')+
  tm_shape(indonesia_bond)+tm_borders(col='black')
