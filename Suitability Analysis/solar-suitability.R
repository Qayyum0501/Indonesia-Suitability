library(terra)
library (sf)
library(tmap)
library(tidyverse)

#Function
quick_mask_raster <- function(raster_data, masking_vector){
  masking_vector <- st_transform(masking_vector, st_crs(raster_data))
  masked_raster_data <- mask(raster_data, masking_vector)
  return(masked_raster_data)
}

#FUNCTION
radiation_to_power <- function(G, CA=1, SF=0.7, p=0.16, hours=1){
  kWh <- G * CA * SF * p 
  return(kWh)
}


#Load Data___________________________________________________________
indonesia<-read_sf('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Indonesia Boundary\\idn_admbnda_adm1_bps_20200401.shp')
st_bbox(indonesia)
raster_template = rast( resolution = 0.005,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
ind_solar<-rast("solar_2020_2022.tif")
ind_grid_line<-rast('Grid_network.tif')
ind_water_body<-rast('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Water Body Data\\waterIndonesia.tif')
Elevation<-rast('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Elevation\\IDN_alt.vrt')

land_cover<-rast('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Land Cover\\IDN_cov.vrt')
ind_road<-rast('Indonesia_road_classify.tif')
ind_pop<-rast('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Population\\population.tif')
ind_protected_area<-read_sf('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Protected Planet\\WDPA_WDOECM_Nov2023_Public_IDN_shp-polygons.shp')
ind_temp<-rast('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Temperature\\TEMP.tif')
ind_peatland<-read_sf('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Peatland\\Indonesia_peat_lands.shp')

#Classification of criteria_______________________________________________

#Solar
ind_solar<-ind_solar*365
ind_solar<-resample(ind_solar,raster_template)
solar_m<- matrix(c(0, 1500,  1,
                   1500,1800, 2,
                   1800,2500,3), ncol=3, byrow=TRUE)
solar_rc <- classify(ind_solar, solar_m, include.lowest=TRUE)


#Temperature above 2m
ind_temp_crop<-crop(ind_temp,raster_template)
ind_temp_resample<-resample(ind_temp_crop,raster_template)
plot(ind_temp_resample)
temp_matrix<-matrix (c(0, 20, 3,
                    20,25,2,
                    25, 30,1), ncol=3, byrow= TRUE)
ind_temp_class<-classify(ind_temp_resample,temp_matrix,include.lowest=TRUE)
ind_temp_mask<-quick_mask_raster(ind_temp_class,indonesia)
plot(ind_temp_mask)

#Grid Indonesia (Vector)
st_crs(ind_grid)$units
ind_grid_1km<-st_buffer(ind_grid,1000)
ind_grid_1km_raster<-rasterize(ind_grid$geometry,LCSS)

library(osmdata)
coords<-matrix(c(95.01079,141.01940,-11.00762, 6.07693),byrow = TRUE,nrow =2,ncol = 2, dimnames = list(c('x','y'),c('min','max')))
location <- opq(coords)

powerplant = add_osm_feature(location, key = 'power', value = 'line')
powerplant.sf = osmdata_sf(powerplant)
tm_shape(powerplant.sf$osm_lines)+tm_lines(col='voltage')

ind_grid<-rasterize(powerplant.sf$osm_lines,raster_template)
ind_grid_1km<-st_buffer(powerplant.sf$osm_lines,1000)
ind_grid_1km_raster<-rasterize(ind_grid_1km,raster_template)
ind_grid_5km<-st_buffer(powerplant.sf$osm_lines,5000)
ind_grid_5km_raster<-rasterize(ind_grid_5km,raster_template)
ind_grid_10km<-st_buffer(powerplant.sf$osm_lines,10000)
ind_grid_10km_raster<-rasterize(ind_grid_10km,raster_template)

ind_grid_combined<- c(ind_grid_1km_raster, ind_grid_5km_raster,ind_grid_10km_raster)
names(ind_grid_combined)=c("three","two","one")
plot(ind_grid_combined)
ind_grid_combined_df<-as.data.frame(ind_grid_combined,XY=TRUE)
one_grid<-which(ind_grid_combined_df$one==1)
two_grid<-which(ind_grid_combined_df$two==1)
three_grid<-which(ind_grid_combined_df$three==1)

ind_grid_combine_fix=LCSS
values(ind_grid_combine_fix)=NA
ind_grid_combine_fix[one_grid]=1
ind_grid_combine_fix[two_grid]=2
ind_grid_combine_fix[three_grid]=3

ind_grid_combine_fix<-quick_mask_raster(ind_grid_combine_fix,indonesia)
plot(ind_grid_combine_fix)

writeRaster(ind_grid_combine_fix,"Grid_network.tif")

#Road
st_crs(ind_road)$units
plot(ind_road$geometry)
ind_road_raster<-rasterize(ind_road$geometry,LCSS)
plot(ind_road_raster)

#Road Buffer area of Indonesia
ind_road_25km<-st_buffer(ind_road,25000)
ind_road_25km_raster<-rasterize(ind_road_25km,raster_template)
ind_road_50km<-st_buffer(ind_road,50000)
ind_road_50km_raster<-rasterize(ind_road_50km,raster_template)
plot(ind_road_50km_raster)


ind_road_combined<- c(ind_road_25km_raster, ind_road_50km_raster)
names(ind_road_combined)=c("two","one")
plot(ind_road_combined)
ind_road_combined_df<-as.data.frame(ind_road_combined,XY=TRUE)
one<-which(ind_road_combined_df$one==1)
two<-which(ind_road_combined_df$two==1)

ind_road_combine_fix=LCSS
values(ind_road_combine_fix)=NA
ind_road_combine_fix[two]=2
ind_road_combine_fix[one]=1
plot(ind_road_combine_fix)

writeRaster(ind_road_combine_fix,"Indonesia_road_classify.tif")

#Body Water
ind_water_body<-crop(ind_water_body,raster_template)
ind_water_body<-resample(ind_water_body,raster_template)

#Peatland
ind_peat_raster<-rasterize(ind_peatland,raster_template)
plot(ind_peat_raster)
ind_peat_resample<-resample(ind_peat_raster,raster_template)

plot(ind_peat_resample)

#Elevation Indonesia
plot(Elevation)
elevation_crop<-crop(Elevation,raster_template)
elevation_sampling<-resample(elevation_crop,raster_template)
slope<-terrain(elevation_sampling,v='slope',unit='degrees')
plot(slope)
slope_m<- matrix(c(0, 10,  2,
                   10, 55, 1), ncol=3, byrow=TRUE)
elevation_rc <-classify(slope, slope_m, include.lowest=TRUE )
elevation_rc<-quick_mask_raster(elevation_rc,indonesia)
plot(elevation_rc)

#Land Cover
plot(land_cover)
land_crop<-crop(land_cover,raster_template)
land_crop<-resample(land_crop,raster_template)
r_matrix<-matrix (c(0, 10, 0,
                    10,15,1,
                    15, 16,2,
                    16, 20, 3,
                    20, 25, 4), ncol=3, byrow= TRUE)
land_potential<-classify(land_crop,r_matrix,include.lowest=TRUE)
land_potential<-quick_mask_raster(land_potential,indonesia)
LCSS=land_potential

#Population/Settlement
ind_pop<-crop(ind_pop,raster_template)
ind_pop<-resample(ind_pop,raster_template)
pop_matrix<-matrix (c(0, 0.4,0,
                    0.4,1,1), ncol=3, byrow= TRUE)
ind_pop<-classify(ind_pop,pop_matrix,include.lowest=TRUE)
plot(ind_pop)

#Protected Planet
ind_protected_area_raster<-rasterize(ind_protected_area,raster_template)
ind_protected_area_raster[is.na(ind_protected_area_raster)]<-0

##PLOT______________________________________________________________
tmap_mode('plot')
#SOLAR
tm_shape(solar_rc)+tm_raster(style='cat',alpha=0.7,palette = 'YlOrRd')+
  tm_shape(indonesia)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.03'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size = 1)+
  tm_layout(main.title = "Average yearly Solar Radiance (kWh/m2)")

#ELEVATION
tmap_mode('plot')
tm_shape(elevation_rc)+tm_raster(style='cat',alpha=0.7)+
  tm_shape(indonesia)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size = 1)+
  tm_layout(main.title = "Slope")

#Protected Area
tm_shape(indonesia)+tm_borders(col='black')+
  tm_shape(ind_protected_area_raster)+tm_raster(style='cat',alpha = 0.7,palette = 'darkgreen')+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size = 1)+
  tm_layout(main.title = "Protected Area")

#Land Cover
tm_shape(LCSS)+tm_raster(style='cat',
                         palette=c("forestgreen",'darkorange','darkblue','pink','lightblue')
                         ,alpha=0.7,labels = c('Forests','Herbaceous cover','Rice Field','Cropland','Water Body'))+
  tm_shape(indonesia)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.5,legend.title.size = 0.8)+
  tm_layout(main.title = "Land Cover of Indonesia")

#Grid and Road Network
tmap_mode('plot')
tm_shape(ind_road_combine_fix)+tm_raster(style='cat',palette=c("darkblue",'darkorange'),alpha=0.7)+
  tm_shape(indonesia)+tm_borders(col='black')+
  tm_shape(ind_road)+tm_lines(col='red')+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.5,legend.title.size = 0.8)+
  tm_layout(main.title = "Road Proximity")

#Grid Network
tmap_mode('plot')
tm_shape(ind_grid_combine_fix)+tm_raster(style='cat',palette=c("red",'darkorange','darkgreen'),alpha=0.7)+
  tm_shape(indonesia)+tm_borders(col='black')+
  tm_shape(ind_grid_line)+tm_lines(col='blue')+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.5,legend.title.size = 0.8)+
  tm_layout(main.title = "Grid Network Proximity")

#Temperature
tmap_mode('plot')
tm_shape(ind_temp_mask)+tm_raster(style='cat',alpha=0.7,palette = 'viridis')+
  tm_shape(indonesia)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size = 1)+
  tm_layout(main.title = "Temperature at 2m")

#Peatland
tmap_mode('plot')
tm_shape(ind_peat_resample)+tm_raster(palette = 'darkgreen')+
  tm_shape(indonesia)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size = 1)+
  tm_layout(main.title = "Temperature at 2m")

#Constrained map_________________________________
constrained_map<-c(ind_protected_area_raster,LCSS,ind_pop,ind_peat_resample)
names(constrained_map)=c("Protected Area","Land Used","Settlement","Peatland")
plot(constrained_map)
constrained_map_df=as.data.frame(constrained_map,XY=TRUE)
id = which(constrained_map_df$`Protected Area`==1|
             constrained_map_df$`Land Used`==0|
             constrained_map_df$`Land Used`==2|
             constrained_map_df$Settlement==1|
             constrained_map_df$Peatland==1)
constrained=LCSS
values(constrained)=NA
constrained[id]=1
constrained_mask<-quick_mask_raster(constrained,indonesia)
plot(constrained)

writeRaster(constrained_mask,"constrained_mask.tif")

#Check the are of constrained
ncell(constrained[id])/sum(!is.na(values(LCSS)))

#Plot constrained map area
tmap_mode('plot')
tm_shape(constrained)+tm_raster(style='cat',palette='darkblue',labels = 'Restricted',alpha = 0.7)+
  tm_shape(indonesia)+tm_borders(col='black')+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.5,legend.title.size = 0.8)+
  tm_layout(main.title = "Restricted Area of Indonesia")

#SUITABLE AREA_____________________________________________________
suitable_area<-c(ind_temp_mask,ind_grid_line,ind_road,solar_rc,elevation_rc,constrained_mask)
names(suitable_area)<-c('temperature','grid','road','radiance','slope','constrained')
plot(suitable_area)
suitable_area_df=as.data.frame(suitable_area,XY=TRUE)
suitable_area_df=mutate_all(suitable_area_df,~ifelse(is.na(.),0,.))
suitable_area_df$suitability=round((((0.05*suitable_area_df$temperature+0.28*suitable_area_df$grid+0.09*suitable_area_df$road+0.44*suitable_area_df$radiance+0.15*suitable_area_df$slope)/(2.762992))*100),0)

cons=which(suitable_area_df$constrained!=0)
suit=which(suitable_area_df$suitability>0)

suitability=LCSS
values(suitability)=NA
suitability[suit]=suitable_area_df$suitability[suit]
suitability[cons]=NA
suitabilit_mask<-quick_mask_raster(suitability,indonesia)
plot(suitabilit_mask)

suitability_mask_df<-as.data.frame(suitabilit_mask,xy=TRUE)
count_non_missing1 <- sum(!is.na(suitability_mask_df$IDN_cov))
print(count_non_missing1/sum(!is.na(values(LCSS))))

#Plot Suitability
tmap_mode('plot')
tm_shape(suitabilit_mask)+tm_raster(style='cont',
                         palette='viridis'
                         ,alpha=0.7)+
  tm_shape(indonesia)+tm_borders()+
  tm_text ("ADM1_EN", size=.5, col='black', shadow=TRUE, fontface="bold")+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.5,legend.title.size = 0.5)+
  tm_layout(main.title = "Solar Farm Suitability Index")

#SOLAR PANEL Capacity_______________________________________________________________________
plot(ind_solar)
rad_power=(ind_solar*0.7*0.16)/365
plot(rad_power)
max(rad_power)

solar_power=LCSS
values(solar_power)=NA
power=which(suitable_area_df$suitability>70)
notpower=which(suitable_area_df$suitability<70|suitable_area_df$constrained!=0)
solar_power[power]=rad_power[power]
solar_power[notpower]=NA
plot(solar_power)

#Check Average Yield
solar_power_df=as.data.frame(solar_power,xy=TRUE)
average_solar=mean(solar_power_df$IDN_cov,na.rm=TRUE)
print(average_solar)

#Save the file
library(openxlsx)
write.xlsx(solar_power_df, "solarpowerplant1.xlsx")

#final Selection of Area Indonesia
count_non_missing <- sum(!is.na(solar_power_df$IDN_cov))
print(count_non_missing/sum(!is.na(values(LCSS))))

solar_power
#Plot Final selection of Area Indonesia
tmap_mode('view')
tm_shape(solar_power)+tm_raster(style='cont',
                                palette='viridis',
                                alpha=0.7)+
  tm_shape(indonesia)+tm_borders()+
  tm_text ("ADM1_EN", size=.4, col='black', shadow=TRUE, fontface="bold")+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.5,legend.title.size = 0.5)+
  tm_layout(main.title = "Solar Farm Suitability Index")

writeRaster(solar_power,"final_selection3.tif")

