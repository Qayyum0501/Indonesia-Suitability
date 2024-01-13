library (sf)
library (terra)
library(tmap)
library (dplyr)

#Load Data Power Plant
indonesia<-read_sf('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Indonesia Boundary\\idn_admbnda_adm1_bps_20200401.shp')
grid<-read_sf('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Grid\\grid.geojson')
road<-read_sf('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Road Indonesia\\IDN_roads.shp')
solar_power<-rast('final_selection3.tif')


#Existing Power Plant
powerplant_global<-read.csv('global_power_plant_database.csv')
idn_powerplant<-powerplant_global%>%
  filter(powerplant_global$country=="IDN")
idn_powerplant<-idn_powerplant[c(1:8)]
idn_powerplant<-filter(idn_powerplant,c(idn_powerplant$primary_fuel=="Coal"|idn_powerplant$primary_fuel=="Gas"|idn_powerplant$primary_fuel=="Oil"))
idn_powerplant_sf<-st_as_sf(idn_powerplant,coords=c("longitude","latitude"),crs=4326)

#Plot
tmap_mode('view')
tm_shape(indonesia)+tm_borders()+
tm_shape(idn_powerplant_sf)+
  tm_dots(col='primary_fuel',size='capacity_mw',alpha=0.7,palette=c("red",'darkorange','darkblue'))+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.5,legend.title.size = 0.8)+
  tm_layout(main.title = "Fossil Fuels Power Plant")

#Renewable exsiting power plant
idn_renewable<-read.csv('renewable_existing.csv')
idn_renewable_sf<-st_as_sf(idn_renewable,coords=c("longitude","latitude"),crs=4326)
idn_renewable_sf<-filter(idn_renewable_sf,idn_renewable_sf$type=="solar")

tmap_mode('view')
tm_shape(indonesia)+tm_borders()+
  tm_shape(idn_renewable_sf)+
  tm_dots(col='type',palette=c("darkred",'darkorange','darkblue','purple'))+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.5,legend.title.size = 0.8)+
  tm_layout(main.title = "Fossil Fuels Power Plant")

#transformed crs into 4326
grid_transformed <- st_transform(grid, crs = 4326)

#Load the indonesia solar power plant
idn_proposal<-read.csv('proposed_solar_plant.csv')
idn_proposal_sf<-st_as_sf(idn_proposal,coords=c("longitude","latitude"),crs=4326)
idn_proposal_sf$distances<-st_distance(idn_proposal_sf$geometry,grid_transformed$geometry)
idn_proposal_sf$distances<-apply(idn_proposal_sf$distances,1,min)

#check the distance of power plant into grid
idn_proposal_sf$distance=idn_distance
idn_proposal$min_distance <- apply(df[, c("distance[, 1]", "distance[, 2]", "distance[, 3]")], 1, min)

#Plot tmap grid & indonesia
tmap_mode('view')
tm_shape(solar_power)+tm_raster(style='cont',
                                palette='viridis',
                                alpha=0.7)+
  tm_shape(indonesia)+tm_borders()+
  tm_shape(idn_proposal_sf)+tm_dots(size=0.05,col='red')+
  tm_shape(grid)+tm_lines(col='blue')+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.8,legend.title.size = 0.8)

 