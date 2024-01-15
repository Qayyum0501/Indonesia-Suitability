library(ncdf4)
library(chron)
library(gstat)
library(spatstat)
library(terra)
library(gstat)
library(lattice)
library(RColorBrewer)
library(sf)
library(tmap)
tmap_mode("plot")

#FUNCTION
radiation_to_power <- function(G, A=1, r=1, p=1, hours=1){
  kWh <- G * A * r * p * (hours/3600) / 1000
  return(kWh)
}
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

#LOAD DATA
indonesia<-st_read('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Indonesia Boundary\\idn_admbnda_adm1_bps_20200401.shp')
era <- nc_open("Solar5.nc")
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)


#Code Start
era#3 dimensions [longitude,latitude,time] 

#get Dimension :lon, lat, time
lon <- ncvar_get(era, "longitude")
lat <- ncvar_get(era, "latitude")
time <- ncvar_get(era, "time")
time

#get Dimension :lon, lat, time
dim(time)
dim(lat)

#get the unit of data
tunits <- ncatt_get(era,"time","units")

#Convert Time
tustr <- strsplit(tunits$value, " ") #strsplit: split the element of character vector. we can convert  "hours since 1900-01-01" to "hours"      "since"      "1900-01-01"
tdstr <- strsplit(unlist(tustr)[3], "-") #convert "1900-01-01" to "1900" "01"   "01"
tyear <- as.integer(unlist(tdstr)[1]) 
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
chron(time/24, origin=c(tmonth, tday, tyear))

`#Extract attribute values
ssrd_array <- ncvar_get(era,"ssrd") #get the Surface solar radiation downwards
dim(ssrd_array)
dlname <- ncatt_get(era,"ssrd","long_name")
dunits <- ncatt_get(era,"ssrd","units")
fillvalue <- ncatt_get(era,"ssrd","_FillValue")


idw_results<-list()
for(i in 1:180) {
  ##Slice Data, take specific time
  ssrd_slice<-ssrd_array[,,i]
  length(na.omit(as.vector(ssrd_slice)))/length(as.vector(ssrd_slice))
  max_rad <- max(ssrd_slice, na.rm=TRUE)
 
    ##Combine data & Visualise
  lonlat <- as.matrix( (expand.grid(lon, lat)))
  ssrd_vec <- as.vector( ssrd_slice) 
  ssrd_df <- data.frame(cbind(lonlat,ssrd_vec))
  colnames(ssrd_df) <- c("lon", "lat", "ssrd")
  ssrd_df_value <- na.omit(ssrd_df)
  ssrd_sf<- st_as_sf( ssrd_df_value, coords = c("lon", "lat"))
  st_crs(ssrd_sf) <- 4326 
  ssrd_sf <- st_transform(ssrd_sf, 4326 )
  ncatt_get(era,"ssrd","units")
  
  ssrd_kwh <- as.data.frame (radiation_to_power (ssrd_df_value))
  ssrd_df_value <- cbind(ssrd_df_value,ssrd_kwh$ssrd)
  colnames(ssrd_df_value) [4] <- 'ssrd_kwh'
  ssrd_sf$ssrd_kwh = ssrd_kwh$ssrd

  solar_sf<-st_transform(ssrd_sf,4326)
  coor=as.data.frame(st_coordinates(solar_sf))
  solar_sf$x=coor$X
  solar_sf$y=coor$Y
  solar_nogeom=st_drop_geometry(solar_sf)
  solar_nogeom=na.omit(solar_nogeom)
  gs <-gstat(formula=ssrd_kwh~1, locations=~x+y, data=solar_nogeom, nmax=Inf,set=list(idp=7))
  #Interpolate
  idw <- interpolate(raster_template, gs, debug.level=0)
  idw_results[[as.character(i)]] <- idw
}

January<-(idw_results[[1]]+idw_results[[2]]+idw_results[[3]]+idw_results[[4]])/4
january_mask<-mask(January,indonesia)
plot(january_mask)

tmap_mode('plot')
  tm_shape(january_mask$var1.pred)+tm_raster(style='cont',palette = "viridis", legend.show = TRUE)+
  tm_shape(indonesia)+tm_borders(col='black')+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.5,legend.title.size = 0.8)+
  tm_layout(main.title = "Solar Radiance of January 2022 (kWh/m2)")


July<-(idw_results[[31]]+idw_results[[32]]+idw_results[[33]]+idw_results[[34]])/4
july_mask<-mask(July,indonesia)
plot(july_mask)

  tm_shape(july_mask$var1.pred)+tm_raster(style='cont',palette = "viridis", legend.show = TRUE)+
  tm_shape(indonesia)+tm_borders(col='black')+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.5,legend.title.size = 0.8)+
  tm_layout(main.title = "Solar Radiance of July 2022 (kWh/m2)")



average_idw <- Reduce(`+`, idw_results) / length(idw_results)
plot(average_idw$var1.pred, main = "Average IDW Result")

solar_mask<-mask(average_idw,indonesia)
solar_mask
plot(solar_mask$var1.pred)
names(solar_mask) = c("predicted","observed")

tm_shape(solar_mask$predicted)+
  tm_raster(col="predicted", style = "cont", palette = "viridis", legend.show = TRUE)+
  tm_shape(indonesia)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.5,legend.title.size = 0.8)+
  tm_layout(main.title = "Long Term Yearly Average 2020-2022 (kWh/m2/day)")


#Plot Solar
tm_shape(solar_mask$predicted)+tm_raster(col='predicted',style='quantile',n=5, palette="YlOrRd",alpha=0.7)+
  tm_shape(indonesia)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size = 0.8)+
  tm_scale_bar(position=c('0.7','0.05'),width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size = 1)+
  tm_credits("Datasource:cds.climate.copernicus.eu", position=c('0.14','0.03'),size = 0.7)+
  tm_layout(main.title = "Average Yearly 2020 Global Horizontal Irradiance (kWh/m2)")

writeRaster(solar_mask$predicted,"solar_2020_2022.tif")

null <- RMSE(mean(solar_sf$ssrd_kwh), solar_sf$ssrd_kwh)
null 

n_idp = 20 #examine power ranging from 1 to 20
n_fold =10

rmse <- rep(NA, n_fold) #generate 10 NA
set.seed(7713)
kf <- sample(1:n_fold, nrow(solar_nogeom), replace=TRUE)
va = data.frame( c(1:n_idp), NA)
colnames(va) =c("idp","rmse") 

##Check RMSE
for (j in 1:n_idp) 
{
  for (i in 1:n_fold) {
    test <- solar_nogeom[kf == 1, ]
    train <- solar_nogeom[kf != 1, ]
    gs <- gstat(formula=ssrd_kwh~1, locations=~x+y, data=train, nmax=20, set=list(idp=j))
    pre = predict(gs, test, debug.level=0 )
    rmse[i] <- RMSE(test$ssrd_kwh, pre$var1.pred)
  }
  va[j,2] = (mean(rmse) )
}

va[which(va$rmse==min(va)),]

library(ggplot2)
ggplot(va) +
  geom_point(aes(x = idp, y= rmse))+
  geom_hline(yintercept=min(va), linetype="dashed", color = "red")+
  theme_classic()
