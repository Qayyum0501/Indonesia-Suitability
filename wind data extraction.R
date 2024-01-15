library(ncdf4)
library(chron)
library(gstat)
library(spatstat)
library(terra)
library(gstat)
library(sf)
library(tmap)

#LOAD DATA________________
indonesia<-st_read('C:\\Users\\Qayyum Hamidi Alius\\Dropbox\\UCL\\ESDA 2023\\BENV0093_Spatial Data\\Assignment\\Indonesia Data\\Indonesia Boundary\\idn_admbnda_adm1_bps_20200401.shp')
raster_template = rast( resolution = 0.005,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)

wind_era <- nc_open("wind_data.nc")
lon <- ncvar_get(wind_era, "longitude")
lat <- ncvar_get(wind_era, "latitude")
time <- ncvar_get(wind_era, "time")
time

#get the unit of data
tunits <- ncatt_get(wind_era,"time","units")

#Convert Time
tustr <- strsplit(tunits$value, " ") #strsplit: split the element of character vector. we can convert  "hours since 1900-01-01" to "hours"      "since"      "1900-01-01"
tdstr <- strsplit(unlist(tustr)[3], "-") #convert "1900-01-01" to "1900" "01"   "01"
tyear <- as.integer(unlist(tdstr)[1]) 
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
chron(time/24, origin=c(tmonth, tday, tyear))

wind_u10_array <- ncvar_get(wind_era,"u10")
wind_v10_array <- ncvar_get(wind_era,"v10")


u10_slice <- wind_u10_array[,,24]
v10_slice <- wind_u10_array[,,24]
wind = sqrt ( u10_slice**2 + v10_slice**2) #calculate the speed

lonlat <- as.matrix( (expand.grid(lon, lat)))
wind_vec <- as.vector( wind)
wind_df <- data.frame( cbind( lonlat,wind_vec  ))
colnames(wind_df) <- c("lon", "lat", "wind")
wind_df_value <- na.omit (wind_df)

wind_sf<- st_as_sf( wind_df_value, coords = c("lon", "lat"))
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(wind_sf) <- 4326 
wind_sf = st_transform(wind_sf, 4326)

coor = as.data.frame(st_coordinates(wind_sf))
wind_sf$x = coor$X
wind_sf$y = coor$Y
wind_nogeom = st_drop_geometry(wind_sf) #get rid of geometry but keep all other attributes
wind_nogeom=na.omit(wind_nogeom)

gs <- gstat(formula=wind~1, locations=~x+y, data=wind_nogeom, nmax=Inf, set=list(idp=7))
idw <- interpolate(raster_template, gs, debug.level=0)#interpolate is the function comes with terra
#idw_results[[as.character(i)]] <- idw

plot(idw$var1.var)

wind_mask<-mask(idw,indonesia)

plot(wind_mask$var1.pred)
names(wind_mask) = c("predicted","observed")
wind_unmasked<-idw$var1.pred

writeRaster(wind_unmasked,"wind_unmasked.tif")

null <- RMSE(mean(wind_sf$wind), wind_sf$wind)
null

n_idp = 20 #examine power ranging from 1 to 20
n_fold =10

rmse <- rep(NA, n_fold) #generate 10 NA
set.seed(7713)
kf <- sample(1:n_fold, nrow(wind_nogeom), replace=TRUE)
va = data.frame( c(1:n_idp), NA)
colnames(va) =c("idp","rmse") 



for (j in 1:n_idp) 
{
  for (i in 1:n_fold) {
    test <- wind_nogeom[kf == 1, ]
    train <- wind_nogeom[kf != 1, ]
    gs <- gstat(formula=wind~1, locations=~x+y, data=train, nmax=Inf, set=list(idp=j))
    pre = predict(gs, test, debug.level=0 )
    rmse[i] <- RMSE(test$wind, pre$var1.pred)
  }
  va[j,2] = (mean(rmse) )
}

va[which(va$rmse==min(va)),]


library(ggplot2)
ggplot(va) +
  geom_point(aes(x = idp, y= rmse))+
  geom_hline(yintercept=min(va), linetype="dashed", color = "red")+
  theme_classic()
