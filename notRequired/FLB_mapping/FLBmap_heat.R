## Clean up this script to plot flowshed for manuscript summary figure
#Edited script to do a habitat comparison over multiple ndays
# https://github.com/eqmh/waltonsmith
# ----Check which packages we actually use----
# Find which packages do used functions belong to ----
used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Desktop/forAna.R", alphabetic = FALSE) |> print()

# Find which loaded packages are not used ----
used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
unused.packages <- packageload[!(packageload %in% used.packages)] |> print()


# rm(list=ls())
library(stringr)
library(fields)
# library(dplyr)
library(ggmap)
library(tidyr)
library(factoextra)
library(sf)
library(mapview)
library(geosphere) #distance between points
# library(vegan)
library(ggfortify)
# library(lwgeom)
library(gridExtra)
library(ggpubr)
library(patchwork)
library(cowplot)
library(PNWColors)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(viridis)
library(ggnewscale)
library(ggpubr)
library(ggOceanMaps)
library(tidyterra)
library(cmocean)
library(PNWColors)
library(ggOceanMaps)
library(tidyterra)
library(cmocean)
library(paletteer)
library(colorspace)
library(RColorBrewer)
library(ghibli)
library(htmlwidgets)
library(sf)
library(hddtools)
library(magick)
library(gtools)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(ggrepel)
library(rstudioapi)
library(stringr)
library(sp)
library(raster)
library(mapview)
library(leaflet)
library(dplyr)
library(ggplot2)
library(webshot)
library(cowplot)
library(wesanderson)
library(pals)
library(ggstar)
library(Polychrome)
library(rcartocolor) #use safe for colorblind safe colors

library(fields)
library(lubridate)
library(ncdf4)
# library(NISTunits)
library(raster)
library(rgdal)
library(magrittr)


register_google(key = "AIzaSyBSmQdRPOV8usMAOEkkoH6MISzFjTe5AoE") #new api Feb 2024

bathy=st_read("/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/mesh_florida",layer="mesh_florida")
bathy$bathymetry.log = log(bathy$bathymetry) #log the depth so that the scale works later

FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')

#Read in carbonate chemistry station data
# CC = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/CCflk_plusBathy.csv',na.strings=c(""," "))
flb9 = read.csv('/Users/heidi.k.hirsh/Desktop/FLBcruises/FLBsept2024.csv')
flb9$Latitude=flb9$Lat
flb9$Longitude=flb9$Lon
FLB9 = st_as_sf(flb9, coords= c("Lon","Lat"),crs = st_crs(4326)) 
class(FLB9)

#bring in september carbonate chemistry data
FLBcc= read.csv('/Users/heidi.k.hirsh/Desktop/heidi-query-01.csv')
FLBcc$Station = FLBcc$collected_site

sort(FLBcc$collected_site)
sort(FLB9$Station)
#yay they match

names(FLBcc)
names(FLB9)
#add CC data to FLB9 (match by Station/collected_site) - actually do the reverse

# FLBall = left_join(FLBcc,FLB9,by="Station")
FLBall =  left_join(FLB9,FLBcc,by="Station")
head(FLBall)
class(FLBall)


##pair november data: 
FLBccN = read.csv('/Users/heidi.k.hirsh/Desktop/heidi-query-02.csv')
FLBccN$Station = FLBcc$collected_site
flb11 = read.csv('/Users/heidi.k.hirsh/Desktop/FLSF_Environmental_202411_raw_bottlesHH.csv')
##I NEED COORDINATES!
flb11$Latitude=flb11$Lat
flb11$Longitude=flb11$Lon
FLB11 = st_as_sf(flb11, coords= c("Lon","Lat"),crs = st_crs(4326)) 
class(FLB11)
#pair by collected site


#crop to bathymetry to desired size
xlims = c(-82.5,-79.8)
ylims = c(24.2,26)

box_coords <- tibble(x = xlims, y = ylims) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(st_crs(4326))
# box_coords
bounding_box <- st_bbox(box_coords) %>% st_as_sfc()
bounding_box


st_crs(bathy)==st_crs(bounding_box) #not the same
#change bathy crs

bathy.sf = st_transform(bathy, st_crs(4326))
st_crs(bathy.sf)==st_crs(bounding_box) #true!

bathy_subset <- st_intersection(bathy.sf, bounding_box)

bath.breaks=c(log(1000),log(200), log(50),log(10),log(0))
bath.labs=c('1000','200','50',"10",'0')


bayBox = st_bbox(FLB9) %>% st_as_sfc(st_crs=4326) %>% st_sf %>% st_cast
plot(bayBox)
class(bayBox)


bayPoints=ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  # geom_point(data=FLB9,aes(x=Longitude,y=Latitude,color=ODO.mgL),pch=18,size=3)+
  # geom_point(data=FLBall,aes(x=Longitude,y=Latitude,color=calc_avg_dic_kg_corrected),pch=18,size=3)+
  # geom_point(data=FLBall,aes(x=Longitude,y=Latitude,color=calc_avg_ta_kg_corrected),pch=18,size=3)+
  geom_point(data=FLBall,aes(x=Longitude,y=Latitude,color=corrected_ph),pch=18,size=3)+
  # scale_colour_gradientn(colors = rainbow(8)) +
  ylab('Latitude')+
  xlab('Longitude')+
  coord_sf(xlim = c(-82.1 , -80), ylim = c(24.4, 26), expand = FALSE) + #for backward halos
  theme_bw()
bayPoints


#add bathy
bayBath = ggplot() +
  geom_sf(data=bathy_subset, aes(fill=bathymetry.log),lwd=0)+#color=NA)+ 
  scale_fill_cmocean(name="deep",breaks=bath.breaks, labels=bath.labs)+
  geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+
  geom_point(data = FLBall, aes(x = Longitude, y = Latitude, color=calc_avg_ta_kg_corrected),size=2)+ 
  # geom_point(data = FLBall, aes(x = Longitude, y = Latitude, color=calc_avg_dic_kg_corrected),size=2)+ 
  # geom_point(data = FLBall, aes(x = Longitude, y = Latitude, color=corrected_ph),size=2)+ 
  # geom_point(data = FLBall, aes(x = Longitude, y = Latitude, color=ODO.mgL),size=2)+ 
  scale_colour_gradientn(colors = rainbow(8)) +
  ylab("Longitude")+
  xlab("Latitude")+
  coord_sf(xlim = c(-82.1 , -80), ylim = c(24.4, 26), expand = FALSE) + #for backward halos
  theme_bw()
bayBath

bayBathZ = ggplot() +
  # geom_sf(data=bathy_subset, aes(fill=bathymetry.log),lwd=0)+#color=NA)+ 
  geom_sf(data=bathy_subset, aes(fill=bathymetry),lwd=0)+#color=NA)+  #depth scale is based on map before coord limits? 
  
  scale_fill_cmocean(name="deep")+
  geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+
  # geom_point(data = FLB9, aes(x = Longitude, y = Latitude, color=ODO.mgL),size=2)+ 
  # geom_point(data = FLBall, aes(x = Longitude, y = Latitude, color=calc_avg_dic_kg_corrected),size=2)+ 
  # geom_point(data = FLBall, aes(x = Longitude, y = Latitude, color=calc_avg_ta_kg_corrected),size=2)+
  geom_point(data = FLBall, aes(x = Longitude, y = Latitude, color=corrected_ph),size=2)+
  # scale_color_viridis_c()+
  scale_colour_gradientn(colors = rainbow(8)) +
  ylab("Longitude")+
  xlab("Latitude")+
  # coord_sf(xlim = c(-81.3 , -80.3), ylim = c(24.6, 25.3), expand = FALSE) + #for backward halos
  coord_sf(xlim = c(-81.2 , -80.5), ylim = c(24.95, 25.3), expand = FALSE) + 
  theme_bw()
bayBathZ

#use ggmap to plot parameter over satellite image 
FLK_map=get_map(location=c(-80.8,25.1),zoom=10,maptype = "satellite")
ggmap(FLK_map)


#doesn't work - does contours based on point density? 
ggmap(FLK_map)+
  geom_density2d(data= FLBall, aes(x=Longitude,y=Latitude),color='gray')+
  # geom_density2d(data= FLBall, aes(x=Longitude,y=Latitude,group=corrected_ph),color='gray')+
  # stat_density2d(data= FLBall, aes(x=Longitude,y=Latitude,fill=corrected_ph))+ #,alpha=corrected_ph),size = 0.01, bins = 16, geom = 'polygon') +
  # scale_fill_gradient(low = "green", high = "red") +
  geom_point(data = FLBall, aes(x=Longitude,y=Latitude),size=1)+
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))+
  theme_bw()



# x_coord <- c(1,2,3,4)
# y_coord <- c(1,2,3,4)
# value <- c(12,15,19,30)
# foo <- data.frame(x_coord, y_coord, value)
# library(MBA)
# foo=foo[ order(foo[,1], foo[,2],foo[,3]), ]
# class(foo)
# mba.int <- mba.surf(foo, 300, 300, extend=T)$xyz.est
# library(fields)
# fields::image.plot(mba.int)

#so I need to make a surface? 


##Try kriging
library(gstat)   # For kriging
library(sp)      # For spatial data handling
library(sf)      # For modern spatial data formats
library(raster)  # For raster processing
library(ggplot2) # For visualization
#simplify dataframe
#FLBall is already a spatial object 
crs(FLBall)

# Define the spatial extent and grid resolution
grd <- spsample(as(extent(FLBall), "SpatialPolygons"), type = "regular", n = 10000)
gridded(grd) <- TRUE
proj4string(grd) <- CRS("+proj=longlat +datum=WGS84")
# crs(FLBall)==crs(grd)

# Fit an empirical variogram
variogram_model <- variogram(corrected_ph ~ 1, FLBall)
plot(variogram_model)

# Fit a theoretical variogram - this doesn't work
# fit_variogram <- fit.variogram(variogram_model, model = vgm("Sph")) # try Exp or Gau
fit_variogram <- fit.variogram(variogram_model, model = vgm("Exp"))
plot(variogram_model, fit_variogram)




###### try following fields vignette: https://github.com/NCAR/fields/blob/master/fieldsVignette.pdf

x = cbind(FLBall$Longitude, FLBall$Latitude)
y = FLBall$corrected_ph
quilt.plot(x,y)
# world(add=TRUE)
US(add=TRUE)

#try in ggplot (this is pretty good for a simple points map)
df = data.frame(FLBall$Longitude, FLBall$Latitude, FLBall$corrected_ph)
names(df) = c("Longitude","Latitude","pH")
us = map_data("usa")
ggplot()+
  geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+
  coord_sf(xlim = c(-81.2, -80.5), ylim = c(24.95, 25.3), expand = FALSE) + 
  # geom_polygon(data=us, aes(x=long,y=lat,group=group),fill=NA,color='black')+
  geom_point(aes(x=Longitude,y=Latitude,color=pH), data=df,size=2.5,shape=20)+
  scale_color_gradientn(colors=tim.colors())+
  labs(color="pH")+
  theme_bw()

#kriging ex (https://stackoverflow.com/questions/12990298/expanding-my-interpolation-grid-for-kriging-in-r?rq=4)

x.range <- range(FLBall$Longitude)
y.range <- range(FLBall$Latitude)
data.grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.005),y=seq(from=y.range[1], to=y.range[2], by=0.005))
data.grd
# x.range <- as.integer(range(kr.data@coords[,1]))
# y.range <- as.integer(range(kr.data@coords[,2]))
# data.grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.5),  
#                         y=seq(from=y.range[1], to=y.range[2], by=0.5))

coordinates(data.grd) <- ~x+y
gridded(data.grd) <- TRUE
plot(data.grd, cex=0.5)
points(FLBall, pch=1, col='red', cex=0.7)



#from BT
### krigging resolution
resolution <- .01
adj <- .1
### krigging locations
df_flb.loc <- cbind(lon=FLBall$Longitude,lat=FLBall$Latitude)
loc.grid_flb <- list(lon=seq(min(FLBall$Longitude, na.rm=T) - adj, max(FLBall$Longitude, na.rm=T) + adj, resolution),
                      lat=seq(min(FLBall$Latitude, na.rm=T) - adj, max(FLBall$Latitude, na.rm=T) + adj, resolution))
### limits for plotting
xlims_flb <- range(loc.grid_flb$lon)
ylims_flb <- range(loc.grid_flb$lat)

#kfibinb
my.krig_flb <- spatialProcess(df_flb.loc, FLBall$corrected_ph)
ratio_kriged_flb <- predictSurface(my.krig_flb, loc.grid_flb, extrap=F)
ratio_SE_flb <- predictSurfaceSE(my.krig_flb, loc.grid_flb, extrap=F)

### corrects the krigged min and max to observed min and max
if(max(ratio_kriged_flb$z, na.rm=T) > max(FLBall$corrected_ph, na.rm=T)){
  ratio_kriged_flb$z[which(ratio_kriged_flb$z > max(FLBall$corrected_ph, na.rm=T))] <- max(FLBall$corrected_ph, na.rm=T)
}
if(min(ratio_kriged_flb$z, na.rm=T) < min(FLBall$corrected_ph, na.rm=T)){
  ratio_kriged_flb$z[which(ratio_kriged_flb$z < min(FLBall$corrected_ph, na.rm=T))] <- min(FLBall$corrected_ph, na.rm=T)
}

### color and contour breaks FUCO
breaks_flb <- pretty(FLBall$corrected_ph, n=10)
# breaks_flb <- seq(0, 0.6, by=.05) ### use for fixed breaks.
# cols_flb <- chl_col(length(breaks_fuco)-1)


par(mfrow=c(1,1))

# par(mfrow=c(3,1),mar=c(4.5,4,2,1),oma=c(4,1,4,1))
imagePlot(ratio_kriged_flb$x,
          ratio_kriged_flb$y,
          ratio_kriged_flb$z,
          # col=cols_fuco,
          # breaks=breaks_flb,
          asp=1,
          xlab='',ylab='',las=1,
          xlim=xlims_flb,ylim=ylims_flb,
          # nlevel=length(cols_fuco),
          legend.width=1,legend.mar=3)
contour(ratio_kriged_flb$x,
        ratio_kriged_flb$y,
        ratio_kriged_flb$z,
        levels=breaks_flb,add=T)
image(ratio_SE_flb,add=T,breaks=quantile(ratio_SE_flb$z,c(1,1),na.rm=T),col='white')
# image(topo_lon,topo_lat,topo,breaks=c(-1,100),col='white',add=T)
# plot(FL,col='gray70',add=T)
# plot(world,col='gray70',add=T)
# contour(topo_lon,topo_lat,topo,add=T,levels=c(-100,-50,-25,-10),col='gray40')
points(FLBall$Longitude,FLBall$Latitude,pch=20,col='black')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3,cex=.75)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3,cex=.75)
mtext('FLB pH',adj=1,cex=.75)
  