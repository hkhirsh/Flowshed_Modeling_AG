#12/4/24

# https://github.com/eqmh/waltonsmith

# # Find which packages do used functions belong to ----
# used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Desktop/forAna.R", alphabetic = FALSE) |> print()
# 
# # Find which loaded packages are not used ----
# used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
# unused.packages <- packageload[!(packageload %in% used.packages)] |> print()


# rm(list=ls())
##Try kriging
library(gstat)   # For kriging
library(sp)      # For spatial data handling
library(sf)      # For modern spatial data formats
library(raster)  # For raster processing
library(ggplot2) # For visualization

library(stringr)
library(fields)
library(ggmap)
library(tidyr)
library(factoextra)
library(sf)
library(mapview)
library(geosphere) #distance between points
library(ggfortify)
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
library(raster)
# library(rgdal)
library(magrittr)


register_google(key = "AIzaSyBSmQdRPOV8usMAOEkkoH6MISzFjTe5AoE") #new api Feb 2024

# bathy=st_read("/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/mesh_florida",layer="mesh_florida")
# bathy$bathymetry.log = log(bathy$bathymetry) #log the depth so that the scale works later

FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')

#Need september FLB data for coordinates
flb9 = read.csv('/Users/heidi.k.hirsh/Desktop/FLBcruises/FLBsept2024.csv')
flb9$Latitude=flb9$Lat
flb9$Longitude=flb9$Lon
FLB9 = st_as_sf(flb9, coords= c("Lon","Lat"),crs = st_crs(4326)) 
head(FLB9)

#bring in september carbonate chemistry data
FLBcc= read.csv('/Users/heidi.k.hirsh/Desktop/heidi-query-01.csv')
FLBcc$Station = FLBcc$collected_site
# sort(FLBcc$collected_site)
# sort(FLB9$Station)
# names(FLBcc)
# names(FLB9)

#match by station:
FLBall =  left_join(FLB9,FLBcc,by="Station")
# head(FLBall)
# class(FLBall)

# #crop bathy to desired area
# xlims = c(-82.5,-79.8)
# ylims = c(24.2,26)
# 
# box_coords <- tibble(x = xlims, y = ylims) %>% 
#   st_as_sf(coords = c("x", "y")) %>% 
#   st_set_crs(st_crs(4326))
# 
# bounding_box <- st_bbox(box_coords) %>% st_as_sfc()
# 
# st_crs(bathy)==st_crs(bounding_box) #not the same
# bathy.sf = st_transform(bathy, st_crs(4326))
# st_crs(bathy.sf)==st_crs(bounding_box) #true!
# 
# bathy_subset <- st_intersection(bathy.sf, bounding_box)
# 
# bath.breaks=c(log(1000),log(200), log(50),log(10),log(0))
# bath.labs=c('1000','200','50',"10",'0')

bayBox = st_bbox(FLB9) %>% st_as_sfc(st_crs=4326) %>% st_sf %>% st_cast
# plot(bayBox)
# class(bayBox)

##plot points with csystem values
# bayPoints=ggplot(FLKs1)+
#   geom_sf(fill = "darkgray", lwd = 0)+
#   # geom_point(data=FLB9,aes(x=Longitude,y=Latitude,color=ODO.mgL),pch=18,size=3)+
#   # geom_point(data=FLBall,aes(x=Longitude,y=Latitude,color=calc_avg_dic_kg_corrected),pch=18,size=3)+
#   # geom_point(data=FLBall,aes(x=Longitude,y=Latitude,color=calc_avg_ta_kg_corrected),pch=18,size=3)+
#   geom_point(data=FLBall,aes(x=Longitude,y=Latitude,color=corrected_ph),pch=18,size=3)+
#   # scale_colour_gradientn(colors = rainbow(8)) +
#   ylab('Latitude')+
#   xlab('Longitude')+
#   coord_sf(xlim = c(-82.1 , -80), ylim = c(24.4, 26), expand = FALSE) + 
#   theme_bw()
# bayPoints


#use ggmap to plot parameter over satellite image 
# FLK_map=get_map(location=c(-80.8,25.1),zoom=10,maptype = "satellite")
# ggmap(FLK_map)







# # Define the spatial extent and grid resolution
# grd <- spsample(as(extent(FLBall), "SpatialPolygons"), type = "regular", n = 10000)
# gridded(grd) <- TRUE
# proj4string(grd) <- CRS("+proj=longlat +datum=WGS84")
# # crs(FLBall)==crs(grd)
# 
# # Fit an empirical variogram
# variogram_model <- variogram(corrected_ph ~ 1, FLBall)
# plot(variogram_model)
# 
# # Fit a theoretical variogram - this doesn't work
# # fit_variogram <- fit.variogram(variogram_model, model = vgm("Sph")) # try Exp or Gau
# fit_variogram <- fit.variogram(variogram_model, model = vgm("Exp"))
# plot(variogram_model, fit_variogram)


###### try following fields vignette: https://github.com/NCAR/fields/blob/master/fieldsVignette.pdf

# #this works for making a gridded plot of each value
# x = cbind(FLBall$Longitude, FLBall$Latitude)
# y = FLBall$corrected_ph
# quilt.plot(x,y)
# # world(add=TRUE)
# US(add=TRUE)

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

# #kriging ex (https://stackoverflow.com/questions/12990298/expanding-my-interpolation-grid-for-kriging-in-r?rq=4)
# 
# x.range <- range(FLBall$Longitude)
# y.range <- range(FLBall$Latitude)
# data.grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.005),y=seq(from=y.range[1], to=y.range[2], by=0.005))
# data.grd
# # x.range <- as.integer(range(kr.data@coords[,1]))
# # y.range <- as.integer(range(kr.data@coords[,2]))
# # data.grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.5),  
# #                         y=seq(from=y.range[1], to=y.range[2], by=0.5))
# 
# coordinates(data.grd) <- ~x+y
# gridded(data.grd) <- TRUE
# plot(data.grd, cex=0.5) #made a grid of the area?
# points(FLBall, pch=1, col='red', cex=0.7) #this one doesn't work


#from Brendan Turley's code

loc.grid_flb <- list(lon=seq(min(FLBall$Longitude, na.rm=T) - adj, max(FLBall$Longitude, na.rm=T) + adj, resolution),
                     lat=seq(min(FLBall$Latitude, na.rm=T) - adj, max(FLBall$Latitude, na.rm=T) + adj, resolution))

# pH KRIGGING
# png("FUCO_corrected_pH_plot.png", res = 300)
### krigging resolution 
resolution <- .001
# resolution <- .01
adj <- .1
### krigging locations
df_PH.loc <- cbind(lon=FLBall$Longitude,lat=FLBall$Latitude)
loc.grid_PH <- list(lon=seq(min(FLBall$Longitude, na.rm=T) - adj, max(FLBall$Longitude, na.rm=T) + adj, resolution),
                      lat=seq(min(FLBall$Latitude, na.rm=T) - adj, max(FLBall$Latitude, na.rm=T) + adj, resolution))
### limits for plotting
xlims_PH <- range(loc.grid_flb$lon)
ylims_PH <- range(loc.grid_flb$lat)

#kfibinb
my.krig_PH <- spatialProcess(df_PH.loc, FLBall$corrected_ph)
ratio_kriged_PH <- predictSurface(my.krig_PH, loc.grid_PH, extrap=F) #this was extrap=F
ratio_SE_PH <- predictSurfaceSE(my.krig_PH, loc.grid_PH, extrap=F)

### corrects the krigged min and max to observed min and max
if(max(ratio_kriged_PH$z, na.rm=T) > max(FLBall$corrected_ph, na.rm=T)){
  ratio_kriged_PH$z[which(ratio_kriged_PH$z > max(FLBall$corrected_ph, na.rm=T))] <- max(FLBall$corrected_ph, na.rm=T)
}
if(min(ratio_kriged_PH$z, na.rm=T) < min(FLBall$corrected_ph, na.rm=T)){
  ratio_kriged_PH$z[which(ratio_kriged_PH$z < min(FLBall$corrected_ph, na.rm=T))] <- min(FLBall$corrected_ph, na.rm=T)
}

### color and contour breaks FUCO
breaks_PH <- pretty(FLBall$corrected_ph, n=14)
# breaks_flb <- seq(0, 0.6, by=.05) ### use for fixed breaks.
# cols_flb <- chl_col(length(breaks_fuco)-1)

##This makes an image plot with contours but now context 
par(mfrow=c(1,1)) 
# reversed_colors <- rev(rainbow(100))
reversed_tim_colors <- rev(tim.colors(64))
# par(mfrow=c(3,1),mar=c(4.5,4,2,1),oma=c(4,1,4,1))
imagePlot(ratio_kriged_PH$x,
          ratio_kriged_PH$y,
          ratio_kriged_PH$z,
          # col=cols_fuco,
          # breaks=breaks_flb,
          col=reversed_tim_colors, 
          asp=1,
          xlab='',ylab='',las=1,
          # xlim=xlims_flb,ylim=ylims_flb,
          # nlevel=length(cols_fuco),
          legend.width=1,legend.mar=3)
contour(ratio_kriged_PH$x,
        ratio_kriged_PH$y,
        ratio_kriged_PH$z,
        levels=breaks_PH,add=T)
image(ratio_SE_PH,add=T,breaks=quantile(ratio_SE_PH$z,c(1,1),na.rm=T),col='white')
# image(topo_lon,topo_lat,topo,breaks=c(-1,100),col='white',add=T)
# plot(FL,col='gray70',add=T)
# plot(world,col='gray70',add=T)
# geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+
# plot(FLKs1,color="darkgray",add=T,border="")+ #this only does the outline no fill
# plot(FLKs1,col="darkgray",add=T)+
plot(FLKs1, col="darkgray", add=T, border=NA)

# contour(topo_lon,topo_lat,topo,add=T,levels=c(-100,-50,-25,-10),col='gray40')
points(FLBall$Longitude,FLBall$Latitude,pch=20,col='black')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3,cex=1)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3,cex=1)
mtext('corrected pH',adj=1,cex=.75)

# dev.copy(png, "FUCO_corrected_pH_plot.png", width = 8, height = 6, res = 300)
# dev.off()

#DIC KRIGGING
### krigging resolution 
resolution <- .001
# resolution <- .01
adj <- .1
### krigging locations
df_DIC.loc <- cbind(lon=FLBall$Longitude,lat=FLBall$Latitude)
loc.grid_DIC <- list(lon=seq(min(FLBall$Longitude, na.rm=T) - adj, max(FLBall$Longitude, na.rm=T) + adj, resolution),
                    lat=seq(min(FLBall$Latitude, na.rm=T) - adj, max(FLBall$Latitude, na.rm=T) + adj, resolution))
### limits for plotting
xlims_DIC <- range(loc.grid_flb$lon)
ylims_DIC <- range(loc.grid_flb$lat)

#kfibinb
my.krig_DIC <- spatialProcess(df_DIC.loc, FLBall$calc_avg_dic_kg_corrected)
ratio_kriged_DIC <- predictSurface(my.krig_DIC, loc.grid_DIC, extrap=F) #this was extrap=F
ratio_SE_DIC <- predictSurfaceSE(my.krig_DIC, loc.grid_DIC, extrap=F)

### corrects the krigged min and max to observed min and max
if(max(ratio_kriged_DIC$z, na.rm=T) > max(FLBall$calc_avg_dic_kg_corrected, na.rm=T)){
  ratio_kriged_DIC$z[which(ratio_kriged_DIC$z > max(FLBall$calc_avg_dic_kg_corrected, na.rm=T))] <- max(FLBall$calc_avg_dic_kg_corrected, na.rm=T)
}
if(min(ratio_kriged_DIC$z, na.rm=T) < min(FLBall$calc_avg_dic_kg_corrected, na.rm=T)){
  ratio_kriged_DIC$z[which(ratio_kriged_DIC$z < min(FLBall$calc_avg_dic_kg_corrected, na.rm=T))] <- min(FLBall$calc_avg_dic_kg_corrected, na.rm=T)
}

### color and contour breaks FUCO
breaks_DIC <- pretty(FLBall$calc_avg_dic_kg_corrected, n=14)
# breaks_flb <- seq(0, 0.6, by=.05) ### use for fixed breaks.
# cols_flb <- chl_col(length(breaks_fuco)-1)

##This makes an image plot with contours but now context 
par(mfrow=c(1,1)) 
# par(mfrow=c(3,1),mar=c(4.5,4,2,1),oma=c(4,1,4,1))
imagePlot(ratio_kriged_DIC$x,
          ratio_kriged_DIC$y,
          ratio_kriged_DIC$z,
          # col=cols_fuco,
          # breaks=breaks_flb,
          asp=1,
          xlab='',ylab='',las=1,
          # xlim=xlims_flb,ylim=ylims_flb,
          # nlevel=length(cols_fuco),
          legend.width=1,legend.mar=3)
contour(ratio_kriged_DIC$x,
        ratio_kriged_DIC$y,
        ratio_kriged_DIC$z,
        levels=breaks_DIC,add=T)
image(ratio_SE_DIC,add=T,breaks=quantile(ratio_SE_DIC$z,c(1,1),na.rm=T),col='white')
# image(topo_lon,topo_lat,topo,breaks=c(-1,100),col='white',add=T)
# plot(FL,col='gray70',add=T)
# plot(world,col='gray70',add=T)
# geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+
# plot(FLKs1,color="darkgray",add=T,border="")+ #this only does the outline no fill
# plot(FLKs1,col="darkgray",add=T)+
plot(FLKs1, col="darkgray", add=T, border=NA)

  
  # contour(topo_lon,topo_lat,topo,add=T,levels=c(-100,-50,-25,-10),col='gray40')
  points(FLBall$Longitude,FLBall$Latitude,pch=20,col='black')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3,cex=1)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3,cex=1)
mtext('Dissolved Inorganic Carbon (DIC)',adj=1,cex=.75)


#TA KRIGGING
### krigging resolution 
resolution <- .001
# resolution <- .01
adj <- .1
### krigging locations
df_TA.loc <- cbind(lon=FLBall$Longitude,lat=FLBall$Latitude)
loc.grid_TA <- list(lon=seq(min(FLBall$Longitude, na.rm=T) - adj, max(FLBall$Longitude, na.rm=T) + adj, resolution),
                     lat=seq(min(FLBall$Latitude, na.rm=T) - adj, max(FLBall$Latitude, na.rm=T) + adj, resolution))
### limits for plotting
xlims_TA <- range(loc.grid_flb$lon)
ylims_TA <- range(loc.grid_flb$lat)

#kfibinb
my.krig_TA <- spatialProcess(df_TA.loc, FLBall$calc_avg_ta_kg_corrected)
ratio_kriged_TA <- predictSurface(my.krig_TA, loc.grid_TA, extrap=F) #this was extrap=F
ratio_SE_TA <- predictSurfaceSE(my.krig_TA, loc.grid_TA, extrap=F)

### corrects the krigged min and max to observed min and max
if(max(ratio_kriged_TA$z, na.rm=T) > max(FLBall$calc_avg_ta_kg_corrected, na.rm=T)){
  ratio_kriged_TA$z[which(ratio_kriged_ta$z > max(FLBall$calc_avg_ta_kg_corrected, na.rm=T))] <- max(FLBall$calc_avg_ta_kg_corrected, na.rm=T)
}
if(min(ratio_kriged_TA$z, na.rm=T) < min(FLBall$calc_avg_ta_kg_corrected, na.rm=T)){
  ratio_kriged_TA$z[which(ratio_kriged_TA$z < min(FLBall$calc_avg_ta_kg_corrected, na.rm=T))] <- min(FLBall$calc_avg_ta_kg_corrected, na.rm=T)
}

### color and contour breaks FUCO
breaks_TA <- pretty(FLBall$calc_avg_ta_kg_corrected, n=14)
# breaks_flb <- seq(0, 0.6, by=.05) ### use for fixed breaks.
# cols_flb <- chl_col(length(breaks_fuco)-1)

##This makes an image plot with contours but now context 
par(mfrow=c(1,1)) 
# par(mfrow=c(3,1),mar=c(4.5,4,2,1),oma=c(4,1,4,1))
imagePlot(ratio_kriged_TA$x,
          ratio_kriged_TA$y,
          ratio_kriged_TA$z,
          # col=cols_fuco,
          # breaks=breaks_flb,
          asp=1,
          xlab='',ylab='',las=1,
          # xlim=xlims_flb,ylim=ylims_flb,
          # nlevel=length(cols_fuco),
          legend.width=1,legend.mar=3)
contour(ratio_kriged_TA$x,
        ratio_kriged_TA$y,
        ratio_kriged_TA$z,
        levels=breaks_TA,add=T)
image(ratio_SE_TA,add=T,breaks=quantile(ratio_SE_TA$z,c(1,1),na.rm=T),col='white')
# image(topo_lon,topo_lat,topo,breaks=c(-1,100),col='white',add=T)
# plot(FL,col='gray70',add=T)
# plot(world,col='gray70',add=T)
# geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+
# plot(FLKs1,color="darkgray",add=T,border="")+ #this only does the outline no fill
# plot(FLKs1,col="darkgray",add=T)+
  plot(FLKs1, col="darkgray", add=T, border=NA)

  # contour(topo_lon,topo_lat,topo,add=T,levels=c(-100,-50,-25,-10),col='gray40')
  points(FLBall$Longitude,FLBall$Latitude,pch=20,col='black')
  mtext(expression(paste('Longitude (',degree,'W)')),1,line=3,cex=1)
  mtext(expression(paste('Latitude (',degree,'N)')),2,line=3,cex=1)
mtext('Total Alkalinity (TA)',adj=1,cex=.75)


#how the hell did I save/export the plots???




# 
# #plot inset map
# INab=ggplot(data = FLKs1) +
#   # geom_sf(fill = "gray12", lwd = 0) +
#   # geom_sf(fill = "gray50", lwd = 0) +
#   geom_sf(fill = "darkgray", lwd = 0) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())+
#   theme( axis.text.x = element_blank(), 
#          axis.text.y = element_blank(), 
#          axis.ticks = element_blank())+
#   geom_rect(aes(xmin=-81.2,xmax=-80.5,ymin=24.95,ymax=25.30),
#             # geom_rect(aes(xmin=-82.1,xmax=-79.95,ymin=24.4,ymax= 26.2),
#             
#             color='darkred',lwd=.8,fill=NA) +
#   theme(panel.border = element_rect(colour = "black",fill=NA, linewidth=.9))+
#   # theme(panel.border = element_rect(colour = "black",fill=NA, size=.9))+
#   theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
# # geom_rect(aes(xmin=Xn,xmax=Xx,ymin=Yn,ymax=Yx),color='red',linetype='dashed',fill=NA)
# INab
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/insetFLB.png"),plot=INab,width = 8, height = 8, dpi = 300)

