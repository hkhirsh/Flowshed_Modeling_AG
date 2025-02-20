## Clean up this script to plot flowshed for manuscript summary figure
#Edited script to do a habitat comparison over multiple ndays

# rm(list=ls())
library(stringr)
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
# register_google(key = "AIzaSyBSmQdRPOV8usMAOEkkoH6MISzFjTe5AoE") #new api Feb 2024

#read halo waypoints
halo = read.csv('/Users/heidi.k.hirsh/Desktop/halo_points/EPA_Halozone_Sample_Points.csv')
head(halo)
halo$latitude=halo$Latitude
halo$longitude=halo$Longitude
halo.sf = st_as_sf(halo, coords= c("longitude","latitude"),crs = st_crs(4326)) 
#read WS points
ws = read.csv('/Users/heidi.k.hirsh/Desktop/halo_points/WS_2023_coords.csv')
head(ws)
ws$Latitude=ws$dec.lat
ws$Longitude=ws$dec.lon
ws.sf = st_as_sf(ws, coords= c("dec.lon","dec.lat"),crs = st_crs(4326)) 


#Read in carbonate chemistry station data
# CC = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/CCflk_plusBathy.csv',na.strings=c(""," "))
flb9 = read.csv('/Users/heidi.k.hirsh/Desktop/FLBcruises/FLBsept2024.csv')
flb9$Latitude=flb9$Lat
flb9$Longitude=flb9$Lon
FLB9 = st_as_sf(flb9, coords= c("Lon","Lat"),crs = st_crs(4326)) 


bathy=st_read("/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/mesh_florida",layer="mesh_florida")
bathy$bathymetry.log = log(bathy$bathymetry) #log the depth so that the scale works later

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

FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')

bayBox = st_bbox(FLB9) %>% st_as_sfc(st_crs=4326) %>% st_sf %>% st_cast
plot(bayBox)
class(bayBox)

## include shelf points
PTS = read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/CC_dataframes/PTS.csv')
PTS$Lat=PTS$Latitude_Dec_Deg
PTS$Long=PTS$Longitude_Dec_Deg
PTS=st_as_sf(PTS, coords = c("Long","Lat"),crs = st_crs(4326))

#limit to shark river and middle cape
iwant = c("SR","MC")
PTSsub= subset(PTS,line_id %in% iwant)
PTSsub= subset(PTSsub,Latitude_Dec_Deg>25) #remove 2 points in the keys
PTSsub5=subset(PTSsub,bathymetry>5) #only use points where depth is greater than 5m
plotShelfa = subset(PTSsub5,Year_UTC==2018)
plotShelf = subset(plotShelfa,Month_UTC==5)
# mapview(plotShelf)
head(plotShelf)

bayPoints=ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  geom_point(data=FLB9,aes(x=Longitude,y=Latitude,color=ODO.mgL),pch=18,size=3)+
  scale_colour_gradientn(colors = rainbow(8)) +
  # scale_color_brewer(palette = "OrRd")+
  geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color='cyan',size=3,stroke=1)+
  geom_point(data=ws.sf,aes(x=Longitude,y=Latitude),color='darkslategray',size=.7,stroke=1)+ 
  geom_point(data=halo.sf,aes(x=Longitude,y=Latitude),color='deeppink',size=1,stroke=1)+
  ylab('Latitude')+
  xlab('Longitude')+
  coord_sf(xlim = c(-82.1 , -80), ylim = c(24.4, 26), expand = FALSE) + #for backward halos
  theme_bw()
bayPoints
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FLBmaps/FLBpoints2.png"),plot=bayPoints,width = 10, height = 8, dpi = 300)
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/FLBpoints2.png"),plot=bayPoints,width = 10, height = 8, dpi = 300)





#add bathy
bayBath = ggplot() +
  geom_sf(data=bathy_subset, aes(fill=bathymetry.log),lwd=0)+#color=NA)+ 
  scale_fill_cmocean(name="deep",breaks=bath.breaks, labels=bath.labs)+
  geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+
  geom_point(data = FLB9, aes(x = Longitude, y = Latitude, color=ODO.mgL),size=2)+ 
  # scale_color_viridis_c()+
  scale_colour_gradientn(colors = rainbow(8)) +
  geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color='cyan',size=3,stroke=1)+
  geom_point(data=ws.sf,aes(x=Longitude,y=Latitude),color='darkslategray',size=.7,stroke=1)+ 
  geom_point(data=halo.sf,aes(x=Longitude,y=Latitude),color='deeppink',size=1,stroke=1)+
  ylab("Longitude")+
  xlab("Latitude")+
  coord_sf(xlim = c(-82.1 , -80), ylim = c(24.4, 26), expand = FALSE) + #for backward halos
  theme_bw()
bayBath
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FLBmaps/bayBathy2.png"),plot=bayBath,width = 10, height = 8, dpi = 300)

bayBathZ = ggplot() +
  geom_sf(data=bathy_subset, aes(fill=bathymetry.log),lwd=0)+#color=NA)+ 
  # scale_fill_cmocean(name="deep",breaks=bath.breaks, labels=bath.labs)+
  scale_fill_cmocean(name="deep")+
  geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+
  geom_point(data = FLB9, aes(x = Longitude, y = Latitude, color=ODO.mgL),size=2)+ 
  # scale_color_viridis_c()+
  scale_colour_gradientn(colors = rainbow(8)) +
  geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color='cyan',size=3,stroke=1)+
  geom_point(data=ws.sf,aes(x=Longitude,y=Latitude),color='darkslategray',size=.7,stroke=1)+ 
  geom_point(data=halo.sf,aes(x=Longitude,y=Latitude),color='deeppink',size=1,stroke=1)+
  ylab("Longitude")+
  xlab("Latitude")+
  coord_sf(xlim = c(-81.3 , -80.3), ylim = c(24.6, 25.3), expand = FALSE) + #for backward halos
  theme_bw()
bayBathZ
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FLBmaps/bayBathy3zoom.png"),plot=bayBathZ,width = 10, height = 8, dpi = 300)
