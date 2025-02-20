## This script recreates Figure 1: Site map with log scale bathymetry
## Heidi K. Hirsh
## Last edit: Feb 19, 2025

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('ggmap','sf','ggplot2','ggspatial','ggnewscale','tidyterra','ggspatial','ggnewscale','tidyterra',"tidyverse")
lapply(packageload, library, character.only = TRUE)



# _________________________________
FLKs1=st_read('Flowshed_Modeling_InputData/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')

#high resolution SLIM bathymetry
bathy=st_read("Flowshed_Modeling_InputData/mesh_florida",layer="mesh_florida")
bathy$bathymetry.log = log(bathy$bathymetry) #log the depth so that the scale works later

#bring in the point data for later: 
CCf= read.csv('Flowshed_Modeling_InputData/CCfractions_fix.csv')
CCf$Lat=CCf$Latitude
CCf$Long=CCf$Longitude
CCf.sf = st_as_sf(CCf, coords = c("Long","Lat"),crs = st_crs(4326)) 
CCf.sf$Sub_region = factor(CCf.sf$Sub_region, levels=c("BB","UK","MK","LK"))
CCf.sf$Zone = factor(CCf.sf$Zone, levels=c("Inshore","Mid channel","Offshore","Oceanic"))

# #northern points: 
# PTS = read.csv('Flowshed_Modeling_InputData/PTS.csv')
# PTS$Lat=PTS$Latitude_Dec_Deg
# PTS$Long=PTS$Longitude_Dec_Deg
# PTS=st_as_sf(PTS, coords = c("Long","Lat"),crs = st_crs(4326)) 
# # mapview(PTS,zcol='Year_UTC')
# 
# #limit to shark river and middle cape
# iwant = c("SR","MC")
# PTSsub= subset(PTS,line_id %in% iwant)
# PTSsub= subset(PTSsub,Latitude_Dec_Deg>25) #remove 2 points in the keys
# 
# PTSsub5=subset(PTSsub,bathymetry>5) #only use points where depth is greater than 5m
# table(PTSsub5$Year_UTC,PTSsub5$line_id)


#redefine WFS points: 
PTS = read.csv('Flowshed_Modeling_InputData/PTS.csv')
PTS$Lat=PTS$Latitude_Dec_Deg
PTS$Long=PTS$Longitude_Dec_Deg
PTS=st_as_sf(PTS, coords = c("Long","Lat"),crs = st_crs(4326)) 

## limit to shark river and middle cape transects
iwant = c("SR","MC")
PTSsub= subset(PTS,line_id %in% iwant)
PTSsub= subset(PTSsub,Latitude_Dec_Deg>25) #remove 2 points in the keys
PTSsub5=subset(PTSsub,bathymetry>5) #only use points where depth is greater than 5m
plotShelfa = subset(PTSsub5,Year_UTC==2018)
unique(plotShelfa$Year_UTC)
plotShelf = subset(plotShelfa,Month_UTC==5)


#crop to bathymetry to desired size
xlims = c(-82.5,-79.8)
ylims = c(24.2,26)

box_coords <- tibble(x = xlims, y = ylims) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(st_crs(4326))
bounding_box <- st_bbox(box_coords) %>% st_as_sfc()

st_crs(bathy)==st_crs(bounding_box) #not the same
#change bathy crs
bathy.sf = st_transform(bathy, st_crs(4326))
st_crs(bathy.sf)==st_crs(bounding_box) #true!

bathy_subset <- st_intersection(bathy.sf, bounding_box)

####____ color, fill, shape settings

wf_fills    <-c('white','black')
wf_FILL_scale  <- scale_fill_manual(name = "line_name", values = wf_fills,
                                    breaks = c('Middle Cape','Shark River'),
                                    labels = c('Middle Cape','Shark River'))


bath.breaks=c(log(1000),log(200), log(50),log(10),log(0))
bath.labs=c('1000','200','50',"10",'0')


region_shapes1      <-c(22,24,23,25) #for distinguishing fill
region_SHAPE_scale1  <- scale_shape_manual(name = "subregion", values = region_shapes1,
                                           breaks = c('BB','UK','MK','LK'),
                                           labels = c('Biscayne Bay','Upper Keys','Middle Keys','Lower Keys'))


zone_fills=c("#4a3a3b", "#984136","#c26a7a","#ecc0a1" )
zone_FILL_scale  <- scale_fill_manual(name = "Zone", values = zone_fills,
                                      breaks = c('Inshore','Mid channel','Offshore','Oceanic'),
                                      labels = c('Inshore','Mid channel','Offshore','Oceanic'))

####____ make the map

slim = ggplot() +
  #bathymetry data:
  geom_sf(data=bathy_subset, aes(fill=bathymetry.log),lwd=0)+ 
  scale_fill_hypso_c(palette = "etopo1_bathy", 
                     direction = -1, 
                     breaks = bath.breaks, 
                     labels = bath.labs,
                     guide = guide_colorbar(title = 'Depth (m)', reverse = TRUE, draw.ulim = FALSE, draw.llim = FALSE)) +
  new_scale_fill()+
  
  #Florida shoreline/land: 
  geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+ 
  new_scale_fill()+
  
  #add scale and compass:
  annotation_scale(location = "tl", width_hint = 0.2,height=unit(.3,'cm'),text_cex=1.1, pad_x=unit(0.5, "cm"), pad_y=unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.2, "cm"),width = unit(1.0, "cm"),
                         pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"), style = north_arrow_orienteering) +

  #Florida Keys stations
  geom_point(data = CCf.sf, aes(x = dec.lon, y = dec.lat,fill=Zone,shape=Sub_region),size=3)+ #transect points
  zone_FILL_scale +
  # guides(fill=guide_legend(title="Zone"))+
  region_SHAPE_scale1+
  # guides(shape=guide_legend(title="Region"))+
  new_scale_fill()+
 
  #West Florida Shelf stations
  geom_star(data =  plotShelf, aes(x = Longitude_Dec_Deg, y = Latitude_Dec_Deg,fill=line_name), starshape=14,size =3, show.legend=FALSE) + #west florida shelf
  wf_FILL_scale+

  #combine guides
  guides(fill=guide_legend(title="Zone"), 
         shape=guide_legend(title="Region"))+
  
  ylab("Longitude")+
  xlab("Latitude")+
  coord_sf(xlim = c(-82.5, -79.8), ylim = c(24.2, 26), expand = FALSE)+
  theme_bw()


slim
ggsave(filename="Flowshed_Modeling_Figures/Figure1_i.png",plot=slim,width = 10, height = 8, dpi = 300)



#Region boxes and text labels were added outside of R.
