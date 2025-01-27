#site map with bathy using log scale

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('ggmap','sf','ggplot2','ggspatial','ggnewscale','tidyterra','ggspatial','ggnewscale','tidyterra')
# packageload <- c('stringr','basemaps','ggmap','tidyr','factoextra','sf','mapview',
#                  'geosphere','ggfortify','lwgeom','gridExtra','ggpubr','patchwork',
#                  'cowplot','ggplot2','rnaturalearth','rnaturalearthdata',
#                  'ggspatial','viridis','ggnewscale','ggpubr','ggOceanMaps','tidyterra',
#                  'cmocean','ggspatial','ggnewscale','tidyterra')
#PNWColor
lapply(packageload, library, character.only = TRUE)

# ----Check which packages we actually use----
# Find which packages do used functions belong to ----
used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/Flowshed_Modeling/Manuscript_Figures_2024/Figure1_BathySites.R", alphabetic = FALSE) |> print()
# Find which loaded packages are not used ----
used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
unused.packages <- packageload[!(packageload %in% used.packages)] |> print()
--------------------------------------------


register_google(key = "AIzaSyBSmQdRPOV8usMAOEkkoH6MISzFjTe5AoE") #new api Feb 2024

# _________________________________
FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')
# FLKs1=st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')
FLKs2=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_40%2C000_Scale)/Florida_Shoreline_(1_to_40%2C000_Scale).shp')
# FLKs2=st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Florida_Shoreline_(1_to_40%2C000_Scale)/Florida_Shoreline_(1_to_40%2C000_Scale).shp')

#high resolution SLIM bathymetry
bathy=st_read("/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/mesh_florida",layer="mesh_florida")
bathy$bathymetry.log = log(bathy$bathymetry) #log the depth so that the scale works later

#bring in the point data for later: 
CCf= read.csv('/Users/heidi.k.hirsh/Documents/GitHub/test-flk/0_InputData/CCfractions_fix.csv')
CCf$Lat=CCf$Latitude
CCf$Long=CCf$Longitude
CCf.sf = st_as_sf(CCf, coords = c("Long","Lat"),crs = st_crs(4326)) 
CCf.sf$Sub_region = factor(CCf.sf$Sub_region, levels=c("BB","UK","MK","LK"))
CCf.sf$Zone = factor(CCf.sf$Zone, levels=c("Inshore","Mid channel","Offshore","Oceanic"))

#northern points: 
PTS = read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/CC_dataframes/PTS.csv')
PTS$Lat=PTS$Latitude_Dec_Deg
PTS$Long=PTS$Longitude_Dec_Deg
PTS=st_as_sf(PTS, coords = c("Long","Lat"),crs = st_crs(4326)) 
# mapview(PTS,zcol='Year_UTC')

#limit to shark river and middle cape
iwant = c("SR","MC")
PTSsub= subset(PTS,line_id %in% iwant)
# dim(PTSsub) #115  49
# mapview(PTSsub,zcol='bathymetry')
PTSsub= subset(PTSsub,Latitude_Dec_Deg>25) #remove 2 points in the keys
# # dim(PTSsub) #113  49
# mapview(PTSsub,zcol='bathymetry')

PTSsub5=subset(PTSsub,bathymetry>5) #only use points where depth is greater than 5m
table(PTSsub5$Year_UTC,PTSsub5$line_id)

#crop to bathymetry to desired size
xlims = c(-82.5,-79.8)
ylims = c(24.2,26)

box_coords <- tibble(x = xlims, y = ylims) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(st_crs(4326))
# box_coords
bounding_box <- st_bbox(box_coords) %>% st_as_sfc()

st_crs(bathy)==st_crs(bounding_box) #not the same
#change bathy crs

bathy.sf = st_transform(bathy, st_crs(4326))
st_crs(bathy.sf)==st_crs(bounding_box) #true!

bathy_subset <- st_intersection(bathy.sf, bounding_box)
# mapview(bath_subset)
# plot(bathy_subset)


#wait to load the points yet. Just figure out coloring for bathymetry before making it more complicated
# basemap(limits = c(-84, -79.5, 24, 29), bathymetry = TRUE) #low res bathy example

#limit bathy first so that the map doesn't take FOREVER

# ggplot() + geom_sf(data=bathy_subset, aes(fill=bathymetry.log),lwd=0)
# ggplot() + geom_sf(data=bathy, aes(fill=bathymetry.log),lwd=0)

#define scale for point shapes and colors: 
#REGIONS (shape)
region_shapes1      <-c(22,24,23,25) #for distinguishing fill
region_shapes2     <-c(15,16,17,18) #solid
region_SHAPE_scale1  <- scale_shape_manual(name = "subregion", values = region_shapes2,
                                           breaks = c('BB','UK','MK','LK'),
                                           labels = c('Biscayne Bay','Upper Keys','Middle Keys','Lower Keys'))

#ZONES (color and fill)
star=pnw_palette(name="Moth",n=4,type="discrete")
zone_fills    <-rev(star)
zone_FILL_scale  <- scale_fill_manual(name = "Zone", values = zone_fills,
                                      breaks = c('Inshore','Mid channel','Offshore','Oceanic'),
                                      labels = c('Inshore','Mid channel','Offshore','Oceanic'))

# zone_colors    <-c('#fde725','#35b779','#31688e','#440154')
# zone_colors    <-c('#F2541B','#F2ab27','#63d8f2','#2467Bf')
zone_colors    <-c('#cc4c02','#fe9929','#fed98e','#ffffd4')
zone_colors    <-c('#02818a','#67a9cf','#bdc9e1','#f6eff7')
zone_colors    <-c('#ae017e','#f768a1','#fbb4b9','#feebe2')
# pnw=pnw_palette(name="Sailboat",n=4,type="discrete")
# pnw=pnw_palette(name="Bay",n=4,type="discrete")
# new_colors =rev(pnw)


zone_COLOR_scale  <- scale_color_manual(name = "Zone", values = zone_colors,
                                        breaks = c('Inshore','Mid channel','Offshore','Oceanic'),
                                        labels = c('Inshore','Mid channel','Offshore','Oceanic'))

zone_shapes    <-c(1,2,5,0)
zone_SHAPE_scale  <- scale_color_manual(name = "Zone", values = zone_shapes,
                                        breaks = c('Inshore','Mid channel','Offshore','Oceanic'),
                                        labels = c('Inshore','Mid channel','Offshore','Oceanic'))

wf_fills    <-c('white','black')
wf_FILL_scale  <- scale_fill_manual(name = "line_name", values = wf_fills,
                                    breaks = c('Middle Cape','Shark River'),
                                    labels = c('Middle Cape','Shark River'))


# bath.breaks=c(log(1000),log(500),log(100), log(50),log(10),log(5))
# bath.labs=c('1000','500','100','50',"10",'5')

bath.breaks=c(log(1000),log(200), log(50),log(10),log(0))
bath.labs=c('1000','200','50',"10",'0')


#define region shapes
region_shapes2     <-c(15,15,17,18)
# region_shapes2     <-c(16,15,17,18)
# region_shapes      <-c(21,22,24,23) #for distinguishing fill
region_shapes1      <-c(22,24,23,25) #for distinguishing fill
region_SHAPE_scale1  <- scale_shape_manual(name = "subregion", values = region_shapes1,
                                           breaks = c('BB','UK','MK','LK'),
                                           labels = c('Biscayne Bay','Upper Keys','Middle Keys','Lower Keys'))

region_SHAPE_scale2  <- scale_shape_manual(name = "subregion", values = region_shapes2,
                                           breaks = c('BB','UK','MK','LK'),
                                           labels = c('Biscayne Bay','Upper Keys','Middle Keys','Lower Keys'))

star=pnw_palette(name="Moth",n=4,type="discrete")
zone_fills    <-rev(star)
# "#ecc0a1" "#c26a7a" "#984136" "#4a3a3b"
zone_FILL_scale  <- scale_fill_manual(name = "Zone", values = zone_fills,
                                      breaks = c('Inshore','Mid channel','Offshore','Oceanic'),
                                      labels = c('Inshore','Mid channel','Offshore','Oceanic'))




# breaks=log(pretty(exp(bathy_subset$bathymetry.log))),
# labels=pretty(exp(bathy_subset$bathymetry.log)))+


slim = ggplot() +
  geom_sf(data=bathy_subset, aes(fill=bathymetry.log),lwd=0)+#color=NA)+ 
  # scale_fill_cmocean(name="deep",breaks=bath.breaks, labels=bath.labs)+
  # scale_fill_cmocean(name="haline",direction=-1,breaks=bath.breaks, labels=bath.labs)+
  scale_fill_hypso_c(palette="etopo1_bathy",direction=-1,breaks=bath.breaks, labels=bath.labs)+
  guides(fill=guide_colourbar(title='Depth (m)',reverse=T,draw.ulim=FALSE, draw.llim = FALSE))+
  annotation_scale(location = "tl", width_hint = 0.2,height=unit(.3,'cm'),text_cex=1.1, pad_x=unit(0.5, "cm"), pad_y=unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.2, "cm"),width = unit(1.0, "cm"),
                         pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"), style = north_arrow_orienteering) +
  new_scale_fill()+
  # new_scale_color()+
  geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+
  geom_point(data = CCf.sf, aes(x = dec.lon, y = dec.lat,color=Zone),size=3)+
  zone_COLOR_scale+
  # geom_point(data =  PTSsub5, aes(x = Longitude_Dec_Deg, y = Latitude_Dec_Deg), pch=21,size = 4) +
  ylab("Longitude")+
  xlab("Latitude")+
  coord_sf(xlim = c(-82.5, -79.8), ylim = c(24.2, 26), expand = FALSE)+
  theme_bw()
slim

TS=Sys.time()
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Fig1.logBathy_",TS,".png"),plot=slim,width = 10, height = 8, dpi = 300)

# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Figure1/Fig1.logBathy_",TS,".png"),plot=slim,width = 10, height = 8, dpi = 300)
