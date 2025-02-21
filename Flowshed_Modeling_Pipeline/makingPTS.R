## Determine endmember chemistry north of the Florida Keys - in this version remove the sub region boundaries (stick to outermost bounds)
## Using data from AOML short cruises: https://www.aoml.noaa.gov/ocd/ocdweb/shortcruises/shortcruises_main.html

#review 12/13/24 - looking for all relevant inputs for code repo
#Is this where I saved PTS.csv? YES.



## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
# packageload <- c('stringr','lwgeom','dplyr','ggmap','tidyr','factoextra','sf','mapview','geosphere','ggfortify','lwgeom')
packageload <- c('lwgeom','dplyr','ggmap','sf','mapview')
lapply(packageload, library, character.only = TRUE)


##_______Read in original CC data for the Keys 
CC = read.csv('Flowshed_Modeling_InputData/CCnutsparbath_9feb2024.csv') 
CC = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')) #limit to years of interest

##_______Read in short cruise data (two files)
#"north carbonate chemistry" part 1 = Short cruise discrete data 2003-2016
NCC1=read.csv('Flowshed_Modeling_InputData/AOML-ShortCruises_DiscreteData_2003-2016.csv')
#"north carbonate chemistry" part 2 = Short cruise discrete data 2017-2020
NCC2=read.csv('Flowshed_Modeling_InputData/AOML-ShortCruises_DiscreteData_2017-2020.csv')
#Merge the two datasets
NCC = rbind(NCC1, NCC2)

NCC$Lat=NCC$Latitude_Dec_Deg
NCC$Long=NCC$Longitude_Dec_Deg

##_______Limit data to the appropriate spatial area (should I also limit by depth? avoid metabolically influenced shallow water)
#pair with bathymetry point depth from mesh grid
bathy_sf <- st_read(dsn = "Flowshed_Modeling_InputData/mesh_florida",layer="mesh_florida")


# Convert to spatial data 
NCC_sf = st_as_sf(NCC, coords = c("Long","Lat")) %>% st_set_crs(st_crs(bathy_sf))
mapview(NCC_sf,zcol='Year_UTC')

NCC_bathy= st_join(NCC_sf,bathy_sf)
mapview(NCC_bathy,zcol="bathymetry")

NCC %>% filter(DIC_umol.kg>1200,Latitude_Dec_Deg>24.6,Latitude_Dec_Deg<27,Longitude_Dec_Deg>(-84),Longitude_Dec_Deg<(-80)) %>% 
ggplot()+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=Latitude_Dec_Deg))+
  scale_color_viridis_c()+
  theme_bw()


## Which stations from the West Florida Shelf have good seasonal data?

subNCC=NCC %>% filter(DIC_umol.kg>1200,Latitude_Dec_Deg>24.6,Latitude_Dec_Deg<27,Longitude_Dec_Deg>(-84),Longitude_Dec_Deg<(-80))
table(subNCC$Month_UTC)
# 1  3  5  7  8  9 10 11 12 
# 13 16 22  1 14 22 38 49 26 

sfy=NCC %>% filter(DIC_umol.kg>1200,Latitude_Dec_Deg>24.6,Latitude_Dec_Deg<27,Longitude_Dec_Deg>(-84),Longitude_Dec_Deg<(-80)) %>% 
ggplot()+
  geom_point(aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg,color=DIC_umol.kg,size=DIC_umol.kg,shape=factor(ceiling(Month_UTC/3))))+
  scale_color_viridis_c()+
  theme_bw()
sfy


# Use the 14-day backward polygons as the maximum area of interest: 
# a) bounding box of those polygons
# b) large boundary including all polygons


## read in 14-day polygons (this version is already buffered)
shp.list = list.files(path = paste0("Flowshed_Modeling_InputData/yearBows_22april2024"), pattern="*shp", full.names = T) 
shapefile_list = lapply(shp.list,read_sf)
all.yearBows = rbind(shapefile_list[[1]],shapefile_list[[2]],shapefile_list[[3]],shapefile_list[[4]])
yearBows=all.yearBows
# dim(yearBows)


# Add other CC (Keys) data to yearBows geometries
CCbows = left_join(yearBows, CC, by="visitID")

#subset yearBows to only 14 days and only backward  (largest extents for determining relevant boundary)
YB = subset(CCbows, n_days==14 & simu=="backward")

# Define bounding box around all 14day polygons
Box = st_bbox(YB) %>% st_as_sfc(st_crs=4326) %>% st_sf %>% st_cast
st_crs(NCC_sf) == st_crs(Box) #false
Box = st_transform(Box, st_crs(NCC_sf))
st_crs(NCC_sf) == st_crs(Box) #true

NCC_sf_box= NCC_sf[Box,]
mapview(NCC_sf_box) #only the cruise stations inside that bounding box

#highlight subset of points inside bounding box
mapview(NCC_sf)+
  mapview(Box,col.regions='pink')+
  mapview(NCC_sf_box,col.regions='red') 


## Load line dividing area above the Keys (north) and below the Keys (south)
FLKline = st_read('Flowshed_Modeling_InputData/LineV3.kml')
FLKline = st_cast(FLKline,to = 'MULTILINESTRING')
FLKline = st_make_valid(FLKline)
FLKline = st_transform(FLKline, crs=4326)
Box = st_transform(Box,crs=4326)
mapview(Box)+ mapview(FLKline)
st_crs(Box) == st_crs(FLKline)

## Which stations are in the upper category (above the Keys)
BoxNS = lwgeom::st_split(x=Box,y=FLKline)
class(BoxNS) #"sf"         "data.frame"
mapview(BoxNS) +  mapview(NCC_sf_box)

result = st_collection_extract(BoxNS,"POLYGON")
mapview(result[2,],col.regions='green')+mapview(result[1,],col.regions='cyan')

topBox = result[2,]
NCC_sf_box = st_transform(NCC_sf_box,crs=4326)
topBox = st_transform(topBox,crs=4326)
nBoxpts = NCC_sf_box[topBox,] #points inside bounding box AND above the keys line 
mapview(nBoxpts)+mapview(BoxNS,alpha.regions=0)+mapview(topBox,col.regions='green')

## The points we care about are inside the largest bounding boxy so save that subset...

PTS=NCC_sf_box
mapview(PTS)
PTS=cbind(PTS,st_coordinates(PTS))
st_crs(PTS) == st_crs(bathy_sf)
bathy_sf = st_transform(bathy_sf,st_crs(PTS))
st_crs(PTS) == st_crs(bathy_sf)

PTS = st_join(PTS,bathy_sf)
mapview(PTS,zcol="bathymetry")


#look at shark river (SR) and middle cape (MC) transects - seasonality
## first, need to pair station names with Walton Smith station data
eID = read.csv('Flowshed_Modeling_InputData/stations_list.csv')
eID$Station_ID = eID$station_id
transectID= subset(eID, select=c(Station_ID,line_id,line_name,alt_station_id))

PTS=left_join(PTS, transectID, by="Station_ID")
table(PTS$line_id)
## We are interestedt in SR (103 sampels) and MC (12 samples)


# write PTS for use in other scripts
PTS.dg=st_drop_geometry(PTS)
class(PTS.dg)
# write.csv(PTS.dg,file='Flowshed_Modeling_InputData/PTS.csv',row.names=F)

iwant = c("SR","MC")
PTSsub= subset(PTS,line_id %in% iwant)
mapview(PTSsub,zcol='bathymetry')
PTSsub= PTSsub %>% filter(Latitude_Dec_Deg>25) #remove 2 points in the keys
mapview(PTSsub,zcol='bathymetry')
PTSsub.dg=st_drop_geometry(PTSsub)
# write.csv(PTSsub.dg,file='Flowshed_Modeling_InputData/PTSsub.csv',row.names=F)

## Examine seasonal DIC and TA variability (and data coverage)
ggplot(PTSsub)+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=Year_UTC))+
  scale_color_viridis_c()+
  facet_grid('line_id')+
  theme_bw()

ggplot(PTSsub)+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=bathymetry))+
  scale_color_viridis_c()+
  facet_grid('line_id')+
  theme_bw()

ggplot(PTSsub)+
  geom_point(aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg,color=DIC_umol.kg,size=DIC_umol.kg,shape=factor(ceiling(Month_UTC/3))))+
  scale_color_viridis_c()+
  theme_bw()

#where to cut off DIC 
ggplot(PTSsub)+
  geom_point(aes(x=bathymetry,y=DIC_umol.kg,color=Month_UTC))+
  scale_color_viridis_c()+
  facet_grid('line_id')+
  geom_vline(xintercept=3,linetype='dashed')+
  geom_vline(xintercept=4,linetype='dashed')+
  geom_vline(xintercept=5,linetype='dashed')+
  geom_vline(xintercept=6,linetype='dashed')+
  theme_bw()


#seasonal oceanic data in the keys
ggplot(subset(CC,Zone=='Oceanic'))+
  # geom_point(aes(x=jday.utc,y=DIC_umol_kg,color=Sub_region,shape=factor(Year)),size=2)+
  geom_point(aes(x=jday.utc,y=DIC_umol_kg,color=Year,shape=Sub_region),size=2)+
  scale_color_viridis_c()+
  theme_bw()


ggplot(subset(CC,Zone=='Oceanic'))+
  geom_point(aes(x=jday.utc,y=DIC_umol_kg,color=Year,shape=Sub_region),size=2)+
  facet_wrap('Sub_region',ncol=1)+
  scale_color_viridis_c()+
  theme_bw()


