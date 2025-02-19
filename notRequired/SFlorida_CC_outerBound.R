## Determine endmember chemistry north of the Florida Keys - in this version remove the sub region boundaries (stick to outermost bounds)
## Using data from AOML short cruises: https://www.aoml.noaa.gov/ocd/ocdweb/shortcruises/shortcruises_main.html

#review 12/13/24 - looking for all relevant inputs for code repo
#Is this where I saved PTS.csv? 


#Can't save shp as I go along because the column names are too long (over 10 characters???)


# rm(list=ls())
# Load Libraries ----------------------------------------------------------
library(stringr)
library(lwgeom)
library(dplyr)
library(ggmap)
library(tidyr)
library(factoextra)
library(sf)
library(mapview)
library(geosphere) #distance between points
library(vegan)
library(ggfortify)
library(lwgeom)
register_google(key = "AIzaSyB3mq0nJtcNWvQUNuqH_t-MLxEUmwDDmGk") #api updated OCT23

##_______Read in original CC data for the Keys (need metadata for regions)
#**I should be reading in an updated version that ALWAYS has the visitID for matching to other data
CC = read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/CCflk_plusBathy.csv') 
CC = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')) #limit to years of interest
#add visitID so this data can be paired with yearBows
visitID_ch1 =  paste(CC$SiteID,CC$UTCDate_Time)
visitID_ch2 = gsub(" ", "_" , visitID_ch1, perl=TRUE)
CC$visitID =  gsub("[: -]", "" , visitID_ch2, perl=TRUE)


##_______Read in short cruise data (two files)
#"north carbonate chemistry" part 1 = Short cruise discrete data 2003-2016
NCC1=read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/AOML-ShortCruises_DiscreteData_2003-2016/AOML-ShortCruises_DiscreteData_2003-2016.csv')
#"north carbonate chemistry" part 2 = Short cruise discrete data 2017-2020
NCC2=read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/AOML-ShortCruises_DiscreteData_2017-2020/AOML-ShortCruises_DiscreteData_2017-2020.csv')
#Merge the two datasets
NCC = rbind(NCC1, NCC2)

NCC$Lat=NCC$Latitude_Dec_Deg
NCC$Long=NCC$Longitude_Dec_Deg


##_______Limit data to the appropriate spatial area (should I also limit by depth? avoid metabolically influenced shallow water)
#pair with bathymetry point depth from mesh grid
bathy_sf=st_read(dsn = "/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/mesh_florida",layer="mesh_florida")

# Convert to spatial data 
NCC_sf = st_as_sf(NCC, coords = c("Long","Lat")) %>% st_set_crs(st_crs(bathy_sf))
# NCC_sf = st_as_sf(NCC, coords = c("Longitude_Dec_Deg","Latitude_Dec_Deg")) %>% st_set_crs(st_crs(bathy_sf))
# NCC_sf = st_as_sf(NCC, coords = c("Longitude_Dec_Deg","Latitude_Dec_Deg"), crs = st_crs(4326))
mapview(NCC_sf,zcol='Year_UTC')

NCC_bathy= st_join(NCC_sf,bathy_sf)
mapview(NCC_bathy,zcol="bathymetry")
#*move forward with NCC_bathy


NCC %>% filter(DIC_umol.kg>1200,Latitude_Dec_Deg>24.6,Latitude_Dec_Deg<27,Longitude_Dec_Deg>(-84),Longitude_Dec_Deg<(-80)) %>% 
ggplot()+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=Latitude_Dec_Deg))+
  scale_color_viridis_c()+
  theme_bw()


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
# ggsave(sfy,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/sf_samples.png")


# Use the 14-day backward polygons as the maximum area of interest: 
# a) bounding box of those polygons
# b) large boundary including all polygons

#read in 14day polygons
yearBows = st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Bows_forPlotting_14days_tri.shp')
# CCbows = left_join(yearBows, CC, by="visitID")
# View(CCbows)

# Add other CC (Keys) data to YB geometries
CCbows = left_join(yearBows, CC, by="visitID")
names(CCbows)
#save CCbows
# st_write(CCbows,'/Users/heidi.k.hirsh/Desktop/CCbows_5Feb.shp')
# CCbowsR = st_read('/Users/heidi.k.hirsh/Desktop/CCbows_5Feb.shp')


#subset yearBows to only 14 days and only backward 
YB = subset(CCbows, n_days==14 & simu=="backward")
dim(YB)
#save YB
class(YB)
# st_write(YB,'/Users/heidi.k.hirsh/Desktop/YB14b.shp')
# YB2=st_read('/Users/heidi.k.hirsh/Desktop/YB14b.shp')


# Define bounding box around all 14day polygons
Box = st_bbox(YB) %>% st_as_sfc(st_crs=4326) %>% st_sf %>% st_cast
st_crs(NCC_sf) == st_crs(Box)
Box = st_transform(Box, st_crs(NCC_sf)) #I'm not sure why I have to transform again
st_crs(NCC_sf) == st_crs(Box)

NCC_sf_box= NCC_sf[Box,]
mapview(NCC_sf_box)
#highlight subset of points inside bounding box
mapview(NCC_sf)+
  mapview(Box,col.regions='pink')+
  mapview(NCC_sf_box,col.regions='red') 


FLKline = st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/LineV3.kml')
FLKline = st_cast(FLKline,to = 'MULTILINESTRING')
FLKline = st_make_valid(FLKline)
FLKline = st_transform(FLKline, crs=4326)
mapview(Box)+ mapview(FLKline)
st_crs(Box) == st_crs(FLKline)

# BoxNS. = st_intersection(Box,FLKline)
BoxNS = lwgeom::st_split(x=Box,y=FLKline)
mapview(BoxNS) +  mapview(NCC_sf_box)
# mapview(BoxNS) + mapview(FLKline) + mapview(NCC_sf_box)

result = st_collection_extract(BoxNS,"POLYGON")
result
class(result)
mapview(result[2,],col.regions='green')+mapview(result[1,],col.regions='cyan')

topBox = result[2,]
nBoxpts = NCC_sf_box[topBox,] #points inside bounding box AND above the keys line 
mapview(nBoxpts)+mapview(BoxNS,alpha.regions=0)+mapview(topBox,col.regions='green')
#do the same for the tightest polygon bounds

class(BoxNS)

BoxNS.p = st_collection_extract(BoxNS,"POLYGON")
class(BoxNS.p)
mapview(BoxNS.p)

# https://stackoverflow.com/questions/60454067/cpl-write-ogr-error-when-writing-out-shape-file-in-r
# st_write(BoxNS.p,'/Users/heidi.k.hirsh/Desktop/BoxNS.shp')
# test=st_read('/Users/heidi.k.hirsh/Desktop/BoxNS.shp')
# mapview(test)

#12/13 for some reason tight is ending up empty. YB is fine so it must be an issue with the union

# Determine outermost bounds of all 14day (backward) polygons - all years
Tight = st_union(YB,st_crs=4326) %>% st_sf %>% st_cast     #this takes a while
mapview(Tight)
st_crs(NCC_sf) == st_crs(Tight)

Tight = st_transform(Tight, st_crs(NCC_sf))
st_crs(NCC_sf) == st_crs(Tight)
Tight.v <- st_make_valid(Tight) #nothing here? 12/13
mapview(Tight.v)
NCC_sf_tight= NCC_sf[Tight.v,]

# mapview(NCC_sf)+
#   mapview(Tight,col.regions='pink')+
#   mapview(NCC_sf_tight,col.regions='red') 


mapview(Tight.v) + mapview(FLKline)

#isolate top of Tight.v boundary
TightNS = lwgeom::st_split(x=Tight.v,y=FLKline)
TightNS = st_split(Tight.v,FLKline)
class(TightNS)
class(FLKline)


mapview(TightNS) +mapview(NCC_sf_tight)
mapview(TightNS)
mapview(NCC_sf_tight)

spliTight= st_collection_extract(TightNS,"POLYGON")
spliTight= st_collection_extract(TightNS)
mapview(TightNS) + mapview(spliTight)

mapview(spliTight)
dim(spliTight) #this is 628, 1 and I think it should be 2,1
mapview(Tight.v)+mapview(spliTight[2,],col.regions='green')+mapview(spliTight[1,],col.regions='cyan') 

mapview(spliTight[3,])#somehow it is split into way too many polygons. Should just be two sides on either side of line. 
  
  
  
topTight = spliTight[2,]
mapview(topTight)
nTightpts = NCC_sf_tight[topTight,] #points inside bounding box AND above the keys line   #no points? (12/13)
mapview(nTightpts)+mapview(TightNS,alpha.regions=0)+mapview(topTight,col.regions='green')

#look at depth of points
mapview(nTightpts,zcol='Depth_Bottom_meters')
# names(nTightpts)
#mostly -999 
#use mesh grid to pull bathy at each point again? 


#we don't want that point just below the keys but technically above the keys line :/
#start with point inside largest bounding box (Box)
PTS=NCC_sf_box
mapview(PTS)
PTS=cbind(PTS,st_coordinates(PTS))
# Pair with bathy data again here (just to avoid rerunning for now - comment out later!)
# PTS = st_as_sf(NCC, coords = c("Longitude_Dec_Deg","Latitude_Dec_Deg")) %>% st_set_crs(st_crs(bathy_sf))
st_crs(PTS) == st_crs(bathy_sf)
# PTS = st_transform(PTS, crs=st_crs(bathy_sf))
PTS = st_join(PTS,bathy_sf) #also takes a bit of time
mapview(PTS,zcol="bathymetry")
#*move forward with NCC_bathy

#save point IDs based on spatial location
PTS[,'tight'] = NA #inside tight polygon bounds
PTS[,'nBox'] = NA #inside north side of bounding box
PTS[,'nTight'] = NA #inside norht side of tight polygon

# PTS = st_transform(PTS,crs(Tight.v))

for(i in 1:nrow(PTS)) {
  #is the point inside the tight 14d polygon bounds
  in.tight = sf::st_intersects(PTS[i, ], Tight.v)
  in.tightM = as.matrix(in.tight)
  if (in.tightM == "TRUE") {
    PTS$tight[i] = 'yes'
  }
  else {PTS$tight[i] = 'no'}
  
  #is the point inside the upper (northern) part of the bounding box
  in.nbox = sf::st_intersects(PTS[i, ], topBox)
  in.nboxM = as.matrix(in.nbox)
  if (in.nboxM == "TRUE") {
    PTS$nBox[i] = 'yes'
  }
  else {PTS$nBox[i] = 'no'}
  
  #is the point inside the upper (northern) part of the tight 14d poly boundary
  in.nTight = sf::st_intersects(PTS[i, ], topTight)
  in.nTightM = as.matrix(in.nTight)
  if (in.nTightM == "TRUE") {
    PTS$nTight[i] = 'yes'
  }
  else {PTS$nTight[i] = 'no'}
  
} #end spatial identity loop

# mapview(PTS,zcol='tight')+mapview(Tight.v)
# mapview(PTS,zcol='nTight')+mapview(topTight)
# mapview(PTS,zcol='nBox')+mapview(topBox)

mapview(subset(PTS,tight=='yes'),zcol='bathymetry')+mapview(Tight.v,alpha.regions=0)
mapview(subset(PTS,nTight=='yes'),zcol='bathymetry')+mapview(topTight,alpha.regions=0)
mapview(subset(PTS,nBox=='yes'),zcol='bathymetry')+mapview(topBox,alpha.regions=0)


#look at shark river (SR) and middle cape (MC) transects - seasonality
eID = read.csv('/Users/heidi.k.hirsh/Documents/Discrete_Samples_AOML-ShortCruises/stations_list.csv')
# dim(eID)
eID$Station_ID = eID$station_id
transectID= subset(eID, select=c(Station_ID,line_id,line_name,alt_station_id))
# head(transectID)
# dim(transectID)
# dim(NCC)

#Need to join AFTER cropping data to just the S Florida area
PTS=left_join(PTS, transectID, by="Station_ID")
unique(PTS$line_id)


# write PTS for modeling
#drop geo to write? - need to drop geometry to save it as a csv
PTS.dg=st_drop_geometry(PTS)
class(PTS.dg)
# write.csv(PTS.dg,'/Users/heidi.k.hirsh/Desktop/',row.names=F)
# PTStest = read.csv('/Users/heidi.k.hirsh/Desktop/')

iwant = c("SR","MC")
PTSsub= subset(PTS,line_id %in% iwant)
mapview(PTSsub,zcol='bathymetry')
PTSsub= PTSsub %>% filter(Latitude_Dec_Deg>25) #remove 2 points in the keys
dim(PTSsub)
mapview(PTSsub,zcol='bathymetry')

PTSsub.dg=st_drop_geometry(PTSsub)
class(PTSsub.dg)
# write.csv(PTSsub.dg,'/Users/heidi.k.hirsh/Desktop/PTSsub.csv',row.names=F)
# testPTSsub=read.csv('/Users/heidi.k.hirsh/Desktop/PTSsub.csv')

#filter by depth
PTSsub3=PTSsub %>% filter(bathymetry>3) 
PTSsub4=PTSsub %>% filter(bathymetry>4) 


yDIC=ggplot(PTSsub)+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=Year_UTC))+
  scale_color_viridis_c()+
  facet_grid('line_id')+
  theme_bw()
yDIC
# ggsave(yDIC,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/yDIC_MCSRtrans.png")

yDICb=ggplot(PTSsub)+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=bathymetry))+
  scale_color_viridis_c()+
  facet_grid('line_id')+
  theme_bw()
yDICb
# ggsave(yDICb,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/yDIC_MCSRtrans_bathy.png")

# ggplot(PTSsub)+
#   geom_point(aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg,color=DIC_umol.kg,size=DIC_umol.kg,shape=factor(ceiling(Month_UTC/3))))+
#   scale_color_viridis_c()+
#   theme_bw()

#where to cut off DIC #not working 12/13
DICcut=ggplot(PTSsub)+
  geom_point(aes(x=bathymetry,y=DIC_umol.kg,color=Month_UTC))+
  scale_color_viridis_c()+
  facet_grid('line_id')+
  geom_vline(xintercept=3,linetype='dashed')+
  geom_vline(xintercept=4,linetype='dashed')+
  geom_vline(xintercept=5,linetype='dashed')+
  geom_vline(xintercept=6,linetype='dashed')+
  theme_bw()
DICcut
# ggsave(DICcut,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/where cut dic.png")


yDIC2=ggplot(PTSsub3)+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=Year_UTC,shape=line_id),size=2)+
  scale_color_viridis_c()+
  # facet_grid('line_id')+
  theme_bw()
yDIC2

# ggsave(yDIC2,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/yDIC_trans_year_lineid.png")

#seasonal oceanic data in the keys
reg=ggplot(subset(CC,Zone=='Oceanic'))+
  # geom_point(aes(x=jday.utc,y=DIC_umol_kg,color=Sub_region,shape=factor(Year)),size=2)+
  geom_point(aes(x=jday.utc,y=DIC_umol_kg,color=Year,shape=Sub_region),size=2)+
  scale_color_viridis_c()+
  theme_bw()
reg
# ggsave(reg,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/jdayDICreg.png")


# ggplot(subset(CC,Zone=='Oceanic'))+
#   geom_point(aes(x=jday.utc,y=DIC_umol_kg,color=Year,shape=Sub_region),size=2)+
#   facet_wrap('Sub_region')+
#   scale_color_viridis_c()+
#   theme_bw()

#can I plot seasonal data together
class(CC$jday.utc)
class(PTSsub3$YearDay_UTC)


siteyrdic=ggplot()+
  geom_point(data=PTSsub3, aes(x=YearDay_UTC,y=DIC_umol.kg,shape=line_id),color='red',size=2)+
  geom_point(data=subset(CC,Zone=='Oceanic'), aes(x=jday.utc,y=DIC_umol_kg,shape=Sub_region),color='blue',size=2)+
  theme_bw()
siteyrdic
# ggsave(siteyrdic,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/siteyrdic.png")


NScomp=ggplot()+
  geom_point(data=PTSsub3, aes(x=YearDay_UTC,y=DIC_umol.kg),color='orange',size=2)+
  geom_point(data=subset(CC,Zone=='Oceanic'), aes(x=jday.utc,y=DIC_umol_kg),color='blue',size=2)+
  theme_bw()
NScomp
# ggsave(NScomp,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/NScomp.png")


ggplot()+
  geom_point(data=PTSsub3, aes(x=YearDay_UTC,y=DIC_umol.kg,color=bathymetry),shape=16,size=4)+
  geom_point(data=subset(CC,Zone=='Oceanic'), aes(x=jday.utc,y=DIC_umol_kg),color='blue',shape=1,size=4)+
  scale_color_viridis_c()+
  theme_bw()
# ggsave(dicb,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/dicb.png")


#reverse color scale
dicbrev=ggplot()+
  geom_point(data=PTSsub3, aes(x=YearDay_UTC,y=DIC_umol.kg,color=bathymetry),shape=16,size=4)+
  geom_point(data=subset(CC,Zone=='Oceanic'), aes(x=jday.utc,y=DIC_umol_kg,),color='blue',shape=1,size=4)+
  scale_color_viridis_c(direction=-1)+
  theme_bw()
dicbrev
# ggsave(dicbrev,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/dicbrev.png")

yrDIC=ggplot()+
  geom_point(data=PTSsub3, aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)),shape=16,size=4)+
  geom_point(data=subset(CC,Zone=='Oceanic'), aes(x=jday.utc,y=DIC_umol_kg),color='blue',shape=1,size=4)+
  theme_bw()
yrDIC
# ggsave(yrDIC,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/yrDIC.png")



hist(PTSsub3$bathymetry)
nrow(PTSsub3) # 77
nrow(PTSsub4) # 46


dicta.nBOX=ggplot()+
  geom_point(data=subset(PTSsub3,nBox=='yes'),aes(x=DIC_umol.kg,y=TA_umol.kg),color='orange',size=2)+
  geom_point(data=subset(CC,Zone=='Oceanic'),aes(x=DIC_umol_kg,y=TA_umol_kg),color='blue',size=2)+
  theme_bw()
dicta.nBOX
# ggsave(dicta.nBOX,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/dicta.nBOX.png")

dicta.sal=ggplot()+
  geom_point(data=subset(PTS,nBox=='yes'),aes(x=DIC_umol.kg,y=TA_umol.kg,size=CTDSAL_PSS78),color='orange')+
  geom_point(data=subset(PTS,nTight=='yes'),aes(x=DIC_umol.kg,y=TA_umol.kg,size=CTDSAL_PSS78),color='red')+
  geom_point(data=subset(CC,Zone=='Oceanic'),aes(x=DIC_umol_kg,y=TA_umol_kg,size=Salinity_Bottle),color='blue')+
  theme_bw()
dicta.sal
# ggsave(dicta.sal,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/dicta.nBOX.png")

#without salinity
# ggplot()+
#   geom_point(data=subset(PTS,nBox=='yes'),aes(x=DIC_umol.kg,y=TA_umol.kg),color='orange')+
#   geom_point(data=subset(PTS,nTight=='yes'),aes(x=DIC_umol.kg,y=TA_umol.kg),color='red')+
#   geom_point(data=subset(CC,Zone=='Oceanic'),aes(x=DIC_umol_kg,y=TA_umol_kg),color='blue')+
#   theme_bw()

ggplot()+
  geom_point(data=subset(PTS,nBox=='yes'),aes(x=X,y=Y,color=DIC_umol.kg,size=DIC_umol.kg))+
  scale_color_viridis_c()+
  theme_bw()


#run pca of subsets to compare
#choose what to include in pca
#DIC, TA, pCO2, Temp, pH, Sal, Ara
PCAcols = c("DIC_umol.kg","TA_umol.kg","CTDOXY_umol.kg","pH_TS_20C","CTDSAL_PSS78","CTDTEMP_ITS90") #,
           # "Silicate_umol.kg","Nitrite_and_Nitrate_umol.kg","Ammonium_umol.kg","Phosphate_umol.kg")


install.packages('naniar')
library(naniar)
#where is replace_with_na function from? 
PTS.na= PTS %>% replace_with_na(replace = list(DIC_umol.kg=-999.0, TA_umol.kg=-999.0, CTDSAL_PSS78=-999.000, pH_TS_20C=-999.0000, CTDTEMP_ITS90=-999.000)) 
#Replace all -999 values with NA (will it work for different number of decimal places?)
#this step does not change dimensions
  
# pca.data = PTS[,PCAcols]
# pca.na = pca.data  %>% replace_with_na(replace = list(DIC_umol.kg=-999.0,TA_umol.kg=-999.0,  CTDSAL_PSS78=-999.000, pH_TS_20C=-999.0000,CTDTEMP_ITS90=-999.000)) #, CTDOXY_umol.kg=-999.0,
                                               # Silicate_umol.kg=-999.00,Nitrite_and_Nitrate_umol.kg=-999.000,Ammonium_umol.kg=-999.00,Phosphate_umol.kg=-999.000))
#how many rows do NOT have NA in any parameter
PTS.nogeo = st_drop_geometry(PTS.na)
dim(PTS.nogeo) #204  42
table(PTS.nogeo$nTight)
# no yes 
# 192  12 

PTS.nona = na.omit(PTS.nogeo)
dim(PTS.nona) #152  42
table(PTS.nona$nTight) #I'm losing all my points inside the 14d polygon (and above the keys)
# no yes 
# 149   3 

# pca.data = scale(na.omit(PTS.nogeo[,PCAcols]))
# pca.data = pca.data[complete.cases(pca.data),] #this would also remove NAs

CC.rda2 = prcomp(PTS.nona[,PCAcols],scale=TRUE)
summary(CC.rda2)
dim(PTS.nona[,PCAcols]) #152   6

names(PTS.nona)
plotMe =PTS.nona
# plotMe = subset(PTS.nona, bathymetry<100)
dim(plotMe)# 148  42

par(mfrow=c(1,1))
autoplot(CC.rda2, data = plotMe, colour="nTight", 
         # shape="nTight",
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3,
         loadings.label.vjust = -1.0,
         loadings.label.colour="black")+ #,
         # frame = TRUE, frame.type = 'norm')+
  scale_fill_continuous(limits=c(0,30), breaks=c(0,15,30), guide=guide_colourbar(nbin = 100, draw.ulim = FALSE, draw.llim = FALSE))+
theme_bw()



#look at seasonality 

#add an index column for spatial location
tightYr=ggplot(data=nTightpts,aes(x=DIC_umol.kg,y=TA_umol.kg,color=as.factor(Year_UTC)))+
  geom_point(size=3)+
  # facet_wrap(~Season, ncol=2)+
  theme_bw()+
  theme(legend.position="bottom")
ggsave(tightYr,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/tightYr.png")


#bounding boxes by group: https://stackoverflow.com/questions/54696440/create-polygons-representing-bounding-boxes-for-subgroups-using-sf
