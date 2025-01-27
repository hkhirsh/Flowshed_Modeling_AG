## Clean up this script to plot flowshed for manuscript summary figure
#first I want to look at all inshore site bows (just edit halozone figures for that)
#facet by months or years?
#then limit to only 7 days and look at different times of year

#then zoom in to one site in the middle keys (long key) - use same 2 times

rm(list=ls())
library(PNWColors)
library(paletteer)
library(colorspace)
library(RColorBrewer)
library(ghibli)
library(htmlwidgets)
library(sf)

# install.packages("remotes")
# remotes::install_github("ropensci/hddtools")
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
register_google(key = "AIzaSyBSmQdRPOV8usMAOEkkoH6MISzFjTe5AoE") #new api Feb 2024
FLK_map=get_map(location=c(-80.99306,25.27732),zoom=8,maptype = "satellite")


#Read in yearBows (if not already loaded from running the section above)
shp.list = list.files(path = paste0("/Users/heidi.k.hirsh/Desktop/yearBows_22april2024"), pattern="*shp", full.names = T) 
length(shp.list)
shp.list
shapefile_list = lapply(shp.list,read_sf)

all.yearBows = rbind(shapefile_list[[1]],shapefile_list[[2]],shapefile_list[[3]],shapefile_list[[4]])
yearBows=all.yearBows

#Read in carbonate chemistry station data
CC = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/',na.strings=c(""," "))

#subset the carb chem data to represent the years we want and a subset of SiteIDs
CCyr = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')) #subset of chemistry samples that match bow ties:
## assign visitID columns to allow us to pair CC to the spatial polygon data:
CCyr$visitID_ch1 =  paste(CCyr$SiteID,CCyr$UTCDate_Time)
# head(CCyr$visitID_ch1)
CCyr$visitID_ch2 =    gsub(" ", "_" , CCyr$visitID_ch1, perl=TRUE)
# head(CCyr$visitID_ch2) 
CCyr$visitID =  gsub("[: -]", "" , CCyr$visitID_ch2, perl=TRUE)
# head(CCyr$visitID)

plotCCyr = st_as_sf(CCyr, coords = c("Longitude","Latitude"), crs = st_crs(4326))
mapview(plotCCyr,zcol="SiteID")

#Simplify CCyr to just the columns required for plotting: 
CCyr.s = CCyr[c('visitID','SiteID','Latitude', 'Longitude','Year','Sub_region','Zone','Season','Month','MY','MoY','pointDepth','Precipitation')]
# head(CCyr.s)
names(CCyr.s)

CCbows=NULL
CCbowsALL=NULL

# CCbowsALL = left_join(yearBows, CCyr, by="visitID") 
CCbowsALL = left_join(yearBows, CCyr.s, by="visitID") 
class(CCbowsALL)  #"sf"         "data.frame"
dim(CCbowsALL)  #38640    73
# 38640/14/2 #1380 (4 extra samples)
# CCbows1=na.omit(CCbows0) #no extra

unique(CCbowsALL$Zone)

# CCbows=subset(CCbowsALL, Zone=='Inshore') 
# CCbows=subset(CCbowsALL, Zone=='Inshore' & Sub_region=='MK') 

# CCbows=subset(CCbowsALL, Zone=='Inshore' & simu=='backward')
CCbows=subset(CCbowsALL, Zone=='Inshore' & simu=='backward' & Year==2019) 
unique(CCbows$SiteID)
CCbows$SiteID = factor(CCbows$SiteID, levels = c("1","EK_IN","4","5","UK_IN","7","10","13","16","19","24"))
unique(CCbows$SiteID)
#For manuscript plotting, just use 2019
# CCbows=subset(CCbowsALL, Zone=='Inshore' & simu=='backward' & MoY==6) 

dim(CCbows) 


concave=CCbows
dim(concave)
concave_utm17 = st_transform(concave, 26917)
dim(concave_utm17)



#buffer (extend) out and in
# D=1/111.111*.5 #degrees per km
D=300
concave.bo = st_buffer(concave_utm17, dist=D, nQuadSegs=30)   
dim(concave.bo)


concave.bi = st_buffer(concave.bo, dist=-D, nQuadSegs=30)
dim(concave.bi)

plotData = concave.bi
dim(plotData)

dim(plotData)
plotData_t = st_transform(plotData,crs='+proj=longlat +datum=WGS84')
# unique(plotData$SiteID)
unique(plotData$MY)

# MoYr = unique(plotData$MY)
# MoYr

#set up to loop through this. Label with Month Year. 

FLK_mapGG=get_map(location=c(-80.99306,25.27732),zoom=7,maptype = "satellite")
class(FLK_mapGG)
FLKs1=st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')
class(FLKs1)
# FLKs2=st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Florida_Shoreline_(1_to_40%2C000_Scale)/Florida_Shoreline_(1_to_40%2C000_Scale).shp')
pnw=pnw_palette(name="Bay",n=14,type="continuous")
pnw
# pnw2=pnw_palette(name="Bay",n=4,type="discrete")

my_i=NULL
# my_i
dim(plotData)
# my_i=1
for (my_i in 1:length(MoYr)) {
  
  timeWindow=MoYr[my_i]
  ploThis = subset(plotData,MY==timeWindow)
  ploThis_t = st_transform(ploThis,crs='+proj=longlat +datum=WGS84')
  # dim(ploThis_t)
  
# GG = ggmap(FLK_mapGG)+
  GG = ggplot(FLKs1)+
  geom_sf(fill = "black", lwd = 0)+
  # geom_sf(data=subset(ploThis_t,simu=='forward'),aes(color=n_days,fill=n_days),inherit.aes = FALSE,alpha=0)+
  geom_sf(data=subset(ploThis_t,simu=='backward'),aes(color=Sub_region,fill=Sub_region),inherit.aes = FALSE,alpha=0.03)+
  scale_color_brewer(palette='Dark2')+
  scale_fill_brewer(palette='Dark2')+
  # geom_sf(data=subset(ploThis_t,simu=='backward'),aes(color=n_days,fill=n_days),inherit.aes = FALSE,alpha=0)+
  # scale_color_gradientn(colours=pnw)+
  # scale_fill_gradientn(colours=pnw)+
  geom_point(aes(x=Longitude,y=Latitude),color="blue",data=ploThis_t)+
  ylab('Latitude')+
  xlab('Longitude')+
  # coord_sf(xlim = c(-83.7, -80), ylim = c(23.55, 26), expand = FALSE) + #for backward halos
  # scale_x_continuous(limits=st_bbox(ploThis_t)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(ploThis_t)[c(2,4)])+
  scale_x_continuous(limits=st_bbox(plotData_t)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(plotData_t)[c(2,4)])+
  ggtitle(paste0(timeWindow," backward trajectories, n=7days"))+
  # ggtitle(paste0(timeWindow," forward trajectories"))+
  theme_bw()
GG
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Halo_backward14_Lim/",timeWindow,"_Halozone14bLim.png"),plot=GG,width=10, height=10, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FLKpolys_backward/byRegion7/",timeWindow,"_FLK_7b_byReg.png"),plot=GG,width=10, height=10, dpi = 300)
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FLKpolys_backward/byRegionShade7/",timeWindow,"_FLK_7b_byReg.png"),plot=GG,width=10, height=10, dpi = 300)


}
  
# #Make animations for Enrique: 
# ## Animate gifs to compare trajectories
# 
# #Forward
# list.files(path='/Users/heidi.k.hirsh/Desktop/Halo_forward14_Lim', pattern = '*.png', full.names = TRUE) %>% 
#   mixedsort() %>%
#   image_read() %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=2) %>% # animates, can opt for number of loops
#   image_write("/Users/heidi.k.hirsh/Desktop/HaloForward14.gif") # write to current dir
# 
# 
# #Backward
# list.files(path='/Users/heidi.k.hirsh/Desktop/Halo_backward14_Lim', pattern = '*.png', full.names = TRUE) %>% 
#   mixedsort() %>%
#   image_read() %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=2) %>% # animates, can opt for number of loops
#   image_write("/Users/heidi.k.hirsh/Desktop/HaloBackward14.gif") # write to current dir

#pair Site ID with specific color so it works even when only a single site plotted
library(wesanderson)
library(pals)
library(Polychrome)
library(rcartocolor) #use safe for colorblind safe colors

nColor <- length(unique(plot_a$SiteID))
scales::show_col(carto_pal(nColor, "Safe"))
safe_pal <- carto_pal(nColor, "Safe")

#!!Replace greens with standalone visible color!!
clrs0 = rainbow(nColor)
scales::show_col(clrs0)
clrs = rainbow(nColor, start = 0, end =0.8)
plot(clrs)

unique(plot_a$SiteID)

#SIMPLE
# site_colors <- safe_pal
site_colors <- clrs0
site_COLOR_scale  <- scale_color_manual(name = "Site", values = site_colors,
                                        breaks = c('1','EK_IN','UK_IN','7','10','13','16','19','24'),
                                        labels = c('1','EK_IN','UK_IN','7','10','13','16','19','24'))
# breaks = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'),
# labels = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'))                                        

site_FILL_scale  <- scale_fill_manual(name = "Site", values = site_colors,
                                      breaks = c('1','EK_IN','UK_IN','7','10','13','16','19','24'),
                                      labels = c('1','EK_IN','UK_IN','7','10','13','16','19','24'))                                 

# class0_colors = c('forestgreen','khaki','lightslateblue','steelblue1',
#                   'darkgray','white','orange4','red')
# class0_colors = c('magenta','khaki','forestgreen','white','orange4','red')
unique(over_boxF$ClassLv0)
#[1] "Seagrass"                  "Unconsolidated Sediment"   "Coral Reef and Hardbottom" "Land"                      "Not Classified"           
# [6] "Mangrove"                  "Artificial"     
class0_FILL_scale = scale_fill_manual(name = "Habitat Class 0", values = class0_colors,
                                      breaks = c("Seagrass","Unconsolidated Sediment","Coral Reef and Hardbottom",
                                                 "Land","Not Classified","Mangrove","Artificial"),
                                      labels = c("Seagrass","Unconsolidated Sediment","Coral Reef and Hardbottom",
                                                  "Land","Not Classified","Mangrove","Artificial"))
  
class0_COLOR_scale = scale_color_manual(name = "Habitat Class 0", values = class0_colors,
                                       breaks = c("Seagrass","Unconsolidated Sediment","Coral Reef and Hardbottom",
                                                  "Land","Not Classified","Mangrove","Artificial"),
                                       labels = c("Seagrass","Unconsolidated Sediment","Coral Reef and Hardbottom",
                                                   "Land","Not Classified","Mangrove","Artificial"))

classN_colors = c('deeppink2','forestgreen','steelblue1','khaki',
                  'saddlebrown','white','gray50','purple','purple')
# unique(over_boxF$ClassLvNew)
# length(unique(over_boxF$ClassLvNew))
# unique(coral_sf$ClassLvNew)
# [1] "Coral Reef"              "Unconsolidated Sediment" "Seagrass"                "Not Classified"          "Pavement"               
# [6] "Land"                    "Mangrove"                "Artificial"              "Dredged/Excavated"      

classN_FILL_scale = scale_fill_manual(name = "Habitat Class 0.5", values = classN_colors,
                                      breaks = c("Coral Reef","Seagrass","Pavement","Unconsolidated Sediment",
                                                 "Mangrove","Not Classified","Land","Artificial","Dredged/Excavated"),
                                      labels = c("Coral Reef","Seagrass","Pavement","Unconsolidated Sediment",
                                                 "Mangrove","Not Classified","Land","Artificial","Dredged/Excavated"))

classN_COLOR_scale = scale_color_manual(name = "Habitat Class 0.5", values = classN_colors,
                                        breaks = c("Coral Reef","Seagrass","Pavement","Unconsolidated Sediment",
                                                   "Mangrove","Not Classified","Land","Artificial","Dredged/Excavated"),
                                        labels = c("Coral Reef","Seagrass","Pavement","Unconsolidated Sediment",
                                                   "Mangrove","Not Classified","Land","Artificial","Dredged/Excavated"))


#class 1 (ultimately make my own class 1.5)
unique(over_boxF$ClassLv1)
length(unique(over_boxF$ClassLv1))
# [1] "Seagrass (Continuous)"                           "Seagrass (Discontinuous)"                        "Unconsolidated Sediment"                        
# [4] "Individual or Aggregated Patch Reef"             "Land"                                            "Pavement"                                       
# [7] "Not Classified"                                  "Mangrove"                                        "Artificial"                                     
# [10] "Scattered Coral/Rock in Unconsolidated Sediment" "Aggregate Reef"                                  "Reef Rubble"     
class1_colors = c('forestgreen','forestgreen','khaki','magenta',
                  'darkgray','steelblue1','white','orange4','red',
                  'hotpink1','magenta','lightsalmon')
# class1_colors = c('forestgreen','chartreuse3','khaki','lightslateblue',
#                   'darkgray','dodgerblue1','white','orange4','red',
#                   'goldenrod','magenta','hotpink4')

class1_FILL_scale = scale_fill_manual(name = "Habitat Class 0", values = class1_colors,
                                      breaks = c("Seagrass (Continuous)","Seagrass (Discontinuous)","Unconsolidated Sediment","Individual or Aggregated Patch Reef",
                                                 "Land","Pavement","Not Classified","Mangrove","Artificial",
                                                 "Scattered Coral/Rock in Unconsolidated Sediment","Aggregate Reef","Reef Rubble"),
                                      labels = c("Seagrass (Continuous)","Seagrass (Discontinuous)","Unconsolidated Sediment","Individual or Aggregated Patch Reef",
                                                 "Land","Pavement","Not Classified","Mangrove","Artificial",
                                                 "Scattered Coral/Rock in Unconsolidated Sediment","Aggregate Reef","Reef Rubble"))

class1_COLOR_scale = scale_color_manual(name = "Habitat Class 0", values = class1_colors,
                                        breaks = c("Seagrass (Continuous)","Seagrass (Discontinuous)","Unconsolidated Sediment","Individual or Aggregated Patch Reef",
                                                   "Land","Pavement","Not Classified","Mangrove","Artificial",
                                                   "Scattered Coral/Rock in Unconsolidated Sediment","Aggregate Reef","Reef Rubble"),
                                        labels = c("Seagrass (Continuous)","Seagrass (Discontinuous)","Unconsolidated Sediment","Individual or Aggregated Patch Reef",
                                                   "Land","Pavement","Not Classified","Mangrove","Artificial",
                                                   "Scattered Coral/Rock in Unconsolidated Sediment","Aggregate Reef","Reef Rubble"))

# #panel a: all inshore sites 7days back, July 2019
# head(plotData_t)


#++++++Habitat INFO
#

#habitat data: 
#benthic data
# coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb",layer="UnifiedReefMap")
habitat = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/')
coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer="UnifiedReefMap")


# writeHab = st_drop_geometry(coral_sf)
# st_write(writeHab,'/Users/heidi.k.hirsh/Desktop/habitatclasses.csv')

#define new class between class 0 and class 1 that separates pavement as it's own category

coral_sf$ClassLvNew = NA
# head(coral_sf)
# coral_sf$ClassLv01[which(coral_sf$ClassLv1=='Pavement')] = "Pavement"
# coral_sf$ClassLv01[]


for (i in 1:length(coral_sf$ClassLv0)) {
  if (coral_sf$ClassLv1[i]=="Pavement") {coral_sf$ClassLvNew[i] = "Pavement"} 
  else {coral_sf$ClassLvNew[i] = coral_sf$ClassLv0[i] }
}

#Coral Reef and Hardbottom class should now be just "coral reef"
coral_sf$ClassLvNew[which(coral_sf$ClassLvNew=="Coral Reef and Hardbottom")] = "Coral Reef"

unique(coral_sf$ClassLvNew)
# head(coral_sf)

# coral.classes =  coral_sf %>% select('ClassLv0','ClassLv1','ClassLv2','ClassLv3','ClassLv4')

#define plot_a to plot_d here: 
plot_a = subset(plotData_t,n_days %in% c(1,7,14) & MY=="2019-07")
plot_b = subset(plotData_t,n_days %in% c(1,7,14) & MY=="2019-09")
plot_c = subset(plotData_t,SiteID==7 & MY=="2019-07")
plot_d = subset(plotData_t,SiteID==7 & MY=="2019-09")
plot_e = subset(plotData_t,SiteID==7 & MY=="2019-07" & n_days==7)
plot_f = subset(plotData_t,SiteID==7 & MY=="2019-09" & n_days==7)

#Habitat around Long key (combine c and d boundaries)
#use bbox for D
lati = c(-81.2,-80)
longi = c(24.75, 25.6)

poly_coord_df = data.frame(longi,lati)
polyBox = poly_coord_df %>% 
  st_as_sf(coords=c("longi","lati"), crs=4326) %>% 
  st_bbox() #%>% 
  # st_as_sfc()
# polyBox

##C
#Tranform coral_sf to match Bbow CRS:
coral_sfC.t = st_transform(coral_sf,st_crs(plot_c))
# names(coral_sfC.t)
coral_sfC.tv = coral_sfC.t[which(st_is_valid(coral_sfC.t)),]
# names(coral_sfC.tv)
over_bowC = st_intersection(coral_sfC.tv,plot_c)
# names(over_bowC)

#Habitat around Long key (combine c and d boundaries)
#use bbox for D
# lati = c(-81.2,-80)
# longi = c(24.75, 25.6)
# 
# poly_coord_df = data.frame(longi,lati)
# polyBox = poly_coord_df %>% 
#   st_as_sf(coords=c("longi","lati"), crs=4326) %>% 
#   st_bbox() %>% 
#   st_as_sfc(st_crs=4326) %>% 
#   st_sf %>% st_cast
# class(polyBox)
# crs(polyBox)
# # crs(polyBox)=st_crs(plot_c)
# polyBox2=polyBox %>% st_as_sfc(st_crs=4326)
# # class(plot_c)
# class(plot_c)
# class(polyBox)
# class(polyBox2)
# 
# #limit coral_sf habitat map to polyBox
# # polyBox.good = polyBox %>% st_as_sfc(st_crs=4326) %>% st_sf %>% st_cast
# 
# over_Hab = st_intersection(coral_sfD.tv, polyBox2)
# head(over_Hab )



##E
# Define bounding box around all 14day polygons
bowBoxE = st_bbox(plot_c) %>% st_as_sfc(st_crs=4326) %>% st_sf %>% st_cast
bowBoxE.bo=st_buffer(bowBoxE, dist=2000) #, nQuadSegs=30)
over_boxE <- st_intersection(coral_sfC.tv,bowBoxE.bo)
crs(coral_sfC.tv)==crs(bowBoxE.bo)

names(coral_sfC.tv)
class(bowBoxE.bo)
class(bowBoxE)

##D
coral_sfD.t = st_transform(coral_sf,st_crs(plot_d))
coral_sfD.tv = coral_sfD.t[which(st_is_valid(coral_sfD.t)),]
over_bowD = st_intersection(coral_sfD.tv,plot_d)
names(coral_sfD.tv)

##F
bowBoxF = st_bbox(plot_d) %>% st_as_sfc(st_crs=4326) %>% st_sf %>% st_cast
bowBoxF.bo=st_buffer(bowBoxF, dist=2000) #, nQuadSegs=30)
over_boxF <- st_intersection(coral_sfD.tv,bowBoxF.bo)
names(over_boxF)

biggerBoxF.bo = st_buffer(bowBoxF, dist=60000) 
over_bigF = st_intersection(coral_sfD.tv,biggerBoxF.bo)
#End habitat info

dim(coral_sfD.tv)
dim(coral_sfC.tv)
coral_sfD.tv==coral_sfC.tv

#coral reef and hard bottom (Class1) = Aggregate reef, Individual or Aggregated Patch Reef, Pavement, Reef Rubble
#make new class 0.5  where coral reef = Aggregate reef, Individual or Aggregated Patch Reef, Reef Rubble and new pavement class with only pavement 


# names(over_boxF)
# # writeF = subset(over_boxF,select=-c(  Shape_Leng,Shape_Length,Shape_Area,Shape))
# writeF = st_drop_geometry(over_boxF)
# names(writeF)
# st_write(writeF,'/Users/heidi.k.hirsh/Desktop/habitat_F.csv')




# plot_a = subset(plotData_t,n_days %in% c(1,7,14) & MY=="2019-07")
length(unique(plot_a$SiteID)) #9
# unique(plot_a$n_days)
# unique(plot_a$MY)
# plot_a = subset(plotData_t,MY=="2019-07")
# plot_a = subset(plotData_t,n_days<8 & MY=="2019-07")
AA = ggplot(FLKs1)+
    geom_sf(fill = "darkgray", lwd = 0)+
    geom_sf(data=subset(plot_a,simu=='backward'),aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.05)+
    # geom_sf(data=subset(plot_a,simu=='backward'),aes(color=Latitude,fill=Latitude),inherit.aes = FALSE,alpha=0.05)+
    # geom_sf(data=subset(plot_a,simu=='backward'),aes(color=visitID,fill=visitID),inherit.aes = FALSE,alpha=0.05)+
    # geom_sf(data=subset(plot_a,simu=='backward'),aes(color=Sub_region,fill=Sub_region),inherit.aes = FALSE,alpha=0.05)+
    site_COLOR_scale+
    site_FILL_scale+
    # scale_color_gradientn(colours=pnw)+
    # scale_fill_gradientn(colours=pnw)+
    # scale_color_brewer(palette='Dark2')+
    # scale_fill_brewer(palette='Dark2')+
    geom_point(aes(x=Longitude,y=Latitude),color="black",data=plot_a)+
    ylab('Latitude')+
    xlab('Longitude')+
    # scale_x_continuous(limits=st_bbox(plotData_t)[c(1,3)])+
    # scale_y_continuous(limits=st_bbox(plotData_t)[c(2,4)])+
    coord_sf(xlim = c(-82.3, -80), ylim = c(24.4, 26.2), expand = FALSE) + #for backward halos
    # ggtitle(paste0(plot_a$MY," backward trajectories"))+
    ggtitle("July 2019: 1, 7, and 14-day backward trajectories")+
  # ggtitle("2019-07 inshore backward trajectories (1,7,14d)")+
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position="none")
# AA

# plot_b = subset(plotData_t,n_days %in% c(1,7,14) & MY=="2019-09")
# plot_b = subset(plotData_t,MY=="2019-09")
# plot_b = subset(plotData_t,n_days<8 & MY=="2019-09")
# length(unique(plot_b$SiteID)) 
BB = ggplot(FLKs1)+
    geom_sf(fill = "darkgray", lwd = 0)+
    geom_sf(data=subset(plot_b,simu=='backward'),aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.05)+
    # geom_sf(data=subset(plot_b,simu=='backward'),aes(color=Latitude,fill=Latitude),inherit.aes = FALSE,alpha=0.05)+
  # geom_sf(data=subset(plot_b,simu=='backward'),aes(color=visitID,fill=visitID),inherit.aes = FALSE,alpha=0.05)+
    site_COLOR_scale+
    site_FILL_scale+
    # scale_color_gradientn(colours=pnw)+
    # scale_fill_gradientn(colours=pnw)+
    # geom_sf(data=subset(plot_b,simu=='backward'),aes(color=Sub_region,fill=Sub_region),inherit.aes = FALSE,alpha=0.05)+    scale_color_brewer(palette='Dark2')+
    # scale_color_brewer(palette='Dark2')+
    # scale_fill_brewer(palette='Dark2')+
    geom_point(aes(x=Longitude,y=Latitude),color="black",data=plot_b)+
    ylab('Latitude')+
    xlab('Longitude')+
  # scale_x_continuous(limits=st_bbox(plotData_t)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(plotData_t)[c(2,4)])+
  coord_sf(xlim = c(-82.3, -80), ylim = c(24.4, 26.2), expand = FALSE) + #for backward halos
    # ggtitle(paste0(timeWindow," backward trajectories, n=7days"))+
    # ggtitle(paste0(MY," backward trajectories"))+
    # ggtitle("2019-09 inshore backward trajectories (1,7,14d)")+
  ggtitle("Sept 2019: 1, 7, and 14-day backward trajectories")+
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position="none")
# BB
  
# plot_c = subset(plotData_t,SiteID==7 & MY=="2019-07")
CC = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  # geom_sf(data=subset(plot_c,simu=='backward'),aes(color=Sub_region,fill=Sub_region),inherit.aes = FALSE,alpha=0.03)+
  geom_sf(data=subset(plot_c,simu=='backward'),aes(color=n_days),inherit.aes = FALSE,alpha=0)+
  scale_color_gradientn(colours=pnw, breaks=c(1,7,14),labels=c('1 day','7 days','14 days'))+
  # scale_fill_gradientn(colours=pnw)+
  geom_point(aes(x=Longitude,y=Latitude),color="black",data=plot_c)+
  ylab('Latitude')+
  xlab('Longitude')+
  # scale_x_continuous(limits=st_bbox(plotData_t)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(plotData_t)[c(2,4)])+
  coord_sf(xlim = c(-81.2, -80), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
  ggtitle("July 2019: 1-14 day backward trajectories")+
  # ggtitle("2019-07 1-14d backward trajectories, Long Key")+
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position="none")
  # theme(legend. title = element_blank()) 
# CC

# plot_d = subset(plotData_t,SiteID==7 & MY=="2019-09")
DD = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  # geom_sf(data=subset(plot_c,simu=='backward'),aes(color=Sub_region,fill=Sub_region),inherit.aes = FALSE,alpha=0.03)+
  geom_sf(data=subset(plot_d,simu=='backward'),aes(color=n_days),inherit.aes = FALSE,alpha=0)+
  scale_color_gradientn(colours=pnw, breaks=c(1,7,14),labels=c('1 day','7 days','14 days'))+
  # scale_fill_gradientn(colours=pnw)+
  geom_point(aes(x=Longitude,y=Latitude),color="black",data=plot_c)+
  ylab('Latitude')+
  xlab('Longitude')+
  # scale_x_continuous(limits=st_bbox(plotData_t)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(plotData_t)[c(2,4)])+
  coord_sf(xlim = c(-81.2, -80), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
  ggtitle("Sept 2019: 1-14 day backward trajectories")+
  # ggtitle("2019-09 1-14d backward trajectories, Long Key")+
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  # theme(legend.position="none")
# DD


# plot_e = subset(plotData_t,SiteID==7 & MY=="2019-07" & n_days==7)
EE = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  # geom_sf(data=subset(plot_c,n_days %in% c(1,7,14)),color='black',lwd=.4,linetype='dotted',inherit.aes = FALSE,alpha=0)+
  # geom_sf(data=over_boxE,aes(color=ClassLv4, fill=ClassLv4),alpha=.3,lwd=.1)+   #plot all habitat in frame (buffer out bounding box for bows)
  # geom_sf(data=subset(over_bowC,n_days==7),aes(color=ClassLv4, fill=ClassLv4))+ #,color=~factpal(ClassLv4))+
  # geom_sf(data=over_boxE,aes(color=ClassLv0, fill=ClassLv0),lwd=.1)+
  # geom_sf(data=subset(over_bowC,n_days==7),aes(color=ClassLv0, fill=ClassLv0))+ #,color=~factpal(ClassLv4))+
  # class0_COLOR_scale+
  # class0_FILL_scale+
  
  # geom_sf(data=over_boxE,aes(color=ClassLv1, fill=ClassLv1),lwd=.1)+
  # geom_sf(data=subset(over_bowC,n_days==7),aes(color=ClassLv1, fill=ClassLv1))+ #,color=~factpal(ClassLv4))+
  # class1_COLOR_scale+
  # class1_FILL_scale+
  
  
  geom_sf(data=over_boxE,aes(color=ClassLvNew,fill=ClassLvNew),lwd=0,alpha=.5)+
  geom_sf(data=subset(over_bowC,n_days==7),aes(color=ClassLvNew, fill=ClassLvNew))+ #,color=~factpal(ClassLv4))+
  classN_COLOR_scale+
  classN_FILL_scale+
  
  # geom_sf(data=over_bow,aes(color=ClassLv4, fill=ClassLv4),alpha=.2,lwd=.2)+ #,color=~factpal(ClassLv4))+
  geom_sf(data=subset(plot_c,n_days %in% c(1,7,14)),color='black',lwd=.5,linetype='dashed',inherit.aes = FALSE,alpha=0)+
  geom_sf(data=subset(plot_c,n_days==7),color='black',lwd=.7,inherit.aes = FALSE,alpha=0)+
  
  # geom_sf(data=subset(plot_c,n_days %in% c(1,7,14)),color='black',lwd=.6,linetype='dashed',inherit.aes = FALSE,alpha=0)+
  geom_point(aes(x=Longitude,y=Latitude),color="black",data=plot_c)+
  
  geom_sf(fill = "darkgray", lwd = 0)+
  ylab('Latitude')+
  xlab('Longitude')+
  # scale_x_continuous(limits=st_bbox(plot_c)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(plot_c)[c(2,4)])+
  
  coord_sf(xlim = c(-81.2, -80), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
  
  # coord_sf(xlim = c(-81.2, -80), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
  ggtitle("July 2019: 7-day habitat exposure")+
  # ggtitle("2019-07 7d Habitat Exposure, Long Key")+
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  # theme(legend.position="none")
  
  
  # geom_sf(fill = "darkgray", lwd = 0)+
  # # geom_sf(data=subset(plot_c,simu=='backward'),aes(color=n_days,fill=n_days),inherit.aes = FALSE,alpha=0)+
  # # geom_sf(data=subset(plot_e,simu=='backward'),aes(color=n_days,fill=n_days),inherit.aes = FALSE,alpha=0)+
  # # geom_sf(data=subset(plot_e,simu=='backward'),color='black',lwd=.7,inherit.aes = FALSE,alpha=0)+
  # geom_sf(data=subset(plot_c,n_days %in% c(1,7,14)),color='black',lwd=.4,linetype='dotted',inherit.aes = FALSE,alpha=0)+
  # geom_sf(data=subset(plot_c,n_days==7),color='black',lwd=.7,inherit.aes = FALSE,alpha=0)+
  # # scale_color_gradientn(colours=pnw)+
  # # scale_fill_gradientn(colours=pnw)+
  # geom_point(aes(x=Longitude,y=Latitude),color="black",data=plot_c)+
  # ylab('Latitude')+
  # xlab('Longitude')+
  # scale_x_continuous(limits=st_bbox(plot_c)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(plot_c)[c(2,4)])+
  # # coord_sf(xlim = c(-81.2, -80), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
  # ggtitle("2019-07 7d Habitat Exposure, Long Key")+
  # theme_bw()
# EE

# plot_f = subset(plotData_t,SiteID==7 & MY=="2019-09" & n_days==7)
FF = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  # geom_sf(data=subset(plot_c,n_days %in% c(1,7,14)),color='black',lwd=.4,linetype='dotted',inherit.aes = FALSE,alpha=0)+
  # geom_sf(data=over_boxF,aes(color=ClassLv4, fill=ClassLv4),alpha=.3,lwd=.1)+   #plot all habitat in frame (buffer out bounding box for bows)
  # geom_sf(data=subset(over_bowD,n_days==7),aes(color=ClassLv4, fill=ClassLv4))+ #,color=~factpal(ClassLv4))+
  # geom_sf(data=over_boxF,aes(color=ClassLv0, fill=ClassLv0),lwd=.1)+ 
  # geom_sf(data=subset(over_bowD,n_days==7),aes(color=ClassLv0, fill=ClassLv0))+ #,color=~factpal(ClassLv4))+
  # class0_COLOR_scale+
  # class0_FILL_scale+

  # geom_sf(data=over_boxF,aes(color=ClassLv1, fill=ClassLv1),lwd=.1)+
  # geom_sf(data=subset(over_bowD,n_days==7),aes(color=ClassLv1, fill=ClassLv1))+ #,color=~factpal(ClassLv4))+
  # class1_COLOR_scale+
  # class1_FILL_scale+
  
  geom_sf(data=over_boxF,aes(color=ClassLvNew,fill=ClassLvNew),lwd=0,alpha=.5)+
  geom_sf(data=subset(over_bowD,n_days==7),aes(color=ClassLvNew, fill=ClassLvNew))+ #,color=~factpal(ClassLv4))+
  classN_COLOR_scale+
  classN_FILL_scale+
  
  # geom_sf(data=over_bow,aes(color=ClassLv4, fill=ClassLv4),alpha=.2,lwd=.2)+ #,color=~factpal(ClassLv4))+
  geom_sf(data=subset(plot_d,n_days==7),color='black',lwd=.7,inherit.aes = FALSE,alpha=0)+
  geom_sf(data=subset(plot_d,n_days %in% c(1,7,14)),color='black',lwd=.5,linetype='dashed',inherit.aes = FALSE,alpha=0)+
  geom_point(aes(x=Longitude,y=Latitude),color="black",data=plot_d)+
  
  geom_sf(fill = "darkgray", lwd = 0)+
  ylab('Latitude')+
  xlab('Longitude')+
  scale_x_continuous(limits=st_bbox(plot_d)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(plot_d)[c(2,4)])+
  # coord_sf(xlim = c(-81.2, -80), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
  # ggtitle("2019-09 7d Habitat Exposure, Long Key")+
  ggtitle("Sept 2019: 7-day habitat exposure")+
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        # plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())#,
        # panel.border = element_blank())
  # theme(legend.position="none")

  # geom_sf(fill = "darkgray", lwd = 0)+
  # # geom_sf(data=subset(plot_d,simu=='backward'),aes(color=n_days,fill=n_days),inherit.aes = FALSE,alpha=0)+
  # # geom_sf(data=subset(plot_f,simu=='backward'),aes(color=n_days,fill=n_days),inherit.aes = FALSE,alpha=0)+
  # geom_sf(data=subset(plot_d,n_days %in% c(1,7,14)),color='black',lwd=.4,linetype='dotted',inherit.aes = FALSE,alpha=0)+
  # geom_sf(data=subset(plot_d,n_days==7),color='black',lwd=.7,inherit.aes = FALSE,alpha=0)+
  # # scale_color_gradientn(colours=pnw)+
  # # scale_fill_gradientn(colours=pnw)+
  # geom_point(aes(x=Longitude,y=Latitude),color="black",data=plot_d)+
  # ylab('Latitude')+
  # xlab('Longitude')+
  # scale_x_continuous(limits=st_bbox(plot_d)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(plot_d)[c(2,4)])+
  # # coord_sf(xlim = c(-81.2, -80), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
  # ggtitle("2019-07 7d Habitat Exposure, Long Key")+
  # theme_bw()
# FF


# AB = plot_grid(AA,BB,CC,DD,
#             labels=c("A","B","C","D"))

AB = plot_grid(AA,BB,CC,DD,EE,FF,
               labels=c("A","B","C","D","E","F"),ncol=2)

AB2 = plot_grid(AA,BB,CC,DD,HAB,
               labels=c("A","B","C","D","E"),ncol=2)

#print timestamp: 
TS=Sys.time()
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Test_Figs/test_",TS,".png"),plot=AB2,width=10, height=10, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Test_Figs/ff_",TS,".png"),plot=FF,width=10, height=10, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Test_Figs/ee_",TS,".png"),plot=EE,width=10, height=10, dpi = 300)




#combo habitat map

HAB = ggplot(FLKs1)+
  # geom_sf(fill = "gray27", lwd = 0)+


  geom_sf(data=over_bigF,aes(color=ClassLvNew,fill=ClassLvNew),lwd=0,alpha=.7)+
  geom_sf(fill = "gray50", lwd = 0)+
  
  # geom_sf(data=over_boxE,aes(color=ClassLvNew,fill=ClassLvNew),lwd=0,alpha=.7)+
  # geom_sf(data=subset(over_bowC,n_days==7),aes(color=ClassLvNew, fill=ClassLvNew),lwd=0,alpha=.7)+ #,color=~factpal(ClassLv4))+
  geom_sf(data=subset(plot_c,n_days %in% c(7,14)),color='black',lwd=.7,linetype='twodash',inherit.aes = FALSE,alpha=0)+
  geom_sf(data=subset(plot_c,n_days==7),color='black',lwd=.87,inherit.aes = FALSE,alpha=0)+
  # geom_point(aes(x=Longitude,y=Latitude),shape="\u2605",color="black",data=plot_c)+
  
  # geom_sf(data=over_boxF,aes(color=ClassLvNew,fill=ClassLvNew),lwd=0,alpha=.7)+
  # geom_sf(data=subset(over_bowD,n_days==7),aes(color=ClassLvNew, fill=ClassLvNew),lwd=0,alpha=.7)+ #,color=~factpal(ClassLv4))+
  geom_sf(data=subset(plot_d,n_days==7),color='black',lwd=.87,inherit.aes = FALSE,alpha=0)+
  geom_sf(data=subset(plot_d,n_days %in% c(7,14)),color='black',lwd=.7,linetype='twodash',inherit.aes = FALSE,alpha=0)+
  # geom_point(aes(x=Longitude,y=Latitude),color="black",data=plot_d)+
  
  # geom_point(aes(x=Longitude,y=Latitude),shape=1,fill='black',color='white',stroke=1.5,size=3,data=plot_c)+
  geom_point(aes(x=Longitude,y=Latitude),color='black',size=3,data=plot_c)+
  
  classN_COLOR_scale+
  classN_FILL_scale+
  

  ylab('Latitude')+
  xlab('Longitude')+
  # scale_x_continuous(limits=st_bbox(plot_c)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(plot_c)[c(2,4)])+
  coord_sf(xlim = c(-81.2, -80), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
  

  # ggtitle("July 2019: 7-day habitat exposure")+
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# theme(legend.position="none")

TS=Sys.time()
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Test_Figs/HAB_",TS,".png"),plot=HAB,width=10, height=10, dpi = 300)












# ee = ggplot(FLKs1)+
#   geom_sf(fill = "darkgray", lwd = 0)+
#   # geom_sf(data=subset(plot_c,n_days %in% c(1,7,14)),color='black',lwd=.4,linetype='dotted',inherit.aes = FALSE,alpha=0)+
#   geom_sf(data=over_boxE,aes(color=ClassLv4, fill=ClassLv4),alpha=.3,lwd=.1)+   #plot all habitat in frame (buffer out bounding box for bows)
#   geom_sf(data=subset(over_bowC,n_days==7),aes(color=ClassLv4, fill=ClassLv4))+ #,color=~factpal(ClassLv4))+
#   # geom_sf(data=over_bow,aes(color=ClassLv4, fill=ClassLv4),alpha=.2,lwd=.2)+ #,color=~factpal(ClassLv4))+
#   geom_sf(data=subset(plot_c,n_days %in% c(1,7,14)),color='black',lwd=.5,linetype='dashed',inherit.aes = FALSE,alpha=0)+
#   geom_sf(data=subset(plot_c,n_days==7),color='black',lwd=.7,inherit.aes = FALSE,alpha=0)+
#   
#   # geom_sf(data=subset(plot_c,n_days %in% c(1,7,14)),color='black',lwd=.6,linetype='dashed',inherit.aes = FALSE,alpha=0)+
#   geom_point(aes(x=Longitude,y=Latitude),color="black",data=plot_c)+
#   ylab('Latitude')+
#   xlab('Longitude')+
#   scale_x_continuous(limits=st_bbox(plot_c)[c(1,3)])+
#   scale_y_continuous(limits=st_bbox(plot_c)[c(2,4)])+
#   # coord_sf(xlim = c(-81.2, -80), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
#   ggtitle("July 2019: 7-day habitat exposure")+
#   # ggtitle("2019-07 7d Habitat Exposure, Long Key")+
#   theme_bw()
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Test_Figs/ee_",TS,".png"),plot=ee,width=10, height=10, dpi = 300)
# 
# 
# 
# 
# 
# #habitat data: 
# #benthic data
# coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb",layer="UnifiedReefMap")
# habitat = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/')
# coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer="UnifiedReefMap")
# 
# bowTest = plot_e
# dim(bowTest)
# #Tranform coral_sf to match Bbow CRS:
# coral_sf.t = st_transform(coral_sf,st_crs(bowTest))
# coral_sf.tv = coral_sf.t[which(st_is_valid(coral_sf.t)),]
# over_bow = st_intersection(coral_sf.tv,bowTest)
# head(over_bow)
# 
# 
# # Define bounding box around all 14day polygons
# bowBox = st_bbox(plot_c) %>% st_as_sfc(st_crs=4326) %>% st_sf %>% st_cast
# # Boxt = st_transform(Box, st_crs(BowTest)) #I'm not sure why I have to transform again
# #buffer out box
# bowBox.bo=st_buffer(bowBox, dist=2000) #, nQuadSegs=30) 
# class(bowBox)
# class(bowBox.bo)
# 
# # plot(bowBox)
# 
# over_box <- st_intersection(coral_sf.tv,bowBox.bo)
# # plot(over_box)
# 
# 
# # factpal <- colorFactor(rev(terrain.colors(30)), over_bow$ClassLv4)
# factpal <- colorFactor(rev(rainbow(30)), over_bow$ClassLv4) #how to limit classes to only what is inside polygon?
# 
# # BOUNDARIES ON TOP OF HABITAT
# # clrs = rainbow(7, start = 0, end = 0.8)
# # clrs_rev <- rev(rainbow(7, start = 0, end = 0.8))
# # d=ploThis_t$ndays
# # pal = colorNumeric(palette = clrs_rev, domain = min(d):max(d))
# 
# # leaflet() %>% 
# #   addProviderTiles('Esri.WorldImagery') %>%
# #   # addPolygons(data=subset(over_bow,simu=='backward')) %>%
# #   
# #   addPolygons(data=subset(over_bow,simu=='backward'),fillOpacity=1,weight=.2,color=~factpal(ClassLv4)) %>%
# #   # addPolygons(data=over_bow,color='ClassLv4',fillOpacity=1) %>%
# #   
# #   addPolygons(data=subset(bowTest,simu=='backward'),fillOpacity=.0,weight=3) %>%
# #   
# #   addLegend(
# #     'bottomright',
# #     pal = factpal,
# #     values =  over_bow$ClassLv4,
# #     opacity = 1,
# #     labels = over_bow$ClassLv4,
# #     title = "habitat"
# #   )
# 
# 
# #now try ggplot
# #color habitat inside polygon
# ggplot(FLKs1)+
#   geom_sf(fill = "darkgray", lwd = 0)+
#   # geom_sf(data=subset(plot_c,n_days %in% c(1,7,14)),color='black',lwd=.4,linetype='dotted',inherit.aes = FALSE,alpha=0)+
#   geom_sf(data=over_box,aes(color=ClassLv4, fill=ClassLv4),alpha=.5,lwd=.1)+   #plot all habitat in frame (buffer out bounding box for bows)
#   geom_sf(data=over_bow,aes(color=ClassLv4, fill=ClassLv4))+ #,color=~factpal(ClassLv4))+
#   # geom_sf(data=over_bow,aes(color=ClassLv4, fill=ClassLv4),alpha=.2,lwd=.2)+ #,color=~factpal(ClassLv4))+
#   geom_sf(data=subset(plot_c,n_days==7),color='black',lwd=.7,inherit.aes = FALSE,alpha=0)+
#   geom_sf(data=subset(plot_c,n_days %in% c(1,7,14)),color='black',lwd=.6,linetype='dashed',inherit.aes = FALSE,alpha=0)+
#   geom_point(aes(x=Longitude,y=Latitude),color="black",data=plot_c)+
#   ylab('Latitude')+
#   xlab('Longitude')+
#   scale_x_continuous(limits=st_bbox(plot_c)[c(1,3)])+
#   scale_y_continuous(limits=st_bbox(plot_c)[c(2,4)])+
#   # coord_sf(xlim = c(-81.2, -80), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
#   ggtitle("2019-07 7d Habitat Exposure, Long Key")+
#   theme_bw()+
#   theme(legend.position="none")
# 
# 
# 
# 
# # P9 = createPalette(9,  c("#DB6D00", "#009292", "#490092"))
# # swatch(P9)
# # Glasbey = glasbey.colors(9)
# # swatch(Glasbey)
# 
# 
# # star = pnw_palette(name="Starfish",n=length(unique(plot_a$SiteID)),type="discrete")
# # disCol = paletteer_d("colorBlindness::paletteMartin",n=length(unique(plot_a$SiteID)))
# # wes=wes_palette() #better for continuous
# 
# 
# # 
# # #just plot A and B by siteID
# # sites=unique(plot_a$SiteID)
# # sites
# # 
# # #SIMPLE
# # site_colors <- safe_pal
# # site_COLOR_scale  <- scale_color_manual(name = "Site", values = site_colors,
# #                                          breaks = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'),
# #                                          labels = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'))
# #
# # # s_i=1
# # for (s_i in 1:length(sites)) {
# # 
# # aa = ggplot(FLKs1)+
# #   geom_sf(fill = "black", lwd = 0)+
# #   geom_sf(data=subset(plot_a,SiteID==sites[s_i]),aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.05)+
# #   # geom_sf(data=subset(plot_a,simu=='backward'),aes(color=Latitude,fill=Latitude),inherit.aes = FALSE,alpha=0.05)+
# #   # geom_sf(data=subset(plot_a,simu=='backward'),aes(color=visitID,fill=visitID),inherit.aes = FALSE,alpha=0.05)+
# #   # geom_sf(data=subset(plot_a,simu=='backward'),aes(color=Sub_region,fill=Sub_region),inherit.aes = FALSE,alpha=0.05)+
# #   site_COLOR_scale+
# #   # scale_color_gradientn(colours=pnw)+
# #   # scale_fill_gradientn(colours=pnw)+
# #   # scale_color_brewer(palette='Dark2')+
# #   # scale_fill_brewer(palette='Dark2')+
# #   geom_point(aes(x=Longitude,y=Latitude),color="blue",data=plot_a)+
# #   geom_point(aes(x=Longitude,y=Latitude),color="cyan",data=subset(plot_a,SiteID==sites[s_i]))+
# #   ylab('Latitude')+
# #   xlab('Longitude')+
# #   scale_x_continuous(limits=st_bbox(plotData_t)[c(1,3)])+
# #   scale_y_continuous(limits=st_bbox(plotData_t)[c(2,4)])+
# #   # ggtitle(paste0(plot_a$MY," backward trajectories"))+
# #   ggtitle("2019-07 backward trajectories")+
# #   theme_bw()
# # # aa
# # 
# # plot_b = subset(plotData_t,n_days %in% c(1,7,14) & MY=="2019-09")
# # # plot_b = subset(plotData_t,MY=="2019-09")
# # # plot_b = subset(plotData_t,n_days<8 & MY=="2019-09")
# # length(unique(plot_b$SiteID)) 
# # bb = ggplot(FLKs1)+
# #   geom_sf(fill = "black", lwd = 0)+
# #   geom_sf(data=subset(plot_b,SiteID==sites[s_i]),aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.05)+
# #   # geom_sf(data=subset(plot_b,simu=='backward'),aes(color=Latitude,fill=Latitude),inherit.aes = FALSE,alpha=0.05)+
# #   # geom_sf(data=subset(plot_b,simu=='backward'),aes(color=visitID,fill=visitID),inherit.aes = FALSE,alpha=0.05)+
# #   
# #   # scale_color_gradientn(colours=pnw)+
# #   # scale_fill_gradientn(colours=pnw)+
# #   # geom_sf(data=subset(plot_b,simu=='backward'),aes(color=Sub_region,fill=Sub_region),inherit.aes = FALSE,alpha=0.05)+    scale_color_brewer(palette='Dark2')+
# #   # scale_color_brewer(palette='Dark2')+
# #   # scale_fill_brewer(palette='Dark2')+
# #   geom_point(aes(x=Longitude,y=Latitude),color="blue",data=plot_b)+
# #   geom_point(aes(x=Longitude,y=Latitude),color="cyan",data=subset(plot_b,SiteID==sites[s_i]))+
# #   ylab('Latitude')+
# #   xlab('Longitude')+
# #   scale_x_continuous(limits=st_bbox(plotData_t)[c(1,3)])+
# #   scale_y_continuous(limits=st_bbox(plotData_t)[c(2,4)])+
# #   # ggtitle(paste0(timeWindow," backward trajectories, n=7days"))+
# #   # ggtitle(paste0(MY," backward trajectories"))+
# #   ggtitle("2019-09 backward trajectories")+
# #   theme_bw()
# # # bb
# # 
# # 
# # ab = plot_grid(aa,bb,
# #                labels=c("A","B"))
# # 
# # TS=Sys.time()
# # TS
# # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Test_Figs/inshoreSites_",sites[s_i],"_",TS,".png"),plot=ab,width=10, height=10, dpi = 300)
# # }
# 
# 
