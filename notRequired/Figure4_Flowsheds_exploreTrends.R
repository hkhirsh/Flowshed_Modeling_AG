#MOST FINAL FIG 4 CODE - 2024nov27


# register_google(key = "AIzaSyBSmQdRPOV8usMAOEkkoH6MISzFjTe5AoE") #new api Feb 2024



## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('PNWColors','sf','ggplot2','ggmap','mapview','cowplot','ggstar')

lapply(packageload, library, character.only = TRUE)


rm(list=ls())
library(PNWColors)
library(colorspace)
library(RColorBrewer)
library(ghibli)
library(htmlwidgets)
library(sf)
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


# # ----Check which packages we actually use----
# # Find which packages do used functions belong to ----
# used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/Flowshed_Modeling/Manuscript_Figures_2024/Figure4_Flowsheds.R", alphabetic = FALSE) |> print()
# # Find which loaded packages are not used ----
# used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
# unused.packages <- packageload[!(packageload %in% used.packages)] |> print()
# #--------------------------------------------

# 
# #Read in yearBows (if not already loaded from running the section above)
# shp.list = list.files(path = paste0("/Users/heidi.k.hirsh/Documents/FLK_Model1/yearBows_22april2024"), pattern="*shp", full.names = T)
# # length(shp.list)
# # shp.list
# shapefile_list = lapply(shp.list,read_sf)
# 
# all.yearBows = rbind(shapefile_list[[1]],shapefile_list[[2]],shapefile_list[[3]],shapefile_list[[4]])
# yearBows=all.yearBows

## Read in carbonate chemistry station data
CC = read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/CCflk_plusBathy.csv',na.strings=c(""," "))

#subset the carb chem data to represent the years we want and a subset of SiteIDs
CCyr = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')) #subset of chemistry samples that match bow ties:
## assign visitID columns to allow us to pair CC to the spatial polygon data:
CCyr$visitID_ch1 =  paste(CCyr$SiteID,CCyr$UTCDate_Time)
CCyr$visitID_ch2 =    gsub(" ", "_" , CCyr$visitID_ch1, perl=TRUE)
CCyr$visitID =  gsub("[: -]", "" , CCyr$visitID_ch2, perl=TRUE)

plotCCyr = st_as_sf(CCyr, coords = c("Longitude","Latitude"), crs = st_crs(4326))
mapview(plotCCyr,zcol="SiteID")
plotIN = subset(plotCCyr,Zone=='Inshore')
unique(plotIN$Zone)
mapview(plotIN,zcol="SiteID")

# #How can I get a general point for each
# names(plotCCyr)
# mapview(plotCCyr,zcol="SiteID")
# 
# 
# #Simplify CCyr to just the columns required for plotting: 
CCyr.s = CCyr[c('visitID','SiteID','Latitude', 'Longitude','dec.lat','dec.lon','Year','Sub_region','Zone','Season','Month','MY','MoY','pointDepth','Precipitation')]
# names(CCyr)
# 
# CCbowsALL=NULL
# 
# CCbowsALL = left_join(yearBows, CCyr.s, by="visitID")
# # class(CCbowsALL)  #"sf"         "data.frame"
# # dim(CCbowsALL)  #38640    73
# 
# ###__________save ALL backward bows for every year_________
# CCbows=NULL
# CCbows=subset(CCbowsALL, simu=='backward')
# unique(CCbows$SiteID)
# # CCbows$SiteID = factor(CCbows$SiteID, levels = c("1","EK_IN","4","5","UK_IN","7","10","13","16","19","24"))
# 
# concave=CCbows
# concave_utm17 = st_transform(concave, 26917)
# 
# table(concave$Year,concave$SiteID)
# 
D=300
# 
# concave.bo = st_buffer(concave_utm17, dist=D, nQuadSegs=30)
# dim(concave.bo)
# # save(concave.bo, file = "/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/AllBackward_concave_bo.RData")
load("/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/AllBackward_concave_bo.RData")

# concave.bi = st_buffer(concave.bo, dist=-D, nQuadSegs=30)
# dim(concave.bi)
# save(concave.bi, file = "/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/AllBackward_concave_bi.RData")
# load("/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/inshoreAllYrs_concave_bi.RData")


# ###__________save inshore backward bows for every year_________
# CCbows=NULL
# CCbows=subset(CCbowsALL, Zone=='Inshore' & simu=='backward')
# unique(CCbows$SiteID)
# CCbows$SiteID = factor(CCbows$SiteID, levels = c("1","EK_IN","4","5","UK_IN","7","10","13","16","19","24"))
# 
# concave=CCbows
# concave_utm17 = st_transform(concave, 26917)
# 
# table(concave$Year,concave$SiteID)
# 
# D=300
# 
# concave.bo = st_buffer(concave_utm17, dist=D, nQuadSegs=30)
# dim(concave.bo)
# save(concave.bo, file = "/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/inshoreAllYrs_concave_bo.RData")
# 
# concave.bi = st_buffer(concave.bo, dist=-D, nQuadSegs=30)
# dim(concave.bi)
# save(concave.bi, file = "/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/inshoreAllYrs_concave_bi.RData")
# # load("/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/inshore2019_concave_bi.RData")



### Skip previous steps and just load concave polygons (post buffer in)
load("/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/inshoreAllYrs_concave_bi.RData")
plotData = concave.bi
# dim(plotData)
plotData_t = st_transform(plotData,crs='+proj=longlat +datum=WGS84')
unique(plotData$SiteID)
plotData_t$Season = factor(plotData$Season, levels = c("Winter","Spring","Summer","Fall"))
levels(plotData_t$Season)

head(plotData_t)
# ###____________________________________________________________
# 
# CCbows=NULL  
# CCbows=subset(CCbowsALL, Zone=='Inshore' & simu=='backward' & Year==2019)
# # CCbowsM=subset(CCbowsALL, Zone=='Mid channel' & simu=='backward' & Year==2019)
# # #try other zones
# # CCbows=subset(CCbowsALL, simu=='backward' & Year==2019) #A and B coloring will be screwed up by this (too many clases)
# 
# ##uncomment below if only using INshore sites.
# unique(CCbows$SiteID)
# CCbows$SiteID = factor(CCbows$SiteID, levels = c("1","EK_IN","4","5","UK_IN","7","10","13","16","19","24"))
# 
# # unique(CCbowsM$SiteID)
# # CCbowsM$SiteID = factor(CCbowsM$SiteID, levels = c("2","EK_MID","5.5","UK_MID","8","11","14","17","20","23"))
# 
# #run for mid channel
# # CCbows=CCbowsM 
# #also save all unique sites for plotting
# 
# concave=CCbows
# concave_utm17 = st_transform(concave, 26917)
# 
# #buffer (extend) out and in
# # D=1/111.111*.5 #degrees per km
# D=300
# concave.bo = st_buffer(concave_utm17, dist=D, nQuadSegs=30)   
# dim(concave.bo)
# # save(concave.bo, file = "/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/inshore2019_concave_bo.RData")
# 
# concave.bi = st_buffer(concave.bo, dist=-D, nQuadSegs=30)
# dim(concave.bi)
# # save(concave.bi, file = "/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/inshore2019_concave_bi.RData")
# 
# plotData = concave.bi
# dim(plotData)
# 
# dim(plotData)
# plotData_t = st_transform(plotData,crs='+proj=longlat +datum=WGS84')
# ###____________________________________________________________


################## Define appropriate benthic habitat
FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')

nColor <- length(unique(plotIN$SiteID))
# nColor

#!!Replace greens with standalone visible color!!
# clrs0 = rainbow(nColor)
# scales::show_col(clrs0)
clrs = rainbow(nColor, start = 0, end =0.75)
# scales::show_col(clrs)

#SIMPLE
site_colors <- clrs
site_COLOR_scale  <- scale_color_manual(name = "Site", values = site_colors,
                                        breaks = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'),
                                        labels = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'))

site_FILL_scale  <- scale_fill_manual(name = "Site", values = site_colors,
                                      breaks = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'),
                                      labels = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'))


pnw=pnw_palette(name="Bay",n=14,type="continuous")
# pnw

## include shelf points
PTS = read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/CC_dataframes/PTS.csv')
PTS$Lat=PTS$Latitude_Dec_Deg
PTS$Long=PTS$Longitude_Dec_Deg
PTS=st_as_sf(PTS, coords = c("Long","Lat"),crs = st_crs(4326)) 
iwant = c("SR","MC") #limit to shark river and middle cape
PTSsub= subset(PTS,line_id %in% iwant)
PTSsub= subset(PTSsub,Latitude_Dec_Deg>25) #remove 2 points in the keys
PTSsub5=subset(PTSsub,bathymetry>5) #only use points where depth is greater than 5m
plotShelfa = subset(PTSsub5,Year_UTC==2018) #don't need duplicate points, and 2018 has all 6
unique(plotShelfa$Year_UTC)
plotShelf = subset(plotShelfa,Month_UTC==5)
mapview(plotShelf)


# unique(plotData_t$simu) #only backward? 
# 
# unique(plotData$MY)
# length(unique(plotData$MY)) #44 different month/year pairs
# table(plotData$Year,plotData$MoY)
# 
# unique(plotData$visitID)
# length(unique(plotData$visitID)) #391 unique inshore samples
# 
# test=subset(plotData_t,n_days==7 & Year==2018 & Season=="Winter")
# unique(test$Year)
# unique(test$n_days)
# dim(test)

# ploThis = subset(plotData_t,n_days==7 & Year==2018& Season=="Winter")
# 
# unique(ploThis$SiteID)
# #4 panel: 
# 
# AA = ggplot(FLKs1)+
#   geom_sf(fill = "darkgray", lwd = 0)+
#   geom_sf(data=ploThis,aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.05)+
#   site_COLOR_scale+
#   site_FILL_scale+
#   geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+
#   geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+
#   geom_point(aes(x=dec.lon,y=dec.lat,fill=SiteID),color='black',data=ploThis,pch=21,size=1.5,stroke=1)+ #use shared point location for that site
#   # geom_point(aes(x=Longitude,y=Latitude,fill=SiteID),color='black',data=ploThis,pch=21,size=1.5,stroke=1)+
#   # geom_star(aes(x=Longitude,y=Latitude,fill=SiteID),color="black",data=plotData_t,size=3)+
#   ylab('Latitude')+
#   xlab('Longitude')+
#   # coord_sf(xlim = c(-82.1, -79.95), ylim = c(24.4, 26.2), expand = FALSE) + #for backward halos (tight)
#   # coord_sf(xlim = c(-83.7, -80), ylim = c(23.55, 26), expand = FALSE) + #for backward halos for halozone
#   coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
#   
#   # coord_sf(xlim = c(-82.055, -80.092), ylim = c(24.49, 25.73), expand = FALSE) + #for backward halos
#   # coord_sf(xlim = c(-82.05575, -80.09157), ylim = c(24.49353, 25.72), expand = FALSE) + #for backward halos
#   # coord_sf(xlim = c(-82.3, -80), ylim = c(24.4, 26.2), expand = FALSE) + #for backward halos
#   # ggtitle("July 2019")+
#   theme_bw()+
#   # theme(axis.line = element_line(color='black'),
#   #       panel.grid.major = element_blank(),
#   #       panel.grid.minor = element_blank(),
#   #       axis.title.x = element_blank())+
#   theme(axis.line = element_line(color='black'),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())#+
#   # theme(plot.margin = unit(c(0, 0, 0, 5), "cm"))+
#   # theme(legend.position="none")
# AA
# 
# #no need to worry about insets for these plots (more exploratory)


## Ok. now I have a spatial dataframe with the flowshed polygons associated with every inshore station, every time/year
#I can loop through to plot different combinations to explore spatial and temporal variability. 

#also try to label each plot (possible to do each point?) with the number of samples represented 

# plotData_t$Season = factor(plotData$Season, levels = c("Winter","Spring","Summer","Fall"))
sites=unique(plotData_t$SiteID)
length(sites)
plotData_t$Sub_region = factor(plotData$Sub_region, levels = c("BB","UK","MK","LK"))


Yrs = unique(plotData_t$Year)
Yrs
Seas= levels(plotData_t$Season)
Seas
# Dur=unique(plotData_t$n_days)
# Dur

# y_i = 1
# s_i = 1
# d_i = 1

y_i = NULL
s_i = NULL
d_i = NULL
ploThis = NULL
ploThis1 = NULL
ploThis2 = NULL

nn=5

for (y_i in 1:length(Yrs)) {
  timeYear=Yrs[y_i]
  ploThis1 = subset(plotData_t,Year==timeYear)

  for (s_i in 1:length(Seas)) {
    timeSeason=Seas[s_i]
    ploThis2 = subset(ploThis1,Season==timeSeason)

ploThis= ploThis2    

if(nrow(ploThis)>0) {
    
AA = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+                                            #Florida shoreline
  
  # geom_sf(data=subset(ploThis,n_days==nn),aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.1,lwd=.6)+
  # # geom_sf(data=subset(ploThis,n_days==nn),aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.1,size=1.4)+
  # site_COLOR_scale+site_FILL_scale+                                               #Color points by unique site ID
  
  #color by region instead
  geom_sf(data=subset(ploThis,n_days==nn),aes(color=Sub_region,fill=Sub_region),inherit.aes = FALSE,alpha=0.1,lwd=.6)+
  # geom_sf(data=subset(ploThis,n_days==nn),aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.1,size=1.4)+
  # site_COLOR_scale+site_FILL_scale+                                               #Color points by unique site ID
  
  
  geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.7)+         #Plot all stations
  geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.7)+  #add West Florida Shelf points
  geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat,fill=Sub_region),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
  # geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat,fill=SiteID),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
  
  ylab('Latitude')+ xlab('Longitude')+
  # coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
  coord_sf(xlim = c(-82.5, -80), ylim = c(24.3, 26), expand = FALSE) + #for backward halos
  
  # ggtitle("July 2019")+
  ggtitle(paste0(timeYear," ",timeSeason," (",nn,"-day)"))+
  # ggtitle(paste0(timeYear," ",timeSeason," backward trajectories (",nn,"-day)"))+
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())#+
# theme(plot.margin = unit(c(0, 0, 0, 5), "cm"))+
# theme(legend.position="none")
# AA
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/Inshore_",nn,"d_byRegion_v2/",timeYear,"_",timeSeason,"_Inshore",nn,"day.png"),plot=AA,width=10, height=10, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/Inshore_",nn,"d_v2/",timeYear,"_",timeSeason,"_Inshore",nn,"day.png"),plot=AA,width=10, height=10, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/Inshore_",nn,"d/",timeYear,"_",timeSeason,"_Inshore",nn,"day.png"),plot=AA,width=10, height=10, dpi = 300)



} #end part to run if there is data for raster
else {
} #end alternative (don't plot)

  }
}

print("done")


#year season gif
list.files(path='/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/Inshore_5d_v2', pattern = '*.png', full.names = TRUE) %>%
  mixedsort() %>%
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/Inshore_Year_Season_v2.gif")

list.files(path='/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/Inshore_5d_byRegion_v2', pattern = '*.png', full.names = TRUE) %>%
  mixedsort() %>%
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/Inshore_Year_Season_byRegion.gif")


# ###_____________________ Plot 1, 7, 14 days together now (like manuscript)
# y_i = NULL
# s_i = NULL
# d_i = NULL
# ploThis = NULL
# ploThis1 = NULL
# ploThis2 = NULL
# 
# for (y_i in 1:length(Yrs)) {
#   timeYear=Yrs[y_i]
#   ploThis1 = subset(plotData_t,Year==timeYear)
#   
#   for (s_i in 1:length(Seas)) {
#     timeSeason=Seas[s_i]
#     ploThis2 = subset(ploThis1,Season==timeSeason)
#     
#     ploThis= ploThis2    
#     
#     AA = ggplot(FLKs1)+
#       geom_sf(fill = "darkgray", lwd = 0)+                                            #Florida shoreline
#       geom_sf(data=subset(ploThis,n_days%in% c(1,7,14)),aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.075)+
#       site_COLOR_scale+site_FILL_scale+                                               #Color points by unique site ID
#       geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+         #Plot all stations
#       geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+  #add West Florida Shelf points
#       geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat,fill=SiteID),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
#       ylab('Latitude')+ xlab('Longitude')+
#       coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
#       ggtitle("July 2019")+
#       ggtitle(paste0(timeYear," ",timeSeason," backward trajectories (1,7,14-day)"))+
#       theme_bw()+
#       theme(axis.line = element_line(color='black'),
#             panel.grid.major = element_blank(),
#             panel.grid.minor = element_blank())#+
#     # theme(plot.margin = unit(c(0, 0, 0, 5), "cm"))+
#     # theme(legend.position="none")
#     AA
#     ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/Inshore_1714d2/",timeYear,"_",timeSeason,"_Inshore_1714day.png"),plot=AA,width=10, height=10, dpi = 300)
#   }
# }
# ###_____________________ 

###_____________________ Explore annual variability (by season)
# plotData_t$Season = factor(plotData_t$Season, levels = c("Winter","Spring","Summer","Fall"))
#Season needs to be in every dataframe to facet by season for ploThis: 
# FLKs1$Season <- NA
# # CCyr.s$Season <- NA #this one has season for real
# plotShelf$Season <- NA


y_i=NULL
ploThis = NULL
ploThis1 = NULL
ploThis2 = NULL

y_i=5
nn=5

for (y_i in 1:length(Yrs)) {
  timeYear=Yrs[y_i]
  timeYear
  ploThis1 = subset(plotData_t, Year==timeYear)
  # ploThis1 = subset(plotData_t,n_days==nn & Year==timeYear)
  # dim(ploThis1)
  # levels(ploThis1$Season)

  ploThis = subset(ploThis1,n_days==nn)
  # levels(ploThis$Season)
  # dim(ploThis)
  
  # ploThis$Season=factor(ploThis$Season,levels = c("Winter","Spring","Summer","Fall"))
  # ploThis$Season=factor(ploThis$Season, levels = c(Winter,Spring,Summer,Fall))
  # ploThis$Season <- factor(ploThis$Season, levels = c("Winter", "Spring", "Summer", "Fall"))
  # levels(ploThis$Season) 
  # levels(ploThis1$Season)
  # ploThis <- ploThis1 %>%
  #   mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))
  # levels(ploThis$Season) 
  
  # ploThis=PloThis1
  
  BB = ggplot()+
    geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+                                           #Florida shoreline
    geom_sf(data=ploThis,aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.075)+
    site_COLOR_scale+site_FILL_scale+                                                         #Color points by unique site ID
    ##Leave off other points if I want to facet seasons in correct order
    # geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+                 #Plot all stations
    # geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+  #add West Florida Shelf points
    geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat,fill=SiteID),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
    # facet_wrap(~Season,nrow=2)+
    # facet_wrap(~MY,nrow=4)+
    facet_wrap(~Season,nrow=4)+
    # facet_grid(Season~Sub_region)+
    ylab('Latitude')+ xlab('Longitude')+
    coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
    ggtitle("July 2019")+
    ggtitle(paste0(timeYear," backward trajectories (",nn,"-day)"))+
    theme_bw()+
    theme(axis.line = element_line(color='black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())#+
  # theme(plot.margin = unit(c(0, 0, 0, 5), "cm"))+
  # theme(legend.position="none")
  # BB
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/SeasonFacet_7d_inshore2/",timeYear,"_Inshore",nn,"day_season.png"),plot=BB,width=10, height=10, dpi = 300)
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/SeasonFacet_7d_inshore2/",timeYear,"_Inshore",nn,"day_season.png"),plot=BB,width=10, height=10, dpi = 300)
  
  }


#####__________Try faceting by both year and season (no loop needed)

ploThis = NULL
nn=5

# # ploThis = subset(plotData_t,n_days==nn)
# ploThisA = subset(plotData_t,n_days==nn & Year %in% c(2012,2014,2015))
# ploThisB = subset(plotData_t,n_days==nn & Year %in% c(2016,2017,2018))
# ploThisC = subset(plotData_t,n_days==nn & Year %in% c(2019,2020,2021))
# 
# plotSubs = c(ploThisA,ploThisB,ploThisC)
# plotSubs
# test = plotSubs[1]
  # unique(ploThis$Season)
  # unique(ploThis$Year)
  # dim(ploThis)
  
ploThis = subset(plotData_t,n_days==nn & Year %in% c(2012,2014,2015))
unique(ploThis$Year)

aa = ggplot(ploThis)+
    geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+                                           #Florida shoreline
    geom_sf(data=ploThis,aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.075)+
    site_COLOR_scale+site_FILL_scale+                                                         #Color points by unique site ID
    geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat,fill=SiteID),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
    facet_grid(Season~Year)+
    ylab('Latitude')+ xlab('Longitude')+
    coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
    theme_bw()+
    theme(axis.line = element_line(color='black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
aa
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/FacetSeasonYear/Inshore",nn,"day_SeasonYrFacet_allYears_1of3.png"),plot=aa,width=10, height=10, dpi = 300)

#makes more sense to facet 3 years at a time. 
ploThis=NULL
ploThis = subset(plotData_t,n_days==nn & Year %in% c(2016,2017,2018))
unique(ploThis$Year)
bb = ggplot()+
  geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+                                           #Florida shoreline
  geom_sf(data=ploThis,aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.075)+
  site_COLOR_scale+site_FILL_scale+                                                         #Color points by unique site ID
  geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat,fill=SiteID),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
  # facet_wrap(~Season,nrow=4)+
  facet_grid(Season~Year)+
  ylab('Latitude')+ xlab('Longitude')+
  coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/FacetSeasonYear/Inshore",nn,"day_SeasonYrFacet_allYears_2of3.png"),plot=bb,width=10, height=10, dpi = 300)


ploThis=NULL
ploThis = subset(plotData_t,n_days==nn & Year %in% c(2019,2020,2021))
unique(ploThis$Year)
cc = ggplot()+
  geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+                                           #Florida shoreline
  geom_sf(data=ploThis,aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.075)+
  site_COLOR_scale+site_FILL_scale+                                                         #Color points by unique site ID
  geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat,fill=SiteID),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
  # facet_wrap(~Season,nrow=4)+
  facet_grid(Season~Year)+
  ylab('Latitude')+ xlab('Longitude')+
  coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/FacetSeasonYear/Inshore",nn,"day_SeasonYrFacet_allYears_3of3.png"),plot=cc,width=10, height=10, dpi = 300)



####_______ok now one season at a time. all years. 
s_i=NULL
ploThis = NULL
ploThis1 = NULL
ploThis2 = NULL

nn=7
for (s_i in 1:length(Seas)) {
    timeSeason=Seas[s_i]
    ploThis1 = subset(plotData_t,Season==timeSeason)
    
  ploThis = subset(ploThis1,n_days==nn)
  
  BB = ggplot()+
    geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)+                                           #Florida shoreline
    geom_sf(data=ploThis,aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.075)+
    site_COLOR_scale+site_FILL_scale+                                                         #Color points by unique site ID
    geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+                 #Plot all stations
    geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+  #add West Florida Shelf points
    geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat,fill=SiteID),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
    facet_wrap(~Year,nrow=4)+
    ylab('Latitude')+ xlab('Longitude')+
    coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
    ggtitle("July 2019")+
    ggtitle(paste0(timeSeason," backward trajectories (",nn,"-day)"))+
    theme_bw()+
    theme(axis.line = element_line(color='black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  BB
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/FacetSeasonYear/Inshore7day_",timeSeason,"_allYears.png"),plot=BB,width=10, height=10, dpi = 300)
}

## year color scale
nYears=length(unique(plotData_t$Year))
nYears
# pnw=pnw_palette(name="Bay",n=nyears,type="discrete")
# pnw
Yclrs = rainbow(nYears, start = 0, end =0.75)
scales::show_col(Yclrs)

# pnw_palette(name="Starfish",n=7,type="discrete")

# library('viridis')
# colors<- viridis(nYears, option = "viridis")
# scales::show_col(colors)


colors <- hcl.colors(9, palette = "Set2") 
scales::show_col(colors)

# library(RColorBrewer)
# colors <- brewer.pal(nYears, "Set1")  # Or use "Dark2", "Paired", etc.
# scales::show_col(colors)
# 
# colors <- brewer.pal(nYears, "Paired")  # Or use "Dark2", "Paired", etc.
# scales::show_col(colors)


#SIMPLE
year_colors <- colors
year_COLOR_scale  <- scale_color_manual(name = "Year", values = year_colors,
                                        breaks = c('2012','2014','2015','2016','2017','2018','2019','2020','2021'),
                                        labels = c('2012','2014','2015','2016','2017','2018','2019','2020','2021'))

year_FILL_scale  <- scale_fill_manual(name = "Year", values = year_colors,
                                      breaks = c('2012','2014','2015','2016','2017','2018','2019','2020','2021'),
                                      labels = c('2012','2014','2015','2016','2017','2018','2019','2020','2021'))




#### Loop through seasons, color by Year. 
for (s_i in 1:length(Seas)) {
  timeSeason=Seas[s_i]
  ploThis1 = subset(plotData_t,Season==timeSeason)
  
  ploThis = subset(ploThis1,n_days==nn)
  unique(ploThis$Year)
  
  BB = ggplot()+
    geom_sf(data=FLKs1,fill = "gray17", lwd = 0)+                                           #Florida shoreline
    geom_sf(data=ploThis,aes(color=as.factor(Year)),inherit.aes = FALSE,alpha=0)+
    year_COLOR_scale+
    # year_FILL_scale+                                                         #Color points by unique site ID
    geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+                 #Plot all stations
    geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+  #add West Florida Shelf points
    geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat,fill=SiteID),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
    # facet_wrap(~Year,nrow=4)+
    ylab('Latitude')+ xlab('Longitude')+
    # coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
    coord_sf(xlim = c(-82.7, -80), ylim = c(24.35, 26), expand = FALSE) + #for backward halos
    ggtitle(paste0(timeSeason," backward trajectories (",nn,"-day)"))+
    theme_bw()+
    theme(axis.line = element_line(color='black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  BB
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/FacetSeasonYear/Inshore7day_",timeSeason,"_YearColor3.png"),plot=BB,width=10, height=10, dpi = 300)
}

Reg = unique(plotData_t$Sub_region)
nn=7
#### Loop through seasons, color by Year. 
for (r_i in 1:length(unique(plotData_t$Sub_region))) {
    ploThis1 = subset(plotData_t,Sub_region==Reg[r_i])
    
    ploThis = subset(ploThis1,n_days==nn)
    unique(ploThis$Year)
    
  
  BB = ggplot()+
    geom_sf(data=FLKs1,fill = "gray17", lwd = 0)+                                           #Florida shoreline
    geom_sf(data=ploThis,aes(color=as.factor(Year)),inherit.aes = FALSE,alpha=0)+
    year_COLOR_scale+
    # year_FILL_scale+                                                         #Color points by unique site ID
    # geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+                 #Plot all stations
    # geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+  #add West Florida Shelf points
    geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat,fill=SiteID),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
    site_FILL_scale+
    facet_wrap(~Season,nrow=2)+
    ylab('Latitude')+ xlab('Longitude')+
    # coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
    coord_sf(xlim = c(-82.7, -80), ylim = c(24.35, 26), expand = FALSE) + #for backward halos
    ggtitle(paste0(Reg[r_i]," backward trajectories (",nn,"-day)"))+
    theme_bw()+
    theme(axis.line = element_line(color='black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  BB
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/FacetSeasonYear/Inshore",nn,"day_facetSeason_YearColor_Region=",Reg[r_i],".png"),plot=BB,width=10, height=10, dpi = 300)

}

