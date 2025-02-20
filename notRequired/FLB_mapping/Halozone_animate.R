## Clean up this script to plot/review watermass history halozone transect planning
#See 'Bowties_Halozone.R' for notes about duplicates

#target transects: (by site ID)
#Emerald Reef = 2
#Grecian Rocks = 4
#Sombrero Reef = 13
#Eastern Dry Rocks = 24

#Other transects of interest: 
#UK_IN (north of Cheeca)
#7 (south of Cheeca)
#10 (next south of 7)


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
register_google(key = "AIzaSyBSmQdRPOV8usMAOEkkoH6MISzFjTe5AoE") #new api Feb 2024
FLK_map=get_map(location=c(-80.99306,25.27732),zoom=8,maptype = "satellite")


#Read in yearBows (if not already loaded from running the section above)
shp.list = list.files(path = paste0("/Users/heidi.k.hirsh/Documents/FLK_Model1/yearBows_22april2024"), pattern="*shp", full.names = T) 
length(shp.list)
shp.list
shapefile_list = lapply(shp.list,read_sf)

all.yearBows = rbind(shapefile_list[[1]],shapefile_list[[2]],shapefile_list[[3]],shapefile_list[[4]])
yearBows=all.yearBows

#Read in carbonate chemistry station data
CC = read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/CCflk_plusBathy.csv',na.strings=c(""," "))



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
# class(CCyr)
# class(plotCCyr)


CCbows=NULL
CCbowsALL=NULL

CCbowsALL = left_join(yearBows, CCyr, by="visitID") 
class(CCbowsALL)  #"sf"         "data.frame"
dim(CCbowsALL)  #38640    73
# 38640/14/2 #1380 (4 extra samples)
# CCbows1=na.omit(CCbows0) #no extra

table(CCbowsALL$n_days,CCbowsALL$Season)
table(CCbowsALL$n_days,CCbowsALL$Year)
head(CCbowsALL)
names(CCbowsALL)

# mapview(CCbowsALL[1,])
# CCbowsALL[1,]

#try long key site and cheeca 
#subset bows here to get specific ones to plot
# CCbows=subset(CCbowsALL, SiteID %in% c('7','UK_IN')) #Long Key and Islamorada
CCbows=subset(CCbowsALL, SiteID %in% c('2','4','13','24'))
# CCbows=subset(CCbowsALL, SiteID %in% c('7','UK_IN')) 
dim(CCbows) 
# table(CCbows$visitID)
# table(table(CCbows$visitID))





unique(CCbows$MY)
unique(CCbows$Year)
unique(CCbows$Month) #reorder months correctly
#loop through year/month here
#do this for plotting (make all 'CCbows' concave)



concave=CCbows
dim(concave)
concave_utm17 = st_transform(concave, 26917)
dim(concave_utm17)



#save concave halozone bows

#buffer (extend) out and in
# D=1/111.111*.5 #degrees per km
D=300
concave.bo = st_buffer(concave_utm17, dist=D, nQuadSegs=30)  
dim(concave.bo)


save(concave.bo, file = "/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/Halozone_concave_bo.RData")
# load("concave_bo.RData")

concave.bi = st_buffer(concave.bo, dist=-D, nQuadSegs=30)
dim(concave.bi)
unique(concave.bi$SiteID)
save(concave.bi, file = "/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/Halozone_concave_bi.RData")

# plotData=subset(concave.bi,SiteID=='7')
plotData = concave.bi
#transform to using bounding box
plotData_t = st_transform(plotData,crs='+proj=longlat +datum=WGS84')
unique(plotData$SiteID)
unique(plotData$MY)

MoYr = unique(plotData$MY)
# MoYr = unique(concave.bi$MY)
# 
# # my_i=1
# # for (my_i in 1:length(MoYr)) {
#     
#     
#   timeWindow=MoYr[my_i]
#   ploThis = subset(plotData,MY==timeWindow)
#   # ploThis = subset(plotData,MY==timeWindow & n_days<8)
#   # ploThis = subset(concave.bi,MY==timeWindow & n_days<8 & SiteID=='7')
#   
#   # View(ploThis)
#   ploThis_t = st_transform(ploThis,crs='+proj=longlat +datum=WGS84')
#   dim(ploThis_t)
#   
#   # clrs = rainbow(length(unique(ploThis$n_days)), start = 0, end =0.8)
#   clrs = divergingx_hcl(n=length(unique(ploThis$n_days)), palette="Zissou 1")
#   # clrs = diverging_hcl(n=length(unique(ploThis$n_days)), palette="Cyan-Magenta")
#   # clrs = diverging_hcl(n=length(unique(ploThis$n_days)), palette="Blue-Red 3")
#   # clrs_rev <- rev(clrs)
#   clrs_rev <- clrs
#   d=ploThis_t$n_days
#   pal = colorNumeric(palette = clrs_rev, domain = min(d):max(d))
#   
#   # pnw=pnw_palette(name="Bay",n=14,type="continuous")
# 
#   H=leaflet(data=ploThis_t) %>% 
#   addProviderTiles('Esri.WorldImagery') %>%
#   addPolygons(data=subset(ploThis_t,simu=='backward'),fillOpacity=0.01,weight=1,color = ~pal(d))  %>%
#   # addPolygons(data=subset(ploThis_t,simu=='forward'),fillOpacity=0.05,weight=1,color = ~pal(d))  %>%
#     # addPolygons(data=subset(ploThis_t,simu=='backward' & ndays==6),color='magenta',fillOpacity=.1,weight=1)  #%>%
#     # addLegend()ata=subset(ploThis_t,simu=='backward'),labels=plotThis_t$n_days,colors=~pal(d))  %>% 
#   addCircleMarkers(lng = ~ dec.lon,~ dec.lat,radius=1,color='white')
#   H
#   # saveWidget(H, file =paste0("/Users/heidi.k.hirsh/Desktop/CheecaLong/14day_back_justLongZ/",timeWindow,"_LongKeyInshore14b.html"))
#   
# }




#set up to loop through this. Label with Month Year. 
FLK_mapGG=get_map(location=c(-80.99306,25.27732),zoom=7,maptype = "satellite")
FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')
# FLKs2=st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Florida_Shoreline_(1_to_40%2C000_Scale)/Florida_Shoreline_(1_to_40%2C000_Scale).shp')
pnw=pnw_palette(name="Bay",n=14,type="continuous")

my_i=NULL
# my_i
dim(plotData)
# my_i=1
for (my_i in 1:length(MoYr)) {
  
  timeWindow=MoYr[my_i]
  
  #can also try looping through seasons (or Season & year)
  
  ploThis = subset(plotData,MY==timeWindow)
  ploThis_t = st_transform(ploThis,crs='+proj=longlat +datum=WGS84')
  # dim(ploThis_t)
  
# GG = ggmap(FLK_mapGG)+
  GG = ggplot(FLKs1)+
  geom_sf(fill = "black", lwd = 0)+
  # coord_sf(crs = st_crs(4326)) +
  # geom_sf(data=subset(ploThis_t,simu=='forward'),aes(color=n_days,fill=n_days),inherit.aes = FALSE,alpha=0)+
    geom_sf(data=subset(ploThis_t,simu=='backward'),aes(color=n_days,fill=n_days),inherit.aes = FALSE,alpha=0)+
  scale_color_gradientn(colours=pnw)+
  scale_fill_gradientn(colours=pnw)+
  geom_point(aes(x=Longitude,y=Latitude),color="blue",data=ploThis_t)+
  # scale_color_continuous_divergingx(palette="ArmyRose")+
  # scale_color_gradientn(colours=divergingx_hcl(n=length(unique(ploThis$n_days)), palette="Zissou 1"))))+
  # ggplot2::facet_wrap(vars(category))
  ylab('Latitude')+
  xlab('Longitude')+
  # coord_sf(xlim = c(-82.2, -79.8), ylim = c(24.4, 25.8), expand = FALSE) + 
  coord_sf(xlim = c(-83.7, -80), ylim = c(23.55, 26), expand = FALSE) + #for backward halos
  # coord_sf(xlim = c(-82.45, -79.6), ylim = c(24.25, 27.7), expand = FALSE) + #for forward halos
    
  # coord_sf(xlim = c(-81.5, -80), ylim = c(24.6, 25.8), expand = FALSE) +
  # scale_x_continuous(limits=st_bbox(ploThis_t)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(ploThis_t)[c(2,4)])+
  # scale_x_continuous(limits=st_bbox(plotData_t)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(plotData_t)[c(2,4)])+
  ggtitle(paste0(timeWindow," backward trajectories"))+
  # ggtitle(paste0(timeWindow," forward trajectories"))+
    
  theme_bw()
GG
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Halo_backward14_Lim/",timeWindow,"_Halozone14bLim.png"),plot=GG,width=10, height=10, dpi = 300)

}
  
#Make animations for Enrique: 
## Animate gifs to compare trajectories

# #Forward
# list.files(path='/Users/heidi.k.hirsh/Desktop/Halo_forward14_Lim', pattern = '*.png', full.names = TRUE) %>% 
#   mixedsort() %>%
#   image_read() %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=2) %>% # animates, can opt for number of loops
#   image_write("/Users/heidi.k.hirsh/Desktop/HaloForward14.gif") # write to current dir


#Backward
list.files(path='/Users/heidi.k.hirsh/Desktop/Halo_backward14_Lim', pattern = '*.png', full.names = TRUE) %>% 
  mixedsort() %>%
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("/Users/heidi.k.hirsh/Desktop/HaloBackward14.gif") # write to current dir



#Try season loop
#create new variable for year_season
#or loop through year then season
#I like second idea better

plotData$Season = factor(plotData$Season, levels = c("Winter","Spring","Summer","Fall"))

Yrs = unique(plotData$Year)
Yrs
Seas= levels(plotData$Season)
Seas
Dur=unique(plotData$n_days)
Dur
# dim(Seas)
# class(Seas)
# levels(Seas)
# levels(plotData$Season)

# for (my_i in 1:length(MoYr)) {
#   timeWindow=MoYr[my_i]

# y_i=1; s_i=1
dim(plotData)  
table(plotData$Year,plotData$Season)
#try a year that has all seasons (2018)
# y_i=6; s_i=1

y_i = NULL
s_i = NULL

for (y_i in 1:length(Yrs)) {
    timeYear=Yrs[y_i]
    ploThis1 = subset(plotData,Year==timeYear)
    dim(ploThis1)
    
  
    for (s_i in 1:length(Seas)) {
      timeSeason=Seas[s_i]
      ploThis2 = subset(ploThis1,Season==timeSeason)
      dim(ploThis2)
      

  ploThis_t = st_transform(ploThis2,crs='+proj=longlat +datum=WGS84')
  # dim(ploThis_t)

  GG = ggplot(FLKs1)+
    geom_sf(fill = "black", lwd = 0)+
    geom_sf(data=subset(ploThis_t,simu=='backward'),aes(color=n_days,fill=n_days),inherit.aes = FALSE,alpha=0)+
    scale_color_gradientn(colours=pnw)+
    scale_fill_gradientn(colours=pnw)+
    geom_point(aes(x=Longitude,y=Latitude),color="magenta",size=3,data=ploThis_t)+
    ylab('Latitude')+
    xlab('Longitude')+
    coord_sf(xlim = c(-83.7, -80), ylim = c(23.55, 26), expand = FALSE) + #for backward halos
    ggtitle(paste0(timeYear," ",timeSeason," backward trajectories"))+
    
    theme_bw()
  GG
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Halo_backward14_Season/",timeYear,"_",timeSeason,"_Halozone14bYrSeas.png"),plot=GG,width=10, height=10, dpi = 300)
  
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Halo_backward14_Season/",timeWindow,"_Halozone14bSeason.png"),plot=GG,width=10, height=10, dpi = 300)
  
    }
}


#can I do a version looping through years and faceting by season so I can see seasons together? 
y_i=NULL
for (y_i in 1:length(Yrs)) {
  timeYear=Yrs[y_i]
  ploThis1 = subset(plotData,Year==timeYear)
  dim(ploThis1)
  
  ploThis_t = st_transform(ploThis1,crs='+proj=longlat +datum=WGS84')
    # dim(ploThis_t)
    
    GG = ggplot(FLKs1)+
      geom_sf(fill = "black", lwd = 0)+
      geom_sf(data=subset(ploThis_t,simu=='backward'),aes(color=n_days,fill=n_days),inherit.aes = FALSE,alpha=0)+
      scale_color_gradientn(colours=pnw)+
      scale_fill_gradientn(colours=pnw)+
      facet_wrap(~Season,nrow=2)+
      geom_point(aes(x=Longitude,y=Latitude),color="magenta",size=3,data=ploThis_t)+
      ylab('Latitude')+
      xlab('Longitude')+
      coord_sf(xlim = c(-83.7, -80), ylim = c(23.55, 26), expand = FALSE) + #for backward halos
      ggtitle(paste0(timeYear," backward trajectories"))+
      
      theme_bw()
    GG
    ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Halo_backward14_Season/facetSeason2/",timeYear,"_Halozone14b_facetSeason.png"),plot=GG,width=10, height=10, dpi = 300)
    
}




#OR/AND combine all years and plot together. color by year, facet by season
#or leoop through season

#loop through ndays (Dur)
n_i=NULL
for (n_i in 1:length(Dur)) {
  timeN=Dur[n_i]
  ploThis1 = subset(plotData,n_days==timeN)
  dim(ploThis1)
# ploThis1=plotData
# ploThis1= subset(plotData, n_days==7)
ploThis_t = st_transform(ploThis1,crs='+proj=longlat +datum=WGS84')


GG = ggplot(FLKs1)+
  geom_sf(fill = "black", lwd = 0)+
  geom_sf(data=subset(ploThis_t,simu=='backward'),aes(color=Year,fill=Year),inherit.aes = FALSE,alpha=0)+
  scale_color_gradientn(colours=pnw)+
  scale_fill_gradientn(colours=pnw)+
  facet_wrap(~Season,nrow=2)+
  geom_point(aes(x=Longitude,y=Latitude),color="magenta",size=3,data=ploThis_t)+
  ylab('Latitude')+
  xlab('Longitude')+
  coord_sf(xlim = c(-83.7, -80), ylim = c(23.55, 26), expand = FALSE) + #for backward halos
  ggtitle(paste0(timeN," days, backward trajectories"))+
  
  theme_bw()
GG
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Halo_backward14_Season/ndays/",timeN,"days_Halozone14b_facetSeasonColorYear_Ndays.png"),plot=GG,width=10, height=10, dpi = 300)
}

#can I do a version looping through season, color by year
s_i=NULL
for (s_i in 1:length(Seas)) {
  timeSeason=Seas[s_i]
  ploThis1 = subset(plotData,Season==timeSeason)
  # dim(ploThis1)
  ploThis1= subset(ploThis1, n_days==7)
  
  ploThis_t = st_transform(ploThis1,crs='+proj=longlat +datum=WGS84')
  # dim(ploThis_t)
  
  GG = ggplot(FLKs1)+
    geom_sf(fill = "black", lwd = 0)+
    geom_sf(data=subset(ploThis_t,simu=='backward'),aes(color=as.factor(Year),fill=as.factor(Year)),inherit.aes = FALSE,alpha=0)+
    # scale_color_gradientn(colours=pnw)+
    # scale_fill_gradientn(colours=pnw)+
    # facet_wrap(~Season,nrow=2)+
    geom_point(aes(x=Longitude,y=Latitude),color="magenta",size=3,data=ploThis_t)+
    ylab('Latitude')+
    xlab('Longitude')+
    coord_sf(xlim = c(-83.7, -80), ylim = c(23.55, 26), expand = FALSE) + #for backward halos
    ggtitle(paste0(timeSeason," backward trajectories"))+
    
    theme_bw()
  GG
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Halo_backward14_Season/colorYear/",timeSeason,"_Halozone14b_colorYear_7days.png"),plot=GG,width=10, height=10, dpi = 300)
  
}


