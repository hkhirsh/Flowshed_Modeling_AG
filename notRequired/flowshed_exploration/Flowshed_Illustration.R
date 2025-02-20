## Clean up this script to plot flowshed for manuscript summary figure
#first I want to look at all inshore site bows (just edit halozone figures for that)
#facet by months or years?
#then limit to only 7 days and look at different times of year

#then zoom in to one site in the middle keys (long key) - use same 2 times

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
CC = read.csv('/Users/heidi.k.hirsh/Desktop/FLK_data/CCflk_plusBathy.csv',na.strings=c(""," "))

# 
# FLK_mapGG=get_map(location=c(-80.99306,25.27732),zoom=7,maptype = "satellite")
# FLKs1=st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')
# class(FLKs1)
# # FLKs2=st_read('/Users/heidi.k.hirsh/Desktop/FLK_data/Florida_Shoreline_(1_to_40%2C000_Scale)/Florida_Shoreline_(1_to_40%2C000_Scale).shp')
# pnw=pnw_palette(name="Bay",n=14,type="continuous")



#subset the carb chem data to represent the years we want and a subset of SiteIDs
CCyr = subset(CC,Year 
              c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')) #subset of chemistry samples that match bow ties:
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

#Simplify CCyr to just the columns required for plotting: 
CCyr.s = CCyr[c('visitID','SiteID','Latitude', 'Longitude','Year','Sub_region','Zone','Season','Month','MY','MoY','pointDepth','Precipitation')]
head(CCyr.s)
names(CCyr.s)

CCbows=NULL
CCbowsALL=NULL

# CCbowsALL = left_join(yearBows, CCyr, by="visitID") 
CCbowsALL = left_join(yearBows, CCyr.s, by="visitID") 
class(CCbowsALL)  #"sf"         "data.frame"
dim(CCbowsALL)  #38640    73
# 38640/14/2 #1380 (4 extra samples)
# CCbows1=na.omit(CCbows0) #no extra

# #try long key site and cheeca 
# # CCbows=subset(CCbowsALL, SiteID 
# c('7','UK_IN')) #Long Key and Islamorada
# # CCbows=subset(CCbowsALL, SiteID 
# c('2','4','13','24'))
# # CCbows=subset(CCbowsALL, SiteID 
# c('7','UK_IN')) 

unique(CCbowsALL$Zone)

# CCbows=subset(CCbowsALL, Zone=='Inshore') 
# CCbows=subset(CCbowsALL, Zone=='Inshore' & Sub_region=='MK') 

CCbows=subset(CCbowsALL, Zone=='Inshore' & simu=='backward') 
# CCbows=subset(CCbowsALL, Zone=='Inshore' & simu=='backward' & MoY==6) 

dim(CCbows) 

# unique(CCbows$Zone)
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



# buffOut=concave.bo
# 
# dim(buffOut)
# head(buffOut)
# class(buffOut) #"sf"         "tbl_df"     "tbl"        "data.frame"
# # write_sf(buffOut,'/Users/heidi.k.hirsh/Desktop/BuffOut.shp')
# sf::st_write( obj = buffOut, dsn = '/Users/heidi.k.hirsh/Desktop/BuffOut2.shp')
# testbuff= read_sf('/Users/heidi.k.hirsh/Desktop/BuffOut.shp')
# dim(testBuff)

# names(concave.bo)
# concave.bo.simp= select(concave.bo, n_days, simu)


concave.bi = st_buffer(concave.bo, dist=-D, nQuadSegs=30)
dim(concave.bi)

plotData = concave.bi
# plotData=subset(concave.bi, n_days<8)
dim(plotData)

dim(plotData)
plotData_t = st_transform(plotData,crs='+proj=longlat +datum=WGS84')
# unique(plotData$SiteID)
# unique(plotData$MY)

# plot_a = subset(plotData_t,n_days<8)

MoYr = unique(plotData$MY)
MoYr

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


#panel a: all inshore sites 7days back, July 2019

plot_a = subset(plotData_t,n_days<8 & MY=="2019-07")
AA = ggplot(FLKs1)+
    geom_sf(fill = "black", lwd = 0)+
    geom_sf(data=subset(plot_a,simu=='backward'),aes(color=Sub_region,fill=Sub_region),inherit.aes = FALSE,alpha=0.03)+
    scale_color_brewer(palette='Dark2')+
    scale_fill_brewer(palette='Dark2')+
    geom_point(aes(x=Longitude,y=Latitude),color="blue",data=ploThis_t)+
    ylab('Latitude')+
    xlab('Longitude')+
    scale_x_continuous(limits=st_bbox(plotData_t)[c(1,3)])+
    scale_y_continuous(limits=st_bbox(plotData_t)[c(2,4)])+
    ggtitle(paste0(timeWindow," backward trajectories, n=7days"))+
    theme_bw()
# AA

plot_b = subset(plotData_t,n_days<8 & MY=="2019-09")
BB = ggplot(FLKs1)+
    geom_sf(fill = "black", lwd = 0)+
    geom_sf(data=subset(plot_b,simu=='backward'),aes(color=Sub_region,fill=Sub_region),inherit.aes = FALSE,alpha=0.03)+
    scale_color_brewer(palette='Dark2')+
    scale_fill_brewer(palette='Dark2')+
    geom_point(aes(x=Longitude,y=Latitude),color="blue",data=ploThis_t)+
    ylab('Latitude')+
    xlab('Longitude')+
    scale_x_continuous(limits=st_bbox(plotData_t)[c(1,3)])+
    scale_y_continuous(limits=st_bbox(plotData_t)[c(2,4)])+
    ggtitle(paste0(timeWindow," backward trajectories, n=7days"))+
    theme_bw()
# BB
  
plot_c = subset(plotData_t,SiteID==7 & MY=="2019-09")
CC = ggplot(FLKs1)+
  geom_sf(fill = "black", lwd = 0)+
  geom_sf(data=subset(plot_c,simu=='backward'),aes(color=Sub_region,fill=Sub_region),inherit.aes = FALSE,alpha=0.03)+
  scale_color_brewer(palette='Dark2')+
  scale_fill_brewer(palette='Dark2')+
  geom_point(aes(x=Longitude,y=Latitude),color="blue",data=ploThis_t)+
  ylab('Latitude')+
  xlab('Longitude')+
  scale_x_continuous(limits=st_bbox(plotData_t)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(plotData_t)[c(2,4)])+
  ggtitle(paste0(timeWindow," backward trajectories, n=7days"))+
  theme_bw()
CC


AB = plot_grid(AA,BB,CC,DD,
            labels=c("A","B","C","D")

#print timestamp: 
TS=Sys.time()
TS
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Test_Figs/test_",TS,".png"),plot=AB,width=10, height=10, dpi = 300)
