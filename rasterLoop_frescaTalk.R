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
library(terra)
library(patchwork)
# Creates bibliography 
#knitr::write_bib(c(.packages()), "packages.bib")

#### Loapatchwork#### Load the stuff

#######_____________________________________________
FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')
# FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')

## Read in carbonate chemistry station data
CC = read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/CCflk_plusBathy.csv',na.strings=c(""," "))

#subset the carb chem data to represent the years we want and a subset of SiteIDs
CCyr = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')) #subset of chemistry samples that match bow ties:
## assign visitID columns to allow us to pair CC to the spatial polygon data:
CCyr$visitID_ch1 =  paste(CCyr$SiteID,CCyr$UTCDate_Time)
CCyr$visitID_ch2 =    gsub(" ", "_" , CCyr$visitID_ch1, perl=TRUE)
CCyr$visitID =  gsub("[: -]", "" , CCyr$visitID_ch2, perl=TRUE)

# plotCCyr = st_as_sf(CCyr, coords = c("Longitude","Latitude"), crs = st_crs(4326))
# #Simplify CCyr to just the columns required for plotting: 
CCyr.s = CCyr[c('visitID','SiteID','Latitude', 'Longitude','dec.lat','dec.lon','Year','Sub_region','Zone','Season','Month','MY','MoY','pointDepth','Precipitation')]

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
# mapview(plotShelf)

##only test on inshore first (load concave.bo for inshore stations)
load("/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/inshoreAllYrs_concave_bi.RData")



#### Load other points to plot for comparion
## all walton smith transects
ws = read.csv('/Users/heidi.k.hirsh/Desktop/halo_points/WS_2023_coords.csv')
head(ws)
ws$Latitude=ws$dec.lat
ws$Longitude=ws$dec.lon
ws.sf = st_as_sf(ws, coords= c("dec.lon","dec.lat"),crs = st_crs(4326)) 

## Sept 2024 FLB samples: 
flb9 = read.csv('/Users/heidi.k.hirsh/Desktop/FLBcruises/FLBsept2024.csv')
flb9$Latitude=flb9$Lat
flb9$Longitude=flb9$Lon
FLB9 = st_as_sf(flb9, coords= c("Lon","Lat"),crs = st_crs(4326)) 
#######_____________________________________________
data = concave.bi
plotData_t = st_transform(data,crs='+proj=longlat +datum=WGS84')
plotData_t$Season = factor(data$Season, levels = c("Winter","Spring","Summer","Fall"))
levels(plotData_t$Season)

#### color palettes
## site color scale
nColor <- length(unique(plotData_t$SiteID))
clrs = rainbow(nColor, start = 0, end =0.75)
site_colors <- clrs
site_COLOR_scale  <- scale_color_manual(name = "Site", values = site_colors,
                                        breaks = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'),
                                        labels = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'))

site_FILL_scale  <- scale_fill_manual(name = "Site", values = site_colors,
                                      breaks = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'),
                                      labels = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'))

## year color scale
nYears=length(unique(plotData_t$Year))
year_colors <- hcl.colors(9, palette = "Set2") 
# scales::show_col(year_colors)

#SIMPLE
year_COLOR_scale  <- scale_color_manual(name = "Year", values = year_colors,
                                        breaks = c('2012','2014','2015','2016','2017','2018','2019','2020','2021'),
                                        labels = c('2012','2014','2015','2016','2017','2018','2019','2020','2021'))

year_FILL_scale  <- scale_fill_manual(name = "Year", values = year_colors,
                                      breaks = c('2012','2014','2015','2016','2017','2018','2019','2020','2021'),
                                      labels = c('2012','2014','2015','2016','2017','2018','2019','2020','2021'))





# # ### Build overlap frequency rasters for different subsets of data and plot. 


#### Build loop
sites=unique(plotData_t$SiteID)
Yrs = unique(plotData_t$Year)
Seas= levels(plotData_t$Season)
Reg = unique(plotData_t$Sub_region)


#### _____________________ plotting loop for separate seasonal plots
#initialize: 
y_i=NULL; s_i=NULL; r_i=NULL;p_i=NULL
# y_i=3; s_i=3; r_i=3;
nn=5


##make a raster template first: 
# Define the raster template outside of loop
# overall_extent <- ext(-82.7, -80, 24.35, 26)  #based on later your coordinate bounds
##is this large enough to include all polygons?  #not quite so I will just use the original polygon subset extent
# ext(plotData_t) #SpatExtent : -83.6460525073308, -80.0198748985922, 23.6015665134704, 26.0703716407435 (xmin, xmax, ymin, ymax)
# raster_template <- rast(ext=overall_extent, resolution=0.01, crs=crs(FLKs1))
# raster_template <- rast(ext(polygons_vect), resolution = 0.01, crs = crs(polygons_vect))

raster_template <- rast(ext(plotData_t), resolution = 0.01, crs = crs(plotData_t)) 
values(raster_template) <- 0  # Initialize with zeros


# for (y_i in 1:length(Yrs)) {
#   timeYear=Yrs[y_i]
#   ploThis1 = subset(plotData_t,Year==timeYear)
#   print(paste0("Start year ",y_i," of ",length(Yrs)))
timeYear="allYears"

  # for (s_i in 1:length(Seas)) {
  #   timeSeason=Seas[s_i]
  #   print(paste0("Start season ",s_i," of ",length(Seas)))
  #   ploThis2 = subset(ploThis1,Season==timeSeason)
    # ploThis2 = subset(plotData_t,Season==timeSeason)
    
    timeSeason="allSeasons"
    # unique(ploThis2$Year)
    
          
    # for (r_i in 1:length(Reg)) {
    #   place=Reg[r_i]
    #   print(paste0("Start region ",r_i,"_of_",length(Reg)))
    #   ploThis3 = subset(plotData_t,Sub_region==place)
      # ploThis3 = subset(ploThis1,Sub_region==place)
      # ploThis3 = subset(ploThis2,Sub_region==place)
      
      place="allRegions"
      ploThis3=plotData_t
      
    # for (p_i in 1:length(sites)) {
    #     place=sites[p_i]
    #     print(paste0("Start site ",p_i," of ",length(sites)))
    #     ploThis3 = subset(ploThis2,SiteID==place)

        

  ploThis = subset(ploThis3,n_days==nn)
  flowshedNum = length(ploThis$visitID)
  dim(ploThis)
  
  #calculate raster
  
  
  plotFlowshed = ggplot()+
    geom_sf(data=FLKs1,fill = "gray17", lwd = 0)+                                           #Florida shoreline
    # geom_sf(data=ploThis,aes(color=as.factor(Year)),inherit.aes = FALSE,alpha=0)+
    # year_COLOR_scale+
    geom_sf(data=ploThis,aes(color=as.factor(SiteID)),inherit.aes = FALSE,alpha=0)+
    site_COLOR_scale+
    # year_FILL_scale+   
    # geom_sf(data=ploThis,aes(color=SiteID),inherit.aes = FALSE,alpha=0)+
    geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+                 #Plot all stations
    geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+  #add West Florida Shelf points
    geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat,fill=SiteID),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
    # site_COLOR_scale+
    site_FILL_scale+
    # facet_wrap(~Year,nrow=4)+
    ylab('Latitude')+ xlab('Longitude')+
    # coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
    coord_sf(xlim = c(-82.7, -80.0198748985922), ylim = c(24.35, 26.0703716407435), expand = FALSE) + #for backward halos
    # ggtitle(paste0(place,", ",timeSeason,", ",nn,"-days backward (",flowshedNum," samples)"))+
    # ggtitle(paste0(place,", ",timeYear,", ",timeSeason,", ",nn,"-days (",flowshedNum," samples)"))+
    ggtitle(paste0(timeYear,", ",timeSeason,", ",nn,"-days (",place,", ",flowshedNum," samples)"))+
    
    # ggtitle(paste0(place,", ",timeYear,", ",timeSeason,", ",nn,"-days backward"))+
    
    theme_bw()+
    theme(axis.line = element_line(color='black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  plotFlowshed
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/OverlapFrequency/allYears/polygons/Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,".png"),plot=plotFlowshed,width=10, height=10, dpi = 300)
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENÇZY/OverlapFrequency/RegionAll_14/polygons/",place,"/Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,"(",flowshedNum,"bows).png"),plot=plotFlowshed,width=10, height=10, dpi = 300)
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/Everything_5/polygons/",place,"/Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,"(",flowshedNum,"bows).png"),plot=plotFlowshed,width=10, height=10, dpi = 300)
  

  
  #now calculate raster for each region (year and season)
  polygons = ploThis
  
  if(nrow(polygons)>0) {
    
  polygons_vect <- vect(polygons)  # 'polygons' is your sf dataframe
  
  # Define a raster template for the entire dataset
  # raster_template <- rast(ext(polygons_vect), resolution = 0.01, crs = crs(polygons_vect)) ## do this in advance for larger area
  
  # Rasterize each polygon and store in a list
  rasters_list <- lapply(1:nrow(polygons_vect), function(i) {rasterize(polygons_vect[i, ], raster_template, field = 1, background = 0)})
  # Stack all rasters
  rasters_stack <- rast(rasters_list)
  
  # Calculate the overlap frequency
  overlap_raster <- sum(rasters_stack, na.rm = TRUE)
  # plot(overlap_raster)
  raster_df <- as.data.frame(overlap_raster, xy = TRUE)  # Include coordinates (x, y)
  

  #now plot raster (and save)
  RR=ggplot() +
    geom_sf(data=FLKs1,fill = "black", lwd = 0)+
    geom_raster(data = raster_df, aes(x = x, y = y, fill = sum),alpha=.6) +  # Plot the raster data
    scale_fill_viridis_c(option = "magma") +  # Adjust color scale
    geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+                 #Plot all stations
    geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+  #add West Florida Shelf points
    coord_sf(xlim = c(-82.7, -80.0198748985922), ylim = c(24, 26.0703716407435), expand = FALSE) + #for backward halos
    geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
    # theme_minimal() +
    theme_bw() +
    # ggtitle(paste0(place,", ",timeSeason,", ",nn,"-days backward (",flowshedNum," samples)"))+
    # ggtitle(paste0(place,", ",timeYear,", ",timeSeason,", ",nn,"-days (",flowshedNum," samples)"))+
    ggtitle(paste0(timeYear,", ",timeSeason,", ",nn,"-days (",place,", ",flowshedNum," samples)"))+
    # ggtitle(paste0(timeYear,", ",timeSeason,", ",nn,"-days (",place,", ",flowshedNum," samples)"))+
   
    labs(x = "Longitude",
         y = "Latitude",
         # title = "Polygon Overlap Frequency",
         fill = "Overlap Count")
  RR
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/OverlapFrequency/allYearsbySite/rasters/OverlapRaster_Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,"(",flowshedNum,"bows).png"),plot=RR,width=10, height=10, dpi = 300)
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/allYearsbySite/rasters/OverlapRaster_Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,"(",flowshedNum,"bows).png"),plot=RR,width=10, height=10, dpi = 300)
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/Everything_5/rasters/",place,"/OverlapRaster_Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,"(",flowshedNum,"bows).png"),plot=RR,width=10, height=10, dpi = 300)
  

  combo=plotFlowshed/RR
  combo
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/OverlapFrequency/allYearsbySite/combos/COMBOOverlapRaster_Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,"(",flowshedNum,"bows).png"),plot=combo,width=10, height=10, dpi = 300)
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/allYearsbySite/combos/COMBOOverlapRaster_Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,"(",flowshedNum,"bows).png"),plot=combo,width=10, height=10, dpi = 300)
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/RegbyYearbySeason/combos/COMBOOverlapRaster_Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,"(",flowshedNum,"bows).png"),plot=combo,width=10, height=10, dpi = 300)
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/Everything_5/combos/",place,"/COMBOOverlapRaster_Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,"(",flowshedNum,"bows).png"),plot=combo,width=10, height=10, dpi = 300)
  
  
  } #end part to run if there is data for raster
  else {
  } #end alternative (don't plot)
   
# }
# } #End region (or site) loop
# } #End season loop
# } #End year loop
  print("done")
#### _____________________ END plotting loop for separate seasonal plots


  
  
### plot ALL 5day flowsheds and add all current and potential sampling stations: 
  ### plot all flowsheds together
  y_i=NULL; s_i=NULL; r_i=NULL;p_i=NULL
  nn=5
  nn
  
  raster_template <- rast(ext(plotData_t), resolution = 0.01, crs = crs(plotData_t)) 
  values(raster_template) <- 0  # Initialize with zeros
  
  timeYear="allYears"
  timeSeason="allSeasons"
  place="allRegions"
  
  ploThis3=plotData_t
  ploThis = subset(ploThis3,n_days==nn)
  flowshedNum = length(ploThis$visitID)
  
  #now calculate raster for each region (year and season)
  polygons = ploThis
  polygons_vect <- vect(polygons)  
  rasters_list <- lapply(1:nrow(polygons_vect), function(i) {rasterize(polygons_vect[i, ], raster_template, field = 1, background = 0)})
  rasters_stack <- rast(rasters_list)
  overlap_raster <- sum(rasters_stack, na.rm = TRUE)
  raster_df <- as.data.frame(overlap_raster, xy = TRUE)  # Include coordinates (x, y)
  
  RR=ggplot() +
    geom_sf(data=FLKs1,fill = "black", lwd = 0)+
    geom_raster(data = raster_df, aes(x = x, y = y, fill = sum),alpha=.6) +  # Plot the raster data
    scale_fill_viridis_c(option = "magma") +  # Adjust color scale
    
    geom_point(data=ws.sf,aes(x=Longitude,y=Latitude),color='green',size=.45)+ #all WS transects
    # geom_point(data=ws.sf,aes(x=Longitude,y=Latitude),color='red',size=.7,stroke=1)+ #all WS transects
    
    
    # geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="white",size=.7)+                 #Plot all stations
    # geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="white",size=.7)+  #add West Florida Shelf points
    
    geom_point(data=FLB9,aes(x=Longitude,y=Latitude),size=.45,color='cyan')+ #Florida Bay samples
    # geom_point(data=FLB9,aes(x=Longitude,y=Latitude,color=ODO.mgL),pch=18,size=3)+ #Florida Bay samples
    
    
    coord_sf(xlim = c(-82.5, -80.0198748985922), ylim = c(24.3, 26), expand = FALSE) + #for backward halos
    
    # coord_sf(xlim = c(-82.7, -80.0198748985922), ylim = c(24, 26.0703716407435), expand = FALSE) + 
    geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat),color='black',pch=21,size=1.5,stroke=1)+  
    
    #additionally circle "endmember determining samples"
    geom_point(data=subset(CCyr.s,Zone=="Oceanic"),aes(x=dec.lon,y=dec.lat),color='magenta',pch=21,size=1.5,stroke=1)+  
    geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color='magenta',pch=21,size=1.5,stroke=1)+  
    
    geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="white",size=.7)+                 #Plot all stations
    geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="white",size=.7)+  #add West Florida Shelf points
    
    theme_bw() +
    ggtitle(paste0(timeYear,", ",timeSeason,", ",nn,"-days (",place,", ",flowshedNum," samples)"))+
    
    labs(x = "Longitude",
         y = "Latitude",
         # title = "Polygon Overlap Frequency",
         fill = "Overlap Count")
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/Overlap_withPoints/OverlapRaster_Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,"(",flowshedNum,"bows)_v11.png"),plot=RR,width=10, height=10, dpi = 300)
  
  
  
  
  
  # bayPoints=ggplot(FLKs1)+
  #   geom_sf(fill = "darkgray", lwd = 0)+
  #   geom_point(data=FLB9,aes(x=Longitude,y=Latitude,color=ODO.mgL),pch=18,size=3)+
  #   scale_colour_gradientn(colors = rainbow(8)) +
  #   # scale_color_brewer(palette = "OrRd")+
  #   geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color='cyan',size=3,stroke=1)+
  #   geom_point(data=ws.sf,aes(x=Longitude,y=Latitude),color='darkslategray',size=.7,stroke=1)+ 
  #   geom_point(data=halo.sf,aes(x=Longitude,y=Latitude),color='deeppink',size=1,stroke=1)+
  #   ylab('Latitude')+
  #   xlab('Longitude')+
  #   coord_sf(xlim = c(-82.1 , -80), ylim = c(24.4, 26), expand = FALSE) + #for backward halos
  #   theme_bw()
  # bayPoints
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#   
# ## Animate stacks of plots
# # ### MK by Year/Season -COMBO 5d
# # # list.files(path='/Users/heidi.k.hirsh/Desktop/testGif', pattern = '*.png', full.names = TRUE) %>% 
# list.files(path='/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/RegionbyYearbySeason_14/combos/MK', pattern = '*.png', full.names = TRUE) %>%
#     mixedsort() %>%
#     image_read() %>% # reads each path file
#     image_join() %>% # joins image
#     image_animate(fps=1) %>% # animates, can opt for number of loops
#     image_write("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/MK_YearsSeasons_14d_flowshedFrequency.gif")
# 
# #MK by Year/Season -RASTER only 5d
# # list.files(path='/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/RegionbyYearbySeason_14/rasters/MK', pattern = '*.png', full.names = TRUE) %>%
# #   mixedsort() %>%
# #   image_read() %>% # reads each path file
# #   image_join() %>% # joins image
# #   image_animate(fps=1) %>% # animates, can opt for number of loops
# #   image_write("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/MK_YearsSeasons_14d_flowshedFrequency_raster.gif")
# 
# 
# #14 days (by year and season)
# list.files(path='/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/RegionbyYearbySeason_5/combos/MK', pattern = '*.png', full.names = TRUE) %>%
#   mixedsort() %>%
#   image_read() %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=1) %>% # animates, can opt for number of loops
#   image_write("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/MK_YearsSeasons_5d_flowshedFrequency.gif")
# 
# 
# 
# 


#MK by Year/Season -RASTER only 
# list.files(path='/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/RegionbyYearbySeason_5/rasters/MK', pattern = '*.png', full.names = TRUE) %>%
#   mixedsort() %>%
#   image_read() %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=1) %>% # animates, can opt for number of loops
#   image_write("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/MK_YearsSeasons_5d_flowshedFrequency_raster.gif")

## REGION BY SEASON (5)

#Combos
list.files(path='/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/RegionbySeason_5/combos/MK', pattern = '*.png', full.names = TRUE) %>%
  mixedsort() %>%
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/MK_Seasons_5d_flowshedFrequency.gif")

# #Raster only
# list.files(path='/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/RegionbySeason_5/rasters/MK', pattern = '*.png', full.names = TRUE) %>%
#   mixedsort() %>%
#   image_read() %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=1) %>% # animates, can opt for number of loops
#   image_write("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/MK_Seasons_5d_flowshedFrequency_raster.gif")

## REGION BY SEASON (14)
#Combos
list.files(path='/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/RegionbySeason_14/combos/MK', pattern = '*.png', full.names = TRUE) %>%
  mixedsort() %>%
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/MK_Seasons_14d_flowshedFrequency.gif")

#flbgif
list.files(path='/Users/heidi.k.hirsh/Desktop/flb9', pattern = '*.png', full.names = TRUE) %>%
  mixedsort() %>%
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=.5) %>% # animates, can opt for number of loops
  image_write("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/FLBchem.gif")

# #Raster only
# list.files(path='/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/OverlapFrequency/RegionbySeason_14/rasters/MK', pattern = '*.png', full.names = TRUE) %>%
#   mixedsort() %>%
#   image_read() %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=1) %>% # animates, can opt for number of loops
#   image_write("/Users/heidi.k.hirsh/Desktop/FRESCAplotFRENZY/MK_Seasons_14d_flowshedFrequency_raster.gif")

