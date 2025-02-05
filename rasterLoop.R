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
# # ## future: I need a better way to store this data together so I can index it for plotting. 
# 
# nn=14
# # BBpolys = subset(plotData_t, n_days==nn & Zone=="Inshore" & Sub_region=="BB") # & Year==2018 & Season=="Summer")
# # UKpolys = subset(plotData_t, n_days==nn & Zone=="Inshore" & Sub_region=="UK") # & Year==2018 & Season=="Summer")
# MKpolys = subset(plotData_t, n_days==nn & Zone=="Inshore" & Sub_region=="MK" & Year==2018 & Season=="Summer")
# # LKpolys = subset(plotData_t, n_days==nn & Zone=="Inshore" & Sub_region=="LK") # & Year==2018 & Season=="Summer")
# 
# 
# #plot each regional subset together
# 
# # polygons = BBpolys
# # polygons = UKpolys
# polygons = MKpolys
# # polygons = LKpolys
# 
# dim(polygons)
# 
# polygons_vect <- vect(polygons)  # 'polygons' is your sf dataframe
# 
# # Define a raster template for the entire dataset
# raster_template <- rast(ext(polygons_vect), resolution = 0.01, crs = crs(polygons_vect))
# 
# # Rasterize each polygon and store in a list
# rasters_list <- lapply(1:nrow(polygons_vect), function(i) {
#   rasterize(polygons_vect[i, ], raster_template, field = 1, background = 0)
# })
# # Stack all rasters
# rasters_stack <- rast(rasters_list)
# 
# # Calculate the overlap frequency
# overlap_raster <- sum(rasters_stack, na.rm = TRUE)
# plot(overlap_raster)
# raster_df <- as.data.frame(overlap_raster, xy = TRUE)  # Include coordinates (x, y)
# 
# ggplot() +
#   geom_sf(data=FLKs1,fill = "black", lwd = 0)+
#   geom_raster(data = raster_df, aes(x = x, y = y, fill = sum),alpha=.6) +  # Plot the raster data
#   scale_fill_viridis_c(option = "magma") +  # Adjust color scale
#   coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
#   theme_minimal() +
#   labs(title = "Polygon Overlap Frequency",
#        x = "Longitude",
#        y = "Latitude",
#        fill = "Overlap Count")



#### Build loop
sites=unique(plotData_t$SiteID)
Yrs = unique(plotData_t$Year)
Seas= levels(plotData_t$Season)
Reg = unique(plotData_t$Sub_region)
Yrs


#### _____________________ plotting loop for separate seasonal plots
#initialize: 
y_i=NULL; s_i=NULL; r_i=NULL;p_i=NULL
# y_i=3; s_i=3; r_i=3;
nn=14


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
timeYear="allYears"  

  for (s_i in 1:length(Seas)) {
    timeSeason=Seas[s_i]
    print(paste0("Start season ",s_i," of ",length(Seas)))
    # ploThis2 = subset(ploThis1,Season==timeSeason)
    ploThis2 = subset(plotData_t,Season==timeSeason)
    
    # unique(ploThis2$Year)
    
          
    # for (r_i in 1:length(Reg)) {
    #   place=Reg[r_i]
    #   print(paste0("start ",r_i,"_of_",length(Reg)))
    #   ploThis3 = subset(ploThis2,Sub_region==place)
      
    for (p_i in 1:length(sites)) {
        place=sites[p_i]
        print(paste0("Start site ",p_i," of ",length(sites)))
        ploThis3 = subset(ploThis2,SiteID==place)

        

  ploThis = subset(ploThis3,n_days==nn)
  flowshedNum = length(ploThis$visitID)
  dim(ploThis)
  
  #calculate raster
  
  
  plotFlowshed = ggplot()+
    geom_sf(data=FLKs1,fill = "gray17", lwd = 0)+                                           #Florida shoreline
    geom_sf(data=ploThis,aes(color=as.factor(Year)),inherit.aes = FALSE,alpha=0)+
    year_COLOR_scale+
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
    ggtitle(paste0(place,", ",timeSeason,", ",nn,"-days backward (",flowshedNum," samples)"))+
    # ggtitle(paste0(place,", ",timeYear,", ",timeSeason,", ",nn,"-days backward"))+
    
    theme_bw()+
    theme(axis.line = element_line(color='black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  plotFlowshed
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/OverlapFrequency/allYears/polygons/Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,".png"),plot=plotFlowshed,width=10, height=10, dpi = 300)
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/OverlapFrequency/allYearsbySite/polygons/Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,"(",flowshedNum,"bows).png"),plot=plotFlowshed,width=10, height=10, dpi = 300)
  

  
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
    # theme_minimal() +
    theme_bw() +
    ggtitle(paste0(place,", ",timeSeason,", ",nn,"-days backward (",flowshedNum," samples)"))+
    # ggtitle(paste0(place,", ",timeYear,", ",timeSeason,", ",nn,"-days backward"))+
    labs(x = "Longitude",
         y = "Latitude",
         # title = "Polygon Overlap Frequency",
         fill = "Overlap Count")
  RR
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/OverlapFrequency/allYearsbySite/rasters/OverlapRaster_Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,"(",flowshedNum,"bows).png"),plot=RR,width=10, height=10, dpi = 300)
  

  combo=plotFlowshed/RR
  combo
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/OverlapFrequency/allYearsbySite/combos/COMBOOverlapRaster_Inshore_Backward_",nn,"day_",timeYear,"_",timeSeason,"_",place,"(",flowshedNum,"bows).png"),plot=combo,width=10, height=10, dpi = 300)
 
  } #end part to run if there is data for raster
  else {
  } #end alternative (don't plot)
   
# }
} #End region (or site) loop
} #End season loop
#### _____________________ END plotting loop for separate seasonal plots

#### next, plot 4-part seasonal plot by SITE

## instead of looping through seasons, create four different plots and subset for each. Combine into one seasonal plot: 
#initialize: 
y_i=NULL; s_i=NULL; r_i=NULL;p_i=NULL
# y_i=3; s_i=3; r_i=3;
nn=14


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
timeYear="allYears"  

# for (s_i in 1:length(Seas)) {
#   timeSeason=Seas[s_i]
#   print(paste0("Start season ",s_i," of ",length(Seas)))
#   # ploThis2 = subset(ploThis1,Season==timeSeason)
#   ploThis2 = subset(plotData_t,Season==timeSeason)
  
  # for (r_i in 1:length(Reg)) {
  #   place=Reg[r_i]
  #   print(paste0("start ",r_i,"_of_",length(Reg)))
  #   ploThis3 = subset(ploThis2,Sub_region==place)
  
p_i = NULL
# p_i = 6
nn=14

  # for (p_i in 1:length(sites)) {
  #   place=sites[p_i]
  #   print(paste0("Start site ",p_i," of ",length(sites)))
  #   ploThis3 = subset(plotData_t,SiteID==place & n_days==nn)

ploThis3 = subset(plotData_t,SiteID==16 & n_days==nn)
place = 16
    
    ## info for labels:
    timeYear="allYears"
    timeSeason="allSeasons"
    # region=ploThis3$Sub_region
    
    # polygons_vect <-NULL
    # rasters_list <-NULL
    # rasters_stack <-NULL
    # overlap_raster <-NULL
    # raster_df <-NULL

    ## WINTER
    plotWinter = subset(ploThis3,Season=="Winter")
    # dim(plotWinter)
    # unique(plotWinter$Season)
    flowshedNum.W = length(plotWinter$visitID)
    polygons_vect.W <- vect(plotWinter)  # 'polygons' is your sf dataframe
    rasters_list.W <- lapply(1:nrow(polygons_vect.W), function(i) {rasterize(polygons_vect.W[i, ], raster_template, field = 1, background = 0)})
    rasters_stack.W <- rast(rasters_list.W)
    overlap_raster.W <- sum(rasters_stack.W, na.rm = TRUE)
    raster_df.W <- as.data.frame(overlap_raster.W, xy = TRUE)  # Include coordinates (x, y)

    ## SPRING
    plotSpring = subset(ploThis3,Season=="Spring")
    # if plotSpring
    flowshedNum.Sp = length(plotSpring$visitID)
    polygons_vect.Sp <- vect(plotSpring)  # 'polygons' is your sf dataframe
    rasters_list.Sp <- lapply(1:nrow(polygons_vect.Sp), function(i) {rasterize(polygons_vect.Sp[i, ], raster_template, field = 1, background = 0)})
    rasters_stack.Sp <- rast(rasters_list.Sp)
    overlap_raster.Sp <- sum(rasters_stack.Sp, na.rm = TRUE)
    raster_df.Sp <- as.data.frame(overlap_raster.Sp, xy = TRUE)  # Include coordinates (x, y)
    
    ## SUMMER
    plotSummer = subset(ploThis3,Season=="Summer")
    flowshedNum.Su = length(plotSummer$visitID)
    polygons_vect.Su <- vect(plotSummer)  # 'polygons' is your sf dataframe
    rasters_list.Su <- lapply(1:nrow(polygons_vect.Su), function(i) {rasterize(polygons_vect.Su[i, ], raster_template, field = 1, background = 0)})
    rasters_stack.Su <- rast(rasters_list.Su)
    overlap_raster.Su <- sum(rasters_stack.Su, na.rm = TRUE)
    raster_df.Su <- as.data.frame(overlap_raster.Su, xy = TRUE)  # Include coordinates (x, y)
  
    ## FALL
    plotFall = subset(ploThis3,Season=="Fall")
    flowshedNum.F = length(plotFall$visitID)
    polygons_vect.F <- vect(plotFall)  # 'polygons' is your sf dataframe
    rasters_list.F <- lapply(1:nrow(polygons_vect.F), function(i) {rasterize(polygons_vect.F[i, ], raster_template, field = 1, background = 0)})
    rasters_stack.F <- rast(rasters_list.F)
    overlap_raster.F <- sum(rasters_stack.F, na.rm = TRUE)
    raster_df.F <- as.data.frame(overlap_raster.F, xy = TRUE)  # Include coordinates (x, y)

    
    
      # polygons_vect <- vect(polygons)  # 'polygons' is your sf dataframe
      # rasters_list <- lapply(1:nrow(polygons_vect), function(i) {rasterize(polygons_vect[i, ], raster_template, field = 1, background = 0)})
      # rasters_stack <- rast(rasters_list)
      # overlap_raster <- sum(rasters_stack, na.rm = TRUE)
      # raster_df <- as.data.frame(overlap_raster, xy = TRUE)  # Include coordinates (x, y)


      
      #now plot raster (and save)
      WIN=ggplot() +
        geom_sf(data=FLKs1,fill = "black", lwd = 0)+
        geom_raster(data = raster_df.W, aes(x = x, y = y, fill = sum),alpha=.6) +  # Plot the raster data
        scale_fill_viridis_c(option = "magma") +  # Adjust color scale
        geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+                 #Plot all stations
        geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+  #add West Florida Shelf points
        geom_point(data=plotWinter,aes(x=dec.lon,y=dec.lat),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
        # coord_sf(xlim = c(-82.7, -80.0198748985922), ylim = c(24, 26.0703716407435), expand = FALSE) + #for backward halos
        # coord_sf(xlim = c(-81.22, -80.05), ylim = c(24.75, 25.6), expand = FALSE) +
        coord_sf(xlim = c(-82.7, -80.05), ylim = c(24.4, 26.07), expand = FALSE) + 
        theme_bw() +
        # scale_x_continuous(breaks = seq(-83, -80, by = 1),minor_breaks = seq(-83, -80, by = .5)) +
        scale_y_continuous(breaks = seq(24, 26, by = .5))+ #,minor_breaks = seq(24, 26,  by = .5)) +
      # scale_x_continuous(breaks = seq(-82.5, -80.5, by = 1)) +
      # scale_y_continuous(breaks = seq(24.4, 26, by = .5))
        # theme(legend.position = "bottom") +
        theme(legend.position = "right") +    
        # ggtitle(paste0(unique(plotWinter$Sub_region),"_",place,", Winter, ",nn,"-days backward (n=",length(plotWinter$visitID),")"))+
        ggtitle(paste0("Winter (n=",length(plotWinter$visitID),")"))+
        theme(axis.text.y = element_text(size = 12),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),axis.title.y = element_blank())+
        labs(x=NULL,y=NULL,fill=NULL)
      # WIN
      
      SPR=ggplot() +
        geom_sf(data=FLKs1,fill = "black", lwd = 0)+
        geom_raster(data = raster_df.Sp, aes(x = x, y = y, fill = sum),alpha=.6) +  # Plot the raster data
        scale_fill_viridis_c(option = "magma") +  # Adjust color scale
        geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+                 #Plot all stations
        geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+  #add West Florida Shelf points
        geom_point(data=plotSpring,aes(x=dec.lon,y=dec.lat),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
        # coord_sf(xlim = c(-82.7, -80.0198748985922), ylim = c(24, 26.0703716407435), expand = FALSE) + #for backward halos
        # coord_sf(xlim = c(-81.22, -80.05), ylim = c(24.75, 25.6), expand = FALSE) +
        coord_sf(xlim = c(-82.7, -80.05), ylim = c(24.4, 26.07), expand = FALSE) + 
        theme_bw() +
        # scale_x_continuous(breaks = seq(-83, -80, by = 1),minor_breaks = seq(-83, -80, by = .5)) +
        scale_y_continuous(breaks = seq(24, 26, by = .5))+ #,minor_breaks = seq(24, 26,  by = .5)) +
        # ylab(" ")+
        # theme(legend.position = "bottom") +
        theme(legend.position = "right") +
        ggtitle(paste0("Spring (n=",length(plotSpring$visitID),")"))+
        # ggtitle(paste0(unique(plotWinter$Sub_region),"_",place,", Spring, ",nn,"-days backward (n=",length(plotSpring$visitID),")"))+
        theme(axis.text.x = element_blank(),  
              axis.text.y= element_blank(),
              axis.title.x = element_blank(),axis.title.y = element_blank())+
        labs(x=NULL,y=NULL,fill=NULL)
      # SPR
      
      SUM=ggplot() +
        geom_sf(data=FLKs1,fill = "black", lwd = 0)+
        geom_raster(data = raster_df.Su, aes(x = x, y = y, fill = sum),alpha=.6) +  # Plot the raster data
        scale_fill_viridis_c(option = "magma") +  # Adjust color scale
        geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+                 #Plot all stations
        geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+  #add West Florida Shelf points
        geom_point(data=plotSummer,aes(x=dec.lon,y=dec.lat),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
        # coord_sf(xlim = c(-82.7, -80.0198748985922), ylim = c(24, 26.0703716407435), expand = FALSE) + #for backward halos
        # coord_sf(xlim = c(-81.22, -80.05), ylim = c(24.75, 25.6), expand = FALSE) +
        coord_sf(xlim = c(-82.7, -80.05), ylim = c(24.4, 26.07), expand = FALSE) + 
        theme_bw() +
        # scale_x_continuous(breaks = seq(-83, -80, by = 1),minor_breaks = seq(-83, -80, by = .5)) +
        scale_y_continuous(breaks = seq(24, 26, by = .5))+ #,minor_breaks = seq(24, 26,  by = .5)) +
        # theme(legend.position = "bottom") +
        theme(legend.position = "right") +
        theme(axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),axis.title.y = element_blank())+
        ggtitle(paste0("Summer (n=",length(plotSummer$visitID),")"))+
        # ggtitle(paste0(unique(plotWinter$Sub_region),"_",place,", Summer, ",nn,"-days backward (n=",length(plotSummer$visitID),")"))+        
        labs(x=NULL,y=NULL,fill=NULL)
      # SUM
      
      
      FAL=ggplot() +
        geom_sf(data=FLKs1,fill = "black", lwd = 0)+
        geom_raster(data = raster_df.F, aes(x = x, y = y, fill = sum),alpha=.6) +  # Plot the raster data
        scale_fill_viridis_c(option = "magma") +  # Adjust color scale
        geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.2)+                 #Plot all stations
        geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.2)+  #add West Florida Shelf points
        geom_point(data=plotFall,aes(x=dec.lon,y=dec.lat),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
        # coord_sf(xlim = c(-82.7, -80.0198748985922), ylim = c(24, 26.0703716407435), expand = FALSE) + #for backward halos
        # coord_sf(xlim = c(-81.22, -80.05), ylim = c(24.75, 25.6), expand = FALSE) +
        coord_sf(xlim = c(-82.7, -80.05), ylim = c(24.4, 26.07), expand = FALSE) + 
        theme_bw() +
        # scale_x_continuous(breaks = seq(-83, -80, by = 1),minor_breaks = seq(-83, -80, by = .5)) +
        scale_y_continuous(breaks = seq(24, 26, by = .5))+ #,minor_breaks = seq(24, 26,  by = .5)) +
        # ylab(" ")+
        # theme(legend.position = "bottom") +
        theme(legend.position = "right") +
        ggtitle(paste0("Fall (n=",length(plotFall$visitID),")"))+
        # ggtitle(paste0(unique(plotWinter$Sub_region),"_",place,", Fall, ",nn,"-days backward (n=",length(plotFall$visitID),")"))+
        theme(axis.text.x = element_text(size = 12),
              axis.text.y= element_blank(),
              axis.title.x = element_blank(),axis.title.y = element_blank())+
        labs(x=NULL,y=NULL,fill=NULL)
      # FAL
      
      allRast = (WIN+SPR)/(SUM+FAL)
      # allRast = (WIN+SPR)/(SUM+FAL)+ 
      #   plot_layout(guides = 'collect') + # Adjust overall layout
      #   plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0))) +
      #   theme(plot.spacing = unit(5, "pt")) # Reduce space between rows
      # allRast
      
      final_plot <- allRast + plot_annotation(subtitle = paste0("Region:",unique(ploThis3$Sub_region)," Site:",place," ",nn,"-day flowsheds"))
      # final_plot
      
      # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/overlapRvscale2.png"),plot=final_plot,width=10, height=10, dpi = 300)
      ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/overlapRvscale3.png"),plot=final_plot,width=10, height=10, dpi = 300)
      
      # library(Cairo)
      library(devEMF)
      # Cairo::CairoEMF("final_plot.emf", width = 10, height = 10, dpi=300)
      emf("/Users/heidi.k.hirsh/Desktop/final_plot.emf",width = 10, height = 10)
      print(final_plot)  # Ensure the plot is explicitly printed
      dev.off()
      
      library(svglite)
      svglite("/Users/heidi.k.hirsh/Desktop/final_plot.svg", width = 10, height = 10)
      print(final_plot)
      dev.off()
      
      # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/OverlapFrequency/allYearsallSeasons/OverlapRaster_allSeasons_Inshore_Backward_",nn,"day_Site_",place,".png"),plot=final_plot,width=10, height=10, dpi = 300)
      
    # } #end part to run if there is data for raster
    # else {
    # } #end alternative (don't plot)
    
#     # }
#       
#   } #End region (or site) loop\
# 
# # } #End season loop

