## This script recreates Figure 4
##Heidi K. Hirsh
##Last edit: Feb 19, 2025

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('PNWColors','sf','ggplot2','ggmap','mapview','cowplot','ggstar','dplyr')
lapply(packageload, library, character.only = TRUE)

FLKs1=st_read('Flowshed_Modeling_InputData/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')

#Read in carbonate chemistry station data

CC = read.csv('Flowshed_Modeling_InputData/CCflk_plusBathy.csv',na.strings=c(""," "))
# CC=read.csv('Flowshed_Modeling_InputData/CC_complete_cases_26april2024.csv')
#subset the carb chem data to represent the years we want and a subset of SiteIDs
CCyr = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')) #subset of chemistry samples that match bow ties:
## assign visitID columns to allow us to pair CC to the spatial polygon data:
CCyr$visitID_ch1 =  paste(CCyr$SiteID,CCyr$UTCDate_Time)
CCyr$visitID_ch2 =    gsub(" ", "_" , CCyr$visitID_ch1, perl=TRUE)
CCyr$visitID =  gsub("[: -]", "" , CCyr$visitID_ch2, perl=TRUE)

plotCCyr = st_as_sf(CCyr, coords = c("Longitude","Latitude"), crs = st_crs(4326))
# mapview(plotCCyr,zcol="SiteID")

## Load flowshed polygons
shp.list = list.files(path = paste0("Flowshed_Modeling_InputData/yearBows_22april2024"), pattern="*shp", full.names = T) 
shapefile_list = lapply(shp.list,read_sf)
all.yearBows = rbind(shapefile_list[[1]],shapefile_list[[2]],shapefile_list[[3]],shapefile_list[[4]])
yearBows=all.yearBows

#Simplify CCyr to just the columns useful for plotting: 
CCyr.s = CCyr[c('visitID','SiteID','Latitude', 'Longitude','dec.lat','dec.lon','Year','Sub_region','Zone','Season','Month','MY','MoY','pointDepth','Precipitation')]

CCbows=NULL
CCbowsALL=NULL
CCbowsALL = left_join(yearBows, CCyr.s, by="visitID")
# dim(CCbowsALL)

CCbows=subset(CCbowsALL, Zone=='Inshore' & simu=='backward' & Year==2019)
#only inshore sites
unique(CCbows$SiteID)
CCbows$SiteID = factor(CCbows$SiteID, levels = c("1","EK_IN","4","5","UK_IN","7","10","13","16","19","24"))

concave=CCbows
concave_utm17 = st_transform(concave, 26917)


#buffer (extend) out and in
# D=1/111.111*.5 #degrees per km
D=300
concave.bo = st_buffer(concave_utm17, dist=D, nQuadSegs=30)
# dim(concave.bo)

concave.bi = st_buffer(concave.bo, dist=-D, nQuadSegs=30)
# dim(concave.bi)

# save(concave.bi, file ="Flowshed_Modeling_InputData/inshore2019_concave_bi.RData")
# load("Flowshed_Modeling_InputData/inshore2019_concave_bi.RData")

plotData = concave.bi
plotData_t = st_transform(plotData,crs='+proj=longlat +datum=WGS84')



#define plot_a to plot_d here: 
plot_a = subset(plotData_t,n_days %in% c(1,7,14) & MY=="2019-07")
plot_b = subset(plotData_t,n_days %in% c(1,7,14) & MY=="2019-09")
plot_c = subset(plotData_t,SiteID==7 & MY=="2019-07") #try 16
plot_d = subset(plotData_t,SiteID==7 & MY=="2019-09")

nColor <- length(unique(plot_a$SiteID))
clrs0 = rainbow(nColor)
# scales::show_col(clrs0)
clrs = rainbow(nColor, start = 0, end =0.8)

#SIMPLE
# site_colors <- safe_pal
site_colors <- clrs0
site_COLOR_scale  <- scale_color_manual(name = "Site", values = site_colors,
                                        breaks = c('1','EK_IN','UK_IN','7','10','13','16','19','24'),
                                        labels = c('1','EK_IN','UK_IN','7','10','13','16','19','24'))

site_FILL_scale  <- scale_fill_manual(name = "Site", values = site_colors,
                                      breaks = c('1','EK_IN','UK_IN','7','10','13','16','19','24'),
                                      labels = c('1','EK_IN','UK_IN','7','10','13','16','19','24'))


pnw=pnw_palette(name="Bay",n=14,type="continuous")



## include shelf points
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
# mapview(plotShelf)


#4 panel: 

AA = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  geom_sf(data=subset(plot_a,simu=='backward'),aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.05)+
  site_COLOR_scale+
  site_FILL_scale+
  geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+
  geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+
  geom_point(aes(x=Longitude,y=Latitude,fill=SiteID),color='black',data=plot_a,pch=21,size=1.5,stroke=1)+
  geom_star(aes(x=Longitude,y=Latitude,fill=SiteID),color="black",data=plot_c,size=3)+
  ylab('Latitude')+
  xlab('Longitude')+
  coord_sf(xlim = c(-82.1, -79.95), ylim = c(24.4, 26.2), expand = FALSE) + #for backward halos
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position="none")

BB = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  geom_sf(data=plot_b,aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.05)+
  site_COLOR_scale+
  site_FILL_scale+
  geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+
  geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+
  geom_point(aes(x=Longitude,y=Latitude,fill=SiteID),color='black',data=plot_a,pch=21,size=1.5,stroke=1)+
  geom_star(aes(x=Longitude,y=Latitude,fill=SiteID),color="black",data=plot_c,size=3)+
  ylab('Latitude')+
  xlab('Longitude')+
  coord_sf(xlim = c(-82.1, -79.95), ylim = c(24.4, 26.2), expand = FALSE) +
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position="none")

CC = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  geom_sf(data=subset(plot_c,simu=='backward'),aes(color=n_days),inherit.aes = FALSE,alpha=0)+
  scale_color_gradientn(colours=pnw, breaks=c(1,7,14),labels=c('1 day','7 days','14 days'))+
  geom_star(aes(x=Longitude,y=Latitude),color="black",data=plot_c,size=3,fill='black')+
  ylab('Latitude')+
  xlab('Longitude')+
  coord_sf(xlim = c(-81.2, -79.95), ylim = c(24.75, 25.6), expand = FALSE) + 
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position=c(0.87,0.27),legend.title=element_blank())

DD = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  geom_sf(data=subset(plot_d,simu=='backward'),aes(color=n_days),inherit.aes = FALSE,alpha=0)+
  scale_color_gradientn(colours=pnw, breaks=c(1,7,14),labels=c('1 day','7 days','14 days'))+
  geom_star(aes(x=Longitude,y=Latitude),color="black",data=plot_c,size=3,fill='black')+
  ylab('Latitude')+
  xlab('Longitude')+
  coord_sf(xlim = c(-81.2, -79.95), ylim = c(24.75, 25.6), expand = FALSE) +
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position="none")


Fig4 = plot_grid(CC,DD,AA,BB,ncol=2,align="v") #only use v to align (or florida gets squished)
# ggsave(filename="Flowshed_Modeling_Figures/Figure4.png",plot=Fig4,width=10, height=10, dpi = 300)

  
INab=ggplot(data = FLKs1) +
  geom_sf(fill = "darkgray", lwd = 0) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme( axis.text.x = element_blank(), 
         axis.text.y = element_blank(), 
         axis.ticks = element_blank())+
  geom_rect(aes(xmin=-82.1,xmax=-79.95,ymin=24.4,ymax= 26.2),
            color='darkred',lwd=.8,fill=NA) +
  theme(panel.border = element_rect(colour = "black",fill=NA, linewidth=.9))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
INab
# ggsave(filename="Flowshed_Modeling_Figures/Fig4inset.png",plot=INab,width=10, height=10, dpi = 300)

