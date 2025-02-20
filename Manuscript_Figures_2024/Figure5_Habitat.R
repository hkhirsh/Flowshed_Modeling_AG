## This script recreates Figure 5
##Heidi K. Hirsh
##Last edit: Feb 19, 2025

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('sf','ggplot2','raster','mapview','ggplot2','ggstar','cowplot')
lapply(packageload, library, character.only = TRUE)

#Read in flowshed polygons (yearBows)
shp.list = list.files(path = paste0("Flowshed_Modeling_InputData/yearBows_22april2024"), pattern="*shp", full.names = T) 
shapefile_list = lapply(shp.list,read_sf)

all.yearBows = rbind(shapefile_list[[1]],shapefile_list[[2]],shapefile_list[[3]],shapefile_list[[4]])
yearBows=all.yearBows

FLKs1=st_read('Flowshed_Modeling_InputData/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')

CC = read.csv('Flowshed_Modeling_InputData/CCflk_plusBathy.csv',na.strings=c(""," "))
# CC=read.csv('Flowshed_Modeling_InputData/CC_complete_cases_26april2024.csv')

#subset the carb chem data to represent the years we want and a subset of SiteIDs
CCyr = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')) #subset of chemistry samples that match flowsheds
## assign visitID columns to allow us to pair CC to the spatial polygon data:
CCyr$visitID_ch1 =  paste(CCyr$SiteID,CCyr$UTCDate_Time)
CCyr$visitID_ch2 =    gsub(" ", "_" , CCyr$visitID_ch1, perl=TRUE)
CCyr$visitID =  gsub("[: -]", "" , CCyr$visitID_ch2, perl=TRUE)

plotCCyr = st_as_sf(CCyr, coords = c("Longitude","Latitude"), crs = st_crs(4326))
# mapview(plotCCyr,zcol="SiteID")

#Simplify CCyr to just the columns required for plotting: 
CCyr.s = CCyr[c('visitID','SiteID','Latitude', 'Longitude','Year','Sub_region','Zone','Season','Month','MY','MoY','pointDepth','Precipitation','dec.lon','dec.lat')]

CCbows=NULL
CCbowsALL=NULL
CCbowsALL = left_join(yearBows, CCyr.s, by="visitID") 
CCbows=subset(CCbowsALL, simu=='backward' & Year==2019 & SiteID=="UK_MID") 

concave=CCbows
concave_utm17 = st_transform(concave, 26917)

#buffer (extend) out and in
# D=1/111.111*.5 #degrees per km
D=300
concave.bo = st_buffer(concave_utm17, dist=D, nQuadSegs=30)   
# dim(concave.bo)
concave.bi = st_buffer(concave.bo, dist=-D, nQuadSegs=30)
# dim(concave.bi)
plotData = concave.bi
# dim(plotData)
plotData_t = st_transform(plotData,crs='+proj=longlat +datum=WGS84')



##### Define appropriate benthic habitat for Figure 5
coral_sf =  st_read(dsn = "Flowshed_Modeling_InputData/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer="UnifiedReefMap")

plotMe = subset(plotData_t,MY=="2019-07")

## habitat for those flowsheds (July 2019 intersecting habitat)
coral_sfC.t = st_transform(coral_sf,st_crs(plotMe))
coral_sfC.tv = coral_sfC.t[which(st_is_valid(coral_sfC.t)),]
over_bowC = st_intersection(coral_sfC.tv,plotMe)
dim(over_bowC)

# #buffer further out
# bowBoxE = st_bbox(plotMe) %>% st_as_sfc(st_crs=4326) %>% st_sf %>% st_cast
# bowBoxE.bo=st_buffer(bowBoxE, dist=2000) 
# over_boxE <- st_intersection(coral_sfC.tv,bowBoxE.bo)
# crs(coral_sfC.tv)==crs(bowBoxE.bo)

##___________________________ colors, fill, shapes

# unique(coral_sf$ClassLv1)
# length(unique(coral_sf$ClassLv1))
habs1 = c("Aggregate Reef","Individual or Aggregated Patch Reef",
          "Reef Rubble","Ridge",
          "Pavement", 
          "Scattered Coral/Rock in Unconsolidated Sediment","Unconsolidated Sediment", 
          "Seagrass (Continuous)","Seagrass (Discontinuous)",
          "Mangrove",                        
          "Land","Artificial","Dredged/Excavated",
          "Not Classified") 

class1_colors = c('maroon1','red',
                  'blueviolet','mediumturquoise',
                  'steelblue1',
                  'coral','khaki',
                  'darkgreen','forestgreen',
                  'saddlebrown',
                  "gray50",'gray50','navyblue', #use white for unclassified OR classes that are not habitat
                  'white')

class1_FILL_scale = scale_fill_manual(name = "Class 1 Habitat", values = class1_colors, breaks = habs1,labels = habs1)
class1_COLOR_scale = scale_color_manual(name = "Class 1 Habitat", values = class1_colors, breaks = habs1,labels = habs1)


nday_lines     <-c("dashed","dotted","solid","twodash")
nday_LINE_scale  <- scale_linetype_manual(name = "Number of days", values = nday_lines,
                                           breaks = c('1','3','5','7'),
                                           labels = c('1','3','5','7'))

##___________________________
plot5 = subset(plotData_t,n_days %in% c(1,3,5,7) & MY=="2019-07")

Fig5a=ggplot(data = FLKs1) +
  geom_sf(data=coral_sfC.tv,aes(fill=ClassLv1),lwd=0,alpha=.7)+
  # geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+
  geom_sf(fill = "gray50", lwd = 0) +  
  class1_FILL_scale+
  coord_sf(xlim = c(-82.1, -80.1), ylim = c(24.48, 25.7), expand = FALSE) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_rect(aes(xmin=st_bbox(plot5)[1],xmax=st_bbox(plot5)[3],ymin=st_bbox(plot5)[2],ymax=st_bbox(plot5)[4]), color='black',lwd=.9,fill=NA)+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14))+
  ylab('Latitude')+
  xlab('Longitude')+
  theme(legend.position="none")
Fig5a
# ggsave(filename=paste0("Flowshed_Modeling_Figures/Figure5a.png"),plot=Fig5a,width=10, height=10, dpi = 300)


## Inset for where 5a is relative to state of FL:
FL=ggplot(data = FLKs1) +
  geom_sf(fill = "gray50", lwd = 0) +  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank())+
  geom_rect(aes(xmin=-82.1,xmax=-80.1,ymin=24.48,ymax= 25.7),color='darkred',lwd=.8,fill=NA,inherit.aes = FALSE) 
FL
# ggsave(filename=paste0("Flowshed_Modeling_Figures/Fig5aBox.png"),plot=FL,width=10, height=10, dpi = 300)


Fig5b = ggplot(FLKs1)+
  geom_sf(data=coral_sfC.tv,aes(fill=ClassLv1),lwd=0,alpha=.7)+
  geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+
  geom_sf(fill = "gray50", lwd = 0) +
  geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+
  geom_sf(data=plot5,aes(linetype=as.factor(n_days)),color='black',inherit.aes = FALSE,alpha=0,lwd=1.2)+
  nday_LINE_scale +
  geom_star(aes(x=Longitude,y=Latitude),fill='black',color="black",data=plotMe,size=6,starstroke=.5)+
  class1_FILL_scale+
  guides(linetype=FALSE)+
  theme_bw()+
  scale_x_continuous(limits=st_bbox(plot5)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(plot5)[c(2,4)])+
  ylab('Latitude')+
  xlab('Longitude')+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14))+ #,strip.text= element_text(size=12))
  theme(legend.position="none")
Fig5b
# ggsave(filename="Flowshed_Modeling_Figures/Figure5b.png",plot=Fig5b,width=10, height=10, dpi = 300)


##simple plot to get line types
nday_lines     <-c("dashed","dotted","solid","twodash")
nday_LINE_scale  <- scale_linetype_manual(name = "Number of days", values = nday_lines,breaks = c('1','3','5','7'),labels = c('1','3','5','7'))

#fix order!
x=c(1,2,3,4,5,6,7,8,9,10)
y1=c(1,1,1,1,1,1,1,1,1,1)
y3=c(3,3,3,3,3,3,3,3,3,3)
y5=c(5,5,5,5,5,5,5,5,5,5)
y7=c(7,7,7,7,7,7,7,7,7,7)
df=data.frame(x,y1,y3,y5,y7)

df$nd = NULL
for (i in 1:length(df$x)){
  df
}

line = ggplot(df)+
  geom_line(aes(x,y7),linetype='dotdash',lwd=.77)+
  geom_line(aes(x,y5),linetype='dotted',lwd=.77)+
  geom_line(aes(x,y3),linetype='solid',lwd=.77)+
  geom_line(aes(x,y1),linetype='twodash',lwd=.77)+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylim(0,30)+
  xlim(1,20)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
line


# ## Map of the Keys with habitat key and sampling points
# keys2=ggplot(data = FLKs1) +
#   geom_sf(data=coral_sfC.tv,aes(fill=ClassLv1),lwd=0,alpha=.7)+
#   # geom_sf(data=coral_sfC.tv,aes(fill=ClassLv1),lwd=0,alpha=.7)+
#   geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=1)+
#   geom_sf(fill = "gray50", lwd = 0) +
#   class1_FILL_scale+
#   coord_sf(xlim = c(-83, -80), ylim = c(24.3, 26.5), expand = FALSE) +
#   theme_bw()+
#   theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
#   # geom_rect(aes(xmin=st_bbox(plot5)[1],xmax=st_bbox(plot5)[3],ymin=st_bbox(plot5)[2],ymax=st_bbox(plot5)[4]), color='black',lwd=.9,fill=NA)+
#   theme(axis.text=element_text(size=12),axis.title=element_text(size=14))+
#   ylab('Latitude')+
#   xlab('Longitude')
#   # theme(legend.position="none")
# keys2
# # ggsave(filename=paste0("Flowshed_Modeling_Figures/Keys_",Sys.time(),".png"),plot=keys2,width=10, height=10, dpi = 300)
