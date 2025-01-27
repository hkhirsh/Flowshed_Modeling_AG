## current version of script for Fig 5 (27nov2024)



## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('sf','ggplot2','raster','mapview','ggplot2','ggstar','cowplot')
# packageload <- c('PNWColors','paletteer','colorspace','RColorBrewer','ghibli','htmlwidgets',
# 'sf','hddtools','magick','gtools','ggplot2','tidyverse','ggmap','ggrepel','rstudioapi','stringr',
# 'sp','raster','mapview','leaflet','dplyr','ggplot2','ggstar','webshot','cowplot','wesanderson','pals','Polychrome','rcartocolor')
lapply(packageload, library, character.only = TRUE)

# ----Check which packages we actually use----
# Find which packages do used functions belong to ----
used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/Flowshed_Modeling/Manuscript_Figures_2024/Figure5_Habitat.R", alphabetic = FALSE) |> print()
# Find which loaded packages are not used ----
used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
unused.packages <- packageload[!(packageload %in% used.packages)] |> print()
--------------------------------------------
  
  
  

#Read in yearBows (if not already loaded from running the section above)
shp.list = list.files(path = paste0("/Users/heidi.k.hirsh/Documents/FLK_Model1/yearBows_22april2024"), pattern="*shp", full.names = T) 
shapefile_list = lapply(shp.list,read_sf)

all.yearBows = rbind(shapefile_list[[1]],shapefile_list[[2]],shapefile_list[[3]],shapefile_list[[4]])
yearBows=all.yearBows

CC = read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/CCflk_plusBathy.csv',na.strings=c(""," "))

#subset the carb chem data to represent the years we want and a subset of SiteIDs
CCyr = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')) #subset of chemistry samples that match bow ties:
## assign visitID columns to allow us to pair CC to the spatial polygon data:
CCyr$visitID_ch1 =  paste(CCyr$SiteID,CCyr$UTCDate_Time)
CCyr$visitID_ch2 =    gsub(" ", "_" , CCyr$visitID_ch1, perl=TRUE)
CCyr$visitID =  gsub("[: -]", "" , CCyr$visitID_ch2, perl=TRUE)

plotCCyr = st_as_sf(CCyr, coords = c("Longitude","Latitude"), crs = st_crs(4326))
mapview(plotCCyr,zcol="SiteID")

#Simplify CCyr to just the columns required for plotting: 
CCyr.s = CCyr[c('visitID','SiteID','Latitude', 'Longitude','Year','Sub_region','Zone','Season','Month','MY','MoY','pointDepth','Precipitation','dec.lon','dec.lat')]

CCbows=NULL
CCbowsALL=NULL
CCbowsALL = left_join(yearBows, CCyr.s, by="visitID") 
CCbows=subset(CCbowsALL, simu=='backward' & Year==2019 & SiteID=="UK_MID") 
dim(CCbows)

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



################## Define appropriate benthic habitat
FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')
# habitat = read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/exBio.csv') #pretty sure I never actually use this
coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer="UnifiedReefMap")


#define plot_a to plot_d here: 
plotMe = subset(plotData_t,MY=="2019-07")

plot_hab=plotData_t
dim(plot_hab)

#Tranform coral_sf to match Bbow CRS:
coral_sfH.t = st_transform(coral_sf,st_crs(plot_hab))
coral_sfH.tv = coral_sfH.t[which(st_is_valid(coral_sfH.t)),]
dim(coral_sfH.tv)

#habitat for those flowsheds
#July 2019 intersecting habitat
coral_sfC.t = st_transform(coral_sf,st_crs(plotMe))
coral_sfC.tv = coral_sfC.t[which(st_is_valid(coral_sfC.t)),]
over_bowC = st_intersection(coral_sfC.tv,plotMe)
dim(over_bowC)

#buffer further out
bowBoxE = st_bbox(plotMe) %>% st_as_sfc(st_crs=4326) %>% st_sf %>% st_cast
bowBoxE.bo=st_buffer(bowBoxE, dist=2000) 
over_boxE <- st_intersection(coral_sfC.tv,bowBoxE.bo)
crs(coral_sfC.tv)==crs(bowBoxE.bo)


unique(coral_sf$ClassLv1)
length(unique(coral_sf$ClassLv1))

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

habSites = unique(plotData_t$SiteID)

unique(plotData_t$n_days)

nday_lines     <-c("dashed","dotted","solid","twodash")
nday_LINE_scale  <- scale_linetype_manual(name = "Number of days", values = nday_lines,
                                           breaks = c('1','3','5','7'),
                                           labels = c('1','3','5','7'))


plotA = subset(plotData_t,n_days %in% c(1,3,5,7) & MY=="2019-07")
plotA
dim(plotA)

HAB = ggplot(FLKs1)+
    geom_sf(data=coral_sfC.tv,aes(fill=ClassLv1),lwd=0,alpha=.7)+
    # geom_sf(data=coral_sfC.tv,aes(fill=ClassLvNew),lwd=0,alpha=.7)+
    geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+
    geom_sf(fill = "gray50", lwd = 0) +
    geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+
    geom_sf(data=plotA,aes(linetype=as.factor(n_days)),color='black',inherit.aes = FALSE,alpha=0,lwd=1.2)+
    nday_LINE_scale +
    geom_star(aes(x=Longitude,y=Latitude),fill='black',color="black",data=plotMe,size=6,starstroke=.5)+
    class1_FILL_scale+
    # classN_FILL_scale+
    guides(linetype=FALSE)+
    theme_bw()+
    scale_x_continuous(limits=st_bbox(plotA)[c(1,3)])+
    scale_y_continuous(limits=st_bbox(plotA)[c(2,4)])+
    ylab('Latitude')+
    xlab('Longitude')+
    # ggtitle(paste0("July 2019; 1,4,7-day; Site=",habSites[1], " Zone=",plotA$Zone))+
    theme(axis.line = element_line(color='black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    theme(axis.text=element_text(size=12),axis.title=element_text(size=14))+ #,strip.text= element_text(size=12))
    # theme(legend.position=c(.81,.22))
    theme(legend.position="none")

  TS=Sys.time()
  HAB
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Figure5/Habitat_Site=",habSites[1],"_Class1Habitat_",TS,".png"),plot=HAB,width=10, height=10, dpi = 300)
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/code_cleanup_figs/Habitat_Site=",habSites[1],"_Class1Habitat_",TS,".png"),plot=HAB,width=10, height=10, dpi = 300)
  
  
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Figure5/Habitat_Site=",habSites[1],"_Class1Habitat_",TS,".pdf"),plot=HAB,width=10, height=10, dpi = 300)
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Figure5/Habitat_Site=",habSites[1],"_Class1Habitat_",TS,".svg"),plot=HAB,width=10, height=10, dpi = 300)
  

  
# keys=NULL
keys=ggplot(data = FLKs1) +
  # geom_sf(fill = "darkgray", lwd = 0) +  
  geom_sf(data=coral_sfC.tv,aes(fill=ClassLv1),lwd=0,alpha=.7)+
  geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+
  geom_sf(fill = "gray50", lwd = 0) +  
  class1_FILL_scale+
  coord_sf(xlim = c(-82.1, -80.1), ylim = c(24.48, 25.7), expand = FALSE) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
  geom_rect(aes(xmin=st_bbox(plotA)[1],xmax=st_bbox(plotA)[3],ymin=st_bbox(plotA)[2],ymax=st_bbox(plotA)[4]), color='black',lwd=.9,fill=NA)+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14))+
  ylab('Latitude')+
  xlab('Longitude')+
  theme(legend.position="none")
    
TS=Sys.time()
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/code_cleanup_figs/KeysInset_",TS,".png"),plot=keys,width=10, height=10, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Figure5/KeysInset_",TS,".png"),plot=keys,width=10, height=10, dpi = 300)

FL=ggplot(data = FLKs1) +
  geom_sf(fill = "gray50", lwd = 0) +  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank())+
  geom_rect(aes(xmin=-82.1,xmax=-80.1,ymin=24.48,ymax= 25.7),color='darkred',lwd=.8,fill=NA,inherit.aes = FALSE) 

stateIn = cowplot::ggdraw()+ 
  cowplot::draw_plot(keys, x = 0, y = 0, width = 1, height = 1)+
  cowplot::draw_plot(FL+theme(axis.text = element_blank(), axis.ticks = element_blank()), x=-0.095, y=0.265,width = .5, height =1,scale = .42)+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

TS=Sys.time()
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/code_cleanup_figs/KeysInset_",TS,".png"),plot=stateIn,width=10, height=10, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Figure5/KeysInset_",TS,".png"),plot=stateIn,width=10, height=10, dpi = 300)
  
# Fig5= HAB / stateIn
Fig5= stateIn / HAB
  
TS=Sys.time()
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/code_cleanup_figs/HabInset_",TS,".pdf"),plot=Fig5,width=10, height=10, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Figure5/HabInset_",TS,".pdf"),plot=Fig5,width=10, height=10, dpi = 300)
  
  

##shitty line plot to get line types

nday_lines     <-c("dashed","dotted","solid","twodash")
nday_LINE_scale  <- scale_linetype_manual(name = "Number of days", values = nday_lines,
                                          breaks = c('1','3','5','7'),
                                          labels = c('1','3','5','7'))

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
  # geom_line(aes(x,y1),linetype='dotdash',lwd=.77)+
  # geom_line(aes(x,y3),linetype='dotted',lwd=.77)+
  # geom_line(aes(x,y5),linetype='solid',lwd=.77)+
  # geom_line(aes(x,y7),linetype='twodash',lwd=.77)+
  
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
# 
TS=Sys.time()
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/line_",TS,".png"),plot=line,width=3, height=3, dpi = 300)


keys2=ggplot(data = FLKs1) +
  geom_sf(data=coral_sfC.tv,aes(fill=ClassLv1),lwd=0,alpha=.7)+
  # geom_sf(data=coral_sfC.tv,aes(fill=ClassLv1),lwd=0,alpha=.7)+
  geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=1)+
  geom_sf(fill = "gray50", lwd = 0) +
  class1_FILL_scale+
  coord_sf(xlim = c(-83, -80), ylim = c(24.3, 26.5), expand = FALSE) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  # geom_rect(aes(xmin=st_bbox(plotA)[1],xmax=st_bbox(plotA)[3],ymin=st_bbox(plotA)[2],ymax=st_bbox(plotA)[4]), color='black',lwd=.9,fill=NA)+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14))+
  ylab('Latitude')+
  xlab('Longitude')
  # theme(legend.position="none")
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/code_cleanup_figs/Keys_",Sys.time(),".png"),plot=keys2,width=10, height=10, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Figure5/Keys_",Sys.time(),".png"),plot=keys2,width=10, height=10, dpi = 300)
