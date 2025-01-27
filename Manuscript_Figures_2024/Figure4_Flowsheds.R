#MOST FINAL FIG 4 CODE - 2024nov27


# register_google(key = "AIzaSyBSmQdRPOV8usMAOEkkoH6MISzFjTe5AoE") #new api Feb 2024



## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('PNWColors','sf','ggplot2','ggmap','mapview','cowplot','ggstar')

lapply(packageload, library, character.only = TRUE)

# # ----Check which packages we actually use----
# # Find which packages do used functions belong to ----
# used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/Flowshed_Modeling/Manuscript_Figures_2024/Figure4_Flowsheds.R", alphabetic = FALSE) |> print()
# # Find which loaded packages are not used ----
# used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
# unused.packages <- packageload[!(packageload %in% used.packages)] |> print()
# --------------------------------------------





# #Read in yearBows (if not already loaded from running the section above)
# shp.list = list.files(path = paste0("/Users/heidi.k.hirsh/Documents/FLK_Model1/yearBows_22april2024"), pattern="*shp", full.names = T) 
# # length(shp.list)
# # shp.list
# shapefile_list = lapply(shp.list,read_sf)
# 
# all.yearBows = rbind(shapefile_list[[1]],shapefile_list[[2]],shapefile_list[[3]],shapefile_list[[4]])
# yearBows=all.yearBows

#Read in carbonate chemistry station data
CC = read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/CCflk_plusBathy.csv',na.strings=c(""," "))

#subset the carb chem data to represent the years we want and a subset of SiteIDs
CCyr = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')) #subset of chemistry samples that match bow ties:
## assign visitID columns to allow us to pair CC to the spatial polygon data:
CCyr$visitID_ch1 =  paste(CCyr$SiteID,CCyr$UTCDate_Time)
CCyr$visitID_ch2 =    gsub(" ", "_" , CCyr$visitID_ch1, perl=TRUE)
CCyr$visitID =  gsub("[: -]", "" , CCyr$visitID_ch2, perl=TRUE)

plotCCyr = st_as_sf(CCyr, coords = c("Longitude","Latitude"), crs = st_crs(4326))
mapview(plotCCyr,zcol="SiteID")

#How can I get a general point for each
names(plotCCyr)
mapview(plotCCyr,zcol="SiteID")


#Simplify CCyr to just the columns required for plotting: 
CCyr.s = CCyr[c('visitID','SiteID','Latitude', 'Longitude','dec.lat','dec.lon','Year','Sub_region','Zone','Season','Month','MY','MoY','pointDepth','Precipitation')]
# names(CCyr)

# CCbows=NULL
# CCbowsALL=NULL
# 
# CCbowsALL = left_join(yearBows, CCyr.s, by="visitID") 
# # class(CCbowsALL)  #"sf"         "data.frame"
# # dim(CCbowsALL)  #38640    73
# 
#   
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
# 
# concave.bi = st_buffer(concave.bo, dist=-D, nQuadSegs=30)


load("/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/inshore2019_concave_bi.RData")

dim(concave.bi)

plotData = concave.bi
dim(plotData)

dim(plotData)
plotData_t = st_transform(plotData,crs='+proj=longlat +datum=WGS84')



################## Define appropriate benthic habitat
FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')

#define plot_a to plot_d here: 
plot_a = subset(plotData_t,n_days %in% c(1,7,14) & MY=="2019-07")
plot_b = subset(plotData_t,n_days %in% c(1,7,14) & MY=="2019-09")
# plot_c = subset(plotData_t,SiteID==8 & MY=="2019-07") #try 16
# plot_d = subset(plotData_t,SiteID==8 & MY=="2019-09")
plot_c = subset(plotData_t,SiteID==7 & MY=="2019-07") #try 16
plot_d = subset(plotData_t,SiteID==7 & MY=="2019-09")
# plot_e = subset(plotData_t,SiteID==7 & MY=="2019-07" & n_days==7)
# plot_f = subset(plotData_t,SiteID==7 & MY=="2019-09" & n_days==7)


# #just plot A and B by siteID
# sites=unique(plot_a$SiteID)
# sites
# 
# #SIMPLE
# site_colors <- safe_pal
# site_COLOR_scale  <- scale_color_manual(name = "Site", values = site_colors,
#                                          breaks = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'),
#                                          labels = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'))

nColor <- length(unique(plot_a$SiteID))
# scales::show_col(carto_pal(nColor, "Safe"))
# safe_pal <- carto_pal(nColor, "Safe")

#!!Replace greens with standalone visible color!!
clrs0 = rainbow(nColor)
scales::show_col(clrs0)
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
PTS = read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/CC_dataframes/PTS.csv')
PTS$Lat=PTS$Latitude_Dec_Deg
PTS$Long=PTS$Longitude_Dec_Deg
PTS=st_as_sf(PTS, coords = c("Long","Lat"),crs = st_crs(4326)) 

#limit to shark river and middle cape
iwant = c("SR","MC")
PTSsub= subset(PTS,line_id %in% iwant)
PTSsub= subset(PTSsub,Latitude_Dec_Deg>25) #remove 2 points in the keys
PTSsub5=subset(PTSsub,bathymetry>5) #only use points where depth is greater than 5m
plotShelfa = subset(PTSsub5,Year_UTC==2018)
unique(plotShelfa$Year_UTC)
plotShelf = subset(plotShelfa,Month_UTC==5)
mapview(plotShelf)




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
  # scale_x_continuous(limits=st_bbox(plot_a)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(plot_a)[c(2,4)])+
  coord_sf(xlim = c(-82.1, -79.95), ylim = c(24.4, 26.2), expand = FALSE) + #for backward halos
  # coord_sf(xlim = c(-82.055, -80.092), ylim = c(24.49, 25.73), expand = FALSE) + #for backward halos
  # coord_sf(xlim = c(-82.05575, -80.09157), ylim = c(24.49353, 25.72), expand = FALSE) + #for backward halos
  # coord_sf(xlim = c(-82.3, -80), ylim = c(24.4, 26.2), expand = FALSE) + #for backward halos
  # ggtitle("July 2019")+
  theme_bw()+
  # theme(axis.line = element_line(color='black'),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       axis.title.x = element_blank())+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  # theme(plot.margin = unit(c(0, 0, 0, 5), "cm"))+
  theme(legend.position="none")
AA

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
  # coord_sf(xlim = c(-82.3, -80), ylim = c(24.4, 26.2), expand = FALSE) + #for backward halos
  coord_sf(xlim = c(-82.1, -79.95), ylim = c(24.4, 26.2), expand = FALSE) + #for backward halos
  # coord_sf(xlim = c(-82.055, -80.092), ylim = c(24.49, 25.73), expand = FALSE) + #for backward halos
  # scale_x_continuous(limits=st_bbox(plot_a)[c(1,3)])+
  # scale_y_continuous(limits=st_bbox(plot_a)[c(2,4)])+
  # ggtitle("September 2019")+
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  # theme(axis.line = element_line(color='black'),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.title.y= element_blank(),
  #       axis.text.y= element_blank())+
  # theme(plot.margin = unit(c(0, 0, 0,5), "cm"))+
  theme(legend.position="none")
BB

CC = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  geom_sf(data=subset(plot_c,simu=='backward'),aes(color=n_days),inherit.aes = FALSE,alpha=0)+
  scale_color_gradientn(colours=pnw, breaks=c(1,7,14),labels=c('1 day','7 days','14 days'))+
  geom_star(aes(x=Longitude,y=Latitude),color="black",data=plot_c,size=3,fill='black')+
  ylab('Latitude')+
  xlab('Longitude')+
  coord_sf(xlim = c(-81.2, -79.95), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
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
  coord_sf(xlim = c(-81.2, -79.95), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  # theme(axis.line = element_line(color='black'),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       axis.title.y= element_blank(),
  #       axis.text.y= element_blank())+
  # theme(plot.margin = unit(c(0, 0, 0, 5), "cm"))+
  theme(legend.position="none")


AA
BB
CC
DD

# AB = plot_grid(AA,BB,labels=c("A","B"),ncol=2,align="hv")
# AB = plot_grid(CC,DD,AA,BB,labels=c("A","B","C","D"),ncol=2,align="v") #only use v to align (or florida gets squished)

AB = plot_grid(CC,DD,AA,BB,ncol=2,align="v") #only use v to align (or florida gets squished)
# TS=Sys.time()
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Figure4/Figure4_",TS,".png"),plot=AB,width=10, height=10, dpi = 300)
AB



# coord_sf(xlim = c(-82.05575, -80.09157), ylim = c(24.49353, 25.72), expand = FALSE) + #for backward halos
  
INab=ggplot(data = FLKs1) +
  # geom_sf(fill = "gray12", lwd = 0) +
  # geom_sf(fill = "gray50", lwd = 0) +
  geom_sf(fill = "darkgray", lwd = 0) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme( axis.text.x = element_blank(), 
         axis.text.y = element_blank(), 
         axis.ticks = element_blank())+
  geom_rect(aes(xmin=-82.1,xmax=-79.95,ymin=24.4,ymax= 26.2),
            color='darkred',lwd=.8,fill=NA) +
  theme(panel.border = element_rect(colour = "black",fill=NA, linewidth=.9))+
  # theme(panel.border = element_rect(colour = "black",fill=NA, size=.9))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  # geom_rect(aes(xmin=Xn,xmax=Xx,ymin=Yn,ymax=Yx),color='red',linetype='dashed',fill=NA)
INab
# INcd=ggplot(data = FLKs1) +
#   # geom_sf(fill = "gray12", lwd = 0) +
#   # geom_sf(fill = "gray50", lwd = 0) +
#   geom_sf(fill = "darkgray", lwd = 0) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())+
#   theme( axis.text.x = element_blank(), 
#          axis.text.y = element_blank(), 
#          axis.ticks = element_blank())+
#   geom_rect(aes(xmin=-81.2,xmax=-80,ymin=24.75,ymax=25.6),
#             color='darkred',lwd=.6,fill=NA) +
#   theme(panel.border = element_rect(colour = "gray42", fill=NA, size=.7))+
#   theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
#   # theme_void()


# INabcd=ggplot(data = FLKs1) +
#   # geom_sf(fill = "gray12", lwd = 0) +
#   geom_sf(fill = "gray42", lwd = 0) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())+
#   theme( axis.text.x = element_blank(), 
#          axis.text.y = element_blank(), 
#          axis.ticks = element_blank())+
#   geom_rect(aes(xmin=-81.2,xmax=-80,ymin=24.75,ymax=25.6),
#             color='red',lwd=.5,fill=NA) +
#   geom_rect(aes(xmin=st_bbox(plot_a)[1],xmax=st_bbox(plot_a)[3],ymin=st_bbox(plot_a)[2],ymax=st_bbox(plot_a)[4]),
#             color='blue',lwd=.5,fill=NA) +
#   theme_void()
# theme(panel.border = element_rect(colour = "black", fill=NA, size=.4))


# TS=Sys.time()
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Figure4/FLinset_",TS,".png"),plot=INabcd,width=10, height=10, dpi = 300)


# FOCUS
# print(IN, vp = viewport(0.322, 0.859, width = 0.25, height = 0.25))
#I don't know if I used these afterall. I think I combined outside

F4a= cowplot::ggdraw()+
  cowplot::draw_plot(AA, x = 0, y = 0, width = 1, height = 1)+
  cowplot::draw_plot(INab+theme(axis.text = element_blank(), axis.ticks = element_blank()), x=0.014, y=0.3,width = .5, height =1,scale = .47)
# cowplot::draw_plot(INab+theme(axis.text = element_blank(), axis.ticks = element_blank()), x=-0.065, y=0.22,width = .5, height =1,scale = .45) #works outside multiplot

# cowplot::draw_plot(INab+theme(axis.text = element_blank(), axis.ticks = element_blank()), x=0, y=0,width = .6, height =1.2,scale = .45)
# TS=Sys.time()
# # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Figure4/Figure4ab_inset_",TS,".png"),plot=abcd,width=10, height=10, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Figure4/Figure4a_inset_",TS,".png"),plot=F4a,width=10, height=10, dpi = 300)




F4b= cowplot::ggdraw()+
  cowplot::draw_plot(BB, x = 0, y = 0, width = 1, height = 1)+
  cowplot::draw_plot(INab+theme(axis.text = element_blank(), axis.ticks = element_blank()), x=0.015, y=0.3,width = .5, height =1,scale = .47)
F4b


F4c= cowplot::ggdraw()+
  cowplot::draw_plot(CC, x = 0, y = 0, width = 1, height = 1)#+
  # cowplot::draw_plot(INcd+theme(axis.text = element_blank(), axis.ticks = element_blank()), x=.1, y=.2 ,width = .5, height =1,scale = .5)

F4d= cowplot::ggdraw()+
  cowplot::draw_plot(DD, x = 0, y = 0, width = 1, height = 1)#+
  # cowplot::draw_plot(INcd+theme(axis.text = element_blank(), axis.ticks = element_blank()), x=.1, y=.2 ,width = .5, height =1,scale = .5)


# abcd = plot_grid(F4c,F4d,F4a,F4b,ncol=2,align="v") #only use v to align (or florida gets squished)
patch = F4c+F4d+F4a+F4b+plot_layout(ncol=2)+plot_annotation(theme = theme(plot.margin = margin()))
patch
# p1 + p2 + plot_annotation(theme = theme(plot.margin = margin()))
# patch = CC+DD+F4a+F4b+plot_layout(ncol=2)
TS=Sys.time()
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Figure4/Figure4ab_inset_",TS,".png"),plot=patch,width=10, height=10, dpi = 300)

# 
# geom_point(data =  PTSsub5, aes(x = Longitude_Dec_Deg, y = Latitude_Dec_Deg,fill=line_name), pch=21,size = 4) +
#   wf_FILL_scale+




##### poster figures: 

#Split into 2 2-panels: 

tp=1.7 #original 0.3 (transects and shelf)
sp=3 #original point size =1.5
st=1.2 #original stroke = 1
ss=5 #original star 3
sz=.8
ff=16


AA = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  geom_sf(data=subset(plot_a,simu=='backward'),aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.05,lwd=sz)+
  site_COLOR_scale+
  site_FILL_scale+
  geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=tp)+     #all transects
  geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=tp)+
  geom_point(aes(x=Longitude,y=Latitude,fill=SiteID),color='black',data=subset(plot_a,SiteID!=7),pch=21,size=sp,stroke=st)+
  geom_star(aes(x=Longitude,y=Latitude),size = ss+2, color = "black",data=plot_c, fill = 'black') +
  geom_star(aes(x=Longitude,y=Latitude,fill=SiteID),color="black",data=plot_c,size=ss)+
  # scale_y_continuous(sec.axis = dup_axis(name = NULL)) +  # Add ticks to the right y-axis
  coord_sf(xlim = c(-82.1, -80.05), ylim = c(24.4, 26.1), expand = FALSE) + #for backward halos
  theme_bw()+
  labs(x=NULL,y=NULL)+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = ff), 
        # axis.text.y.right = element_text(size = ff),
        axis.text.y = element_text(size = ff))+
  theme(legend.position="none")
AA



BB = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  geom_sf(lwd=sz,data=plot_b,aes(color=SiteID,fill=SiteID),inherit.aes = FALSE,alpha=0.05)+
  site_COLOR_scale+
  site_FILL_scale+
  geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=tp)+
  geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=tp)+
  geom_point(aes(x=Longitude,y=Latitude,fill=SiteID),color='black',data=subset(plot_a,SiteID!=7),pch=21,size=sp,stroke=st)+
  geom_star(aes(x=Longitude,y=Latitude),size = ss+2, color = "black",data=plot_c, fill = 'black') +
  geom_star(aes(x=Longitude,y=Latitude,fill=SiteID),color="black",data=plot_c,size=ss)+
  coord_sf(xlim = c(-82.1, -80.05), ylim = c(24.4, 26.1), expand = FALSE) + #for backward halos
  theme_bw()+
  labs(x=NULL,y=NULL)+
  # scale_y_continuous(sec.axis = dup_axis(name = NULL)) +  # Add ticks to the right y-axis
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(size = ff),  
        axis.text.y = element_text(size = ff),
        panel.grid.minor = element_blank())+
  # theme(legend.position="bottom",legend.direction="horizontal",legend.box = "horizontal",legend.justification="left")+
  theme(legend.position="right",legend.direction="vertical")+
      theme(legend.text = element_text(size = 16),legend.title = element_text(size = 18))
# theme(legend.position="right",legend.direction="vertical")
BB


AB = AA+BB 
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Poster_Figure4ab_13.png"),plot=AB,width=22, height=10, dpi = 300)

# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Poster_Figure4ab_A.png"),plot=AA,width=22, height=10, dpi = 300)
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Poster_Figure4ab_Br2.png"),plot=BB,width=22, height=10, dpi = 300)



### Stack C and D (vertical)
CC = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  geom_sf(data=subset(plot_c,simu=='backward'),aes(color=n_days),inherit.aes = FALSE,alpha=0,lwd=0.6)+
  scale_color_gradientn(colours=pnw, breaks=c(1,7,14),labels=c('1 day','7 days','14 days'))+
  geom_star(aes(x=Longitude,y=Latitude),color="black",data=plot_c,size=4,fill='black')+
  # ylab('Latitude')+
  xlab('  ')+
  coord_sf(xlim = c(-81.22, -80.05), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        # axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        # axis.title.x = element_blank(),  # Remove x-axis title
        # axis.title.x = "", 
        axis.text.x = element_blank(),   # Remove x-axis tick labels
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position=c(0.87,0.21),legend.title=element_blank(),
              legend.text = element_text(size = 13))
# CC
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Poster_Figure4cc.png"),plot=CC,width=22, height=10, dpi = 300)


DD = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  geom_sf(data=subset(plot_d,simu=='backward'),aes(color=n_days),inherit.aes = FALSE,alpha=0,lwd=.6)+
  scale_color_gradientn(colours=pnw, breaks=c(1,7,14),labels=c('1 day','7 days','14 days'))+
  geom_star(aes(x=Longitude,y=Latitude),color="black",data=plot_c,size=4,fill='black')+
  # ylab('Latitude')+
  # xlab('Longitude')+
  coord_sf(xlim = c(-81.22, -80.05), ylim = c(24.75, 25.6), expand = FALSE) + #for backward halos
  theme_bw()+
  theme(axis.line = element_line(color='black'),
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),  # Remove x-axis title
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position="none")
# DD
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Poster_Figure4dd.png"),plot=DD,width=22, height=10, dpi = 300)

CD=CC/DD
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/Poster_Figure4cd_5.png"),plot=CD,width=6, height=10, dpi = 300)
