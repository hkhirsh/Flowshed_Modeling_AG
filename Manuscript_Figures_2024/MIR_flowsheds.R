#Map out the flowsheds backward (and forward?) from the MIR sites, July 2023
#Feb11,2025

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('PNWColors','sf','ggplot2','mapview','cowplot','ggstar')
#'ggmap',
lapply(packageload, library, character.only = TRUE)

# # ----Check which packages we actually use----
# # Find which packages do used functions belong to ----
# used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/Flowshed_Modeling/Manuscript_Figures_2024/Figure4_Flowsheds.R", alphabetic = FALSE) |> print()
# # Find which loaded packages are not used ----
# used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
# unused.packages <- packageload[!(packageload %in% used.packages)] |> print()
# --------------------------------------------

FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')

### MIR Shapefiles aren't plotting right
# #bring in MIR shapefiles: 
# # MIR = read.csv('/Users/heidi.k.hirsh/Documents/GitHub/REEF-PERSISTENCE/NCRMP_MIRE/2022_AnalysisFiles/MIR_2022_benthic_cover.csv')
# 
# 
# mirPoly_shp = st_read('/Users/heidi.k.hirsh/Documents/MIR/remirdata/MIR Shp/MIR Site Polygons.shp')
# class(mirPoly_shp)
# mirPoly_sp = st_transform(mirPoly_shp,crs='+proj=longlat +datum=WGS84')
# class(mirPoly_sp)
# head(mirPoly_sp)
# mapview(mirPoly_sp)
# 
# mirMonitoring_shp <- st_read('/Users/heidi.k.hirsh/Documents/MIR/MIR Shapefiles/Monitoring Subplots/MonitoringSubplotCoordinates.shp') #, crs=st_crs(26717)) #utm zone 17R
# 
# mirOverall_shp<- st_read('/Users/heidi.k.hirsh/Documents/MIR/MIR Shapefiles/Overall Site/MIR Site Polygons.shp')    # "MIRSite"    "Aream2"     "Areakm2"    "Shape__Are" "Shape__Len"
# 
# #sub in other shapefiles from Katey
# mirContRest_shp <- st_read('/Users/heidi.k.hirsh/Documents/MIR/MIR Shapefiles/Control and Restoration Areas/MIR_CA_RMA.shp')      #"MIRSite"    "Aream2"     "Areakm2"    "Shape__Are" "Shape__Len"
# mapview(mirContRest_sp,zcol="ReefSite")
# 
# mirSegments_shp <- st_read('/Users/heidi.k.hirsh/Documents/MIR/MIR Shapefiles/Site Segments/MIR_Segments.shp')   #"SegmentId"  "Site"       "Site_Segme" "Shape__Are" "Shape__Len" "GlobalID"   "CreationDa" "Creator"    "EditDate"   "Editor"    
# mapview(mirSegments_sp,zcol="Site")



#BUFFER FLOWSHEDS
# MIRbows_b = st_read('/Users/heidi.k.hirsh/Desktop/MIRheat/bow_ties_mir/bow_ties_mir_backward/bow_ties_mir_backward.shp')
# # plot(MIRbows_b)
# head(MIRbows_b)
# # MIRbows_f = st_read('/Users/heidi.k.hirsh/Desktop/MIRheat/bow_ties_mir/bow_ties_mir_forward/bow_ties_mir_forward.shp')
# 
# 
# concave=MIRbows_b
# concave_utm17 = st_transform(concave, 26917)
# 
# #buffer (extend) out and in
# # D=1/111.111*.5 #degrees per km
# # D=300
# D=500
# concave.bo = st_buffer(concave_utm17, dist=D, nQuadSegs=30)
# dim(concave.bo)
# #Save it
# # save(concave.bo, file = "/Users/heidi.k.hirsh/Desktop/MIRheat/MIRbows_back_concave_bo_D500.RData")
# 
# # concave.bi = st_buffer(concave.bo, dist=-D, nQuadSegs=30)
load("/Users/heidi.k.hirsh/Desktop/MIRheat/MIRbows_back_concave_bi_D500.RData")

dim(concave.bi)
#Save it
# save(concave.bi, file = "/Users/heidi.k.hirsh/Desktop/MIRheat/MIRbows_back_concave_bi_D500.RData")


plotData = concave.bi
plotData_t = st_transform(plotData,crs='+proj=longlat +datum=WGS84')
# names(plotData_t)

plotData_t$source = factor(plotData_t$source, levels = c("Carysfort North",
                                                   "Carysfort South",
                                                   "Horseshoe",
                                                   "Cheeca",
                                                   "Sombrero",
                                                   "NFH",
                                                   "Looe",
                                                   "EDR"))
unique(plotData_t$source)

# nColor <- length(unique(plotData$source))
# clrs0 = rainbow(nColor)
# scales::show_col(clrs0)
# clrs = rainbow(nColor, start = 0, end =0.8)
# 
# #SIMPLE
# # site_colors <- safe_pal
# site_colors <- clrs0
# site_COLOR_scale  <- scale_color_manual(name = "Site", values = site_colors,
#                                         breaks = c('1','EK_IN','UK_IN','7','10','13','16','19','24'),
#                                         labels = c('1','EK_IN','UK_IN','7','10','13','16','19','24'))
# 
# site_FILL_scale  <- scale_fill_manual(name = "Site", values = site_colors,
#                                       breaks = c('1','EK_IN','UK_IN','7','10','13','16','19','24'),
#                                       labels = c('1','EK_IN','UK_IN','7','10','13','16','19','24'))


#colors for number of days: 
pnw=pnw_palette(name="Bay",n=14,type="continuous")

tp=1.7 #original 0.3 (transects and shelf)
sp=3 #original point size =1.5
st=1.2 #original stroke = 1
ss=5 #original star 3
sz=.8
ff=16

#subset to test plot
# unique(plotData_t$source)
#"Carysfort North" "Carysfort South" "Cheeca"          "Horseshoe"       "Sombrero"        "NFH"             "Looe"            "EDR"         
test = subset(plotData_t,source=='Carysfort North')
# test = subset(plotData_t,n_days==14)
# dim(plotData_t)
# dim(test)
mapview(test,zcol="n_days")
# mapview(test,zcol="source")
class(test)


## Loop through sites 
table(plotData_t$source)
table(plotData_t$n_days)
#### Build loop
s_i=NULL
sites=unique(plotData_t$source)

for (s_i in 1:length(sites)) {
  site=sites[s_i]
  ploThis = subset(plotData_t,source==site)
  print(paste0("Start site ",s_i," of ",length(sites)))
## Loop through n days

mir = ggplot(FLKs1)+
  geom_sf(fill = "darkgray", lwd = 0)+
  # geom_sf(data=subset(ploThis,n_days<6),aes(color=n_days),inherit.aes = FALSE,alpha=0.05,lwd=sz)+
  geom_sf(data=ploThis,aes(color=n_days),inherit.aes = FALSE,alpha=0.05,lwd=sz)+
  
  # coord_sf(xlim = c(-82.1, -80.05), ylim = c(24.4, 26.1), expand = FALSE) + 
  theme_bw()+
  # scale_color_gradientn(colours=pnw)+
  scale_color_gradientn(colours=pnw, breaks=c(1,7,14),labels=c('1 day','7 days','14 days'))+
  labs(x=NULL,y=NULL)+
  scale_x_continuous(limits=st_bbox(plotData_t)[c(1,3)])+
  scale_y_continuous(limits=st_bbox(plotData_t)[c(2,4)])+
  ggtitle(paste0("MIR Site: ",site))+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = ff), 
        # axis.text.y.right = element_text(size = ff),
        axis.text.y = element_text(size = ff))#+
  # theme(legend.position="none")
mir


# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/MIRheat/MIRbows_explore/MIRback_",site,"_alldays.png"),plot=mir,width=22, height=10, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/MIRheat/MIRbows_explore/MIRback_",site,"_5days.png"),plot=mir,width=22, height=10, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/MIRheat/MIRbows_explore/MIRback_",site,"_",nn,"days.png"),plot=mir,width=22, height=10, dpi = 300)

}


##LOOP THROUGH NUMBER OF DAYS AND PLOT COLOR OF SITE: 
n_i=NULL

for (n_i in 1:length(unique(plotData_t$n_days))) {
  nn=n_i
  ploThis = subset(plotData_t,n_days==nn)
  print(paste0("Start day ",n_i," of ",length(unique(plotData_t$n_days))))
  ## Loop through n days
  
  mir = ggplot(FLKs1)+
    geom_sf(fill = "darkgray", lwd = 0)+
    # geom_sf(data=subset(ploThis,n_days<6),aes(color=n_days),inherit.aes = FALSE,alpha=0.05,lwd=sz)+
    geom_sf(data=ploThis,aes(color=source,fill=source),inherit.aes = FALSE,alpha=0.05,lwd=sz)+
    # geom_sf(data=ploThis,aes(color=n_days),inherit.aes = FALSE,alpha=0.05,lwd=sz)+
    
    
    # coord_sf(xlim = c(-82.1, -80.05), ylim = c(24.4, 26.1), expand = FALSE) + 
    theme_bw()+
    # scale_color_gradientn(colours=pnw)+
    # scale_color_gradientn(colours=pnw, breaks=c(1,7,14),labels=c('1 day','7 days','14 days'))+
    labs(x=NULL,y=NULL)+
    scale_x_continuous(limits=st_bbox(plotData_t)[c(1,3)])+
    scale_y_continuous(limits=st_bbox(plotData_t)[c(2,4)])+
    ggtitle(paste0("All MIR sites, ",nn," days backward"))+
    theme(axis.line = element_line(color='black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = ff), 
          # axis.text.y.right = element_text(size = ff),
          axis.text.y = element_text(size = ff))#+
  # theme(legend.position="none")
  # mir
  
  ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/MIRheat/MIRbows_explore/MIRback_allSites_",nn,"days.png"),plot=mir,width=22, height=10, dpi = 300)
}



# INab=ggplot(data = FLKs1) +
#   # geom_sf(fill = "gray12", lwd = 0) +
#   # geom_sf(fill = "gray50", lwd = 0) +
#   geom_sf(fill = "darkgray", lwd = 0) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())+
#   theme( axis.text.x = element_blank(), 
#          axis.text.y = element_blank(), 
#          axis.ticks = element_blank())+
#   geom_rect(aes(xmin=-82.1,xmax=-79.95,ymin=24.4,ymax= 26.2),
#             color='darkred',lwd=.8,fill=NA) +
#   theme(panel.border = element_rect(colour = "black",fill=NA, linewidth=.9))+
#   # theme(panel.border = element_rect(colour = "black",fill=NA, size=.9))+
#   theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
#   # geom_rect(aes(xmin=Xn,xmax=Xx,ymin=Yn,ymax=Yx),color='red',linetype='dashed',fill=NA)
# INab


