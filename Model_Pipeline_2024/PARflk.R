#get point PAR for FLK (ultimately want average PAR over each bow sector)

#this is where we write: CCflk_plusBathy.csv

# 10/5/2023 griddap loop is not running


## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('sf','ggplot2','tidyverse','ggmap','ggrepel','rstudioapi','stringr','sp','raster',
                 'patchwork','mapview','lubridate','rerddap')
# packageload <- c('sf','ggplot2','tidyverse','ggmap','ggrepel','rstudioapi','stringr','sp','rgdal','raster','rgeos',
#                  'patchwork','mapview','lubridate','rerddap')

lapply(packageload, library, character.only = TRUE)

#package loading cherck not working

# # ----Check which packages we actually use----
# # Find which packages do used functions belong to ----
# used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/Flowshed_Modeling/Model_Pipeline_2024/PARflk.R", alphabetic = FALSE) |> print()
# # Find which loaded packages are not used ----
# used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
# unused.packages <- packageload[!(packageload %in% used.packages)] |> print()




# install.packages("remotes")
# remotes::install_github("USGS-R/streamMetabolizer")
# In summer or fall 2023, this package will move from
# https://github.com/USGS-R/streamMetabolizer to
# https://github.com/DOI-USGS/streamMetabolizer.
# Please update your links accordingly.

library(streamMetabolizer) #needed for convert_UTC_to_solartime function


#___________________________#
CCflk <- read.csv('/Users/heidi.k.hirsh/Desktop/Hirsh_FLKmodel_InputFiles/FLK_filtered_ve4.csv')

# Get hour of day for each sample --------------------------------------------------
class(CCflk$UTCDate_Time) #character  
CCflk$UTCDate_Time[1] # "2010-03-08 12:26:00"
CCflk$TIMESTAMP_UTC <- ymd_hms(CCflk$UTCDate_Time) 
class(CCflk$TIMESTAMP_UTC)  #"POSIXct" "POSIXt" 
CCflk$TIMESTAMP_UTC[1] # "2010-03-08 12:26:00 UTC"
CCflk$TIMESTAMP_LST <- convert_UTC_to_solartime(date.time = CCflk$TIMESTAMP_UTC,longitude = CCflk$Longitude,time.type = "apparent solar") 
CCflk$TIMESTAMP_LST[1] #"2010-03-08 06:54:41 UTC"
CCflk$ESTTime[1] #shouldn't this be local (EST) time? ("07:26:00")
CCflk$jday.utc = yday(CCflk$TIMESTAMP_UTC) 
CCflk$jday.lst = yday(CCflk$TIMESTAMP_LST)
CCflk$hrod.lst = hour(CCflk$TIMESTAMP_LST)+minute(CCflk$TIMESTAMP_LST)/60+second(CCflk$TIMESTAMP_LST)/3600
CCflk$hrod[1] #6.911396
#can I use CCflk$ToD instead? 
CCflk$ToD[1] #7.433333


#Remove rows where coordinates are NA: 
#762 samples lack coordinates
which(is.na(CCflk$Latitude))
which(is.na(CCflk$Longitude))
CCflk %>% drop_na(Latitude)


# Add satellite PAR data --------------------------------------------------
CCflk$PAR_MODIS_DAILY=NA
CCflk$PAR_MODIS_8DAY=NA
CCflk$PAR_MODIS_MON=NA
plat=CCflk$Latitude
plon=CCflk$Longitude
ptime=as.POSIXct(CCflk$TIMESTAMP_UTC)

i=1

#need to be online

for(i in 763:length(ptime)){
# for(i in 1:length(ptime)){
  par_g<-griddap("erdMH1par01day",
                 time=c(ptime[i],ptime[i]),
                 latitude=c(plat[i],plat[i]),
                 longitude=c(plon[i],plon[i]))
  CCflk$PAR_MODIS_DAILY[i]=par_g$data$par
  
  par_g8<-griddap("erdMH1par08day",
                  time=c(ptime[i],ptime[i]),
                  latitude=c(plat[i],plat[i]),
                  longitude=c(plon[i],plon[i]))
  CCflk$PAR_MODIS_8DAY[i]=par_g8$data$par
  
  par_gmon<-griddap("erdMH1par0mday",
                    time=c(ptime[i],ptime[i]),
                    latitude=c(plat[i],plat[i]),
                    longitude=c(plon[i],plon[i]))
  CCflk$PAR_MODIS_MON[i]=par_gmon$data$par
  
  print(i)
  print(t(CCflk[,c("PAR_MODIS_DAILY","PAR_MODIS_8DAY","PAR_MODIS_MON")]))
}

CCpar=CCflk

# ggplot(CCflk,aes(y=PAR_MODIS_MON,PAR_MODIS_8DAY))+geom_point()+facet_wrap(.~is.na(PAR_MODIS_DAILY))
ggplot(CCflk,aes(y=DIC_umol_kg,x=PAR_MODIS_8DAY))+geom_point()+facet_wrap(.~is.na(PAR_MODIS_DAILY))

#WRITE out par file: 
# write.csv(CCpar, file='/Users/heidihirsh/Desktop/CCflk_plusPAR_full.csv') 




#Pull depth below samples (match bathymetry minus depth of sample)
CCpar= read.csv('/Users/heidi.k.hirsh/Desktop/Hirsh_FLK/CCflk_plusPAR_full.csv')
unique(CCpar$Sample_Depth_m) #all surface samples (all zero)

#convert CCpar into spatial points dataframe
# dim(CCpar) # 1612   60

which(is.na(CCpar$Latitude)) #762
# CCpar %>% drop_na(Latitude) #this didn't do anything
CCpar = CCpar[!is.na(CCpar$Latitude),]
# dim(CCpar) #1611   60 #dropped one sample
# which(is.na(CCpar$Latitude)) #none

#copy Lat and Lon columns otherwise they will be lost when we turn it into an sf dataframe
CCpar$LonCoord = CCpar$Longitude
CCpar$LatCoord = CCpar$Latitude
# dim(CCpar) #1611   62

bathy_sf=st_read(dsn = "/Users/heidi.k.hirsh/Desktop/FLK_data/mesh_florida",layer="mesh_florida")

CCpar_sf =st_as_sf(CCpar, coords = c("LonCoord", "LatCoord")) %>% st_set_crs(st_crs(bathy_sf))
# dim(CCpar_sf) #1611   61
# class(CCpar_sf) # "sf"         "data.frame"
CCpar_bathy= st_join(CCpar_sf,bathy_sf)
# dim(CCpar_bathy) #1611   63
# View(CCpar_bathy)

mapview(CCpar_bathy,zcol="bathymetry")

#we now have a bathymetry column which is the depth of the water column where the sample was collected. 
#remove geometry to save it as a df for the next step

CCout = CCpar_bathy %>% st_drop_geometry()
colnames(CCout)[colnames(CCout) == 'bathymetry'] <- 'pointDepth'


# View(CCout)

#write out new par file with depth: 
# write.csv(CCout, file='/Users/heidihirsh/Desktop/CCflk_plusBathy.csv')




#limit to 2016 data
CCpar_bathy_16 =CCpar_bathy[which(CCpar_bathy$Year=='2016'),]
mapview(CCpar_bathy_16,zcol="ESTDate")

#keys sampling spread out over multiple days
explore =CCpar_bathy_16[which(CCpar_bathy_16$UTCDate %in% c("16-11-14","16-11-15","16-11-18")),]
explore =CCpar_bathy_16[which(CCpar_bathy_16$UTCDate %in% c("16-09-19","16-09-20","16-09-23")),]
explore =CCpar_bathy_16[which(CCpar_bathy_16$UTCDate %in% c("16-01-04","16-01-05")),]
# unique(CCpar_bathy_16$UTCDate) #"16-01-04" "16-01-05" | "16-03-14" "16-03-15" | "16-05-09" "16-05-10" | "16-07-25" "16-07-26" | "16-09-19" "16-09-20" "16-09-23" | "16-11-14" "16-11-15" "16-11-18"
mapview(CCpar_bathy_16,zcol="ESTDate")
mapview(explore,zcol="Zone")
mapview(explore,zcol="ESTDate")

