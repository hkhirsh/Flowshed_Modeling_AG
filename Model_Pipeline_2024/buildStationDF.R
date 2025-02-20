##This script combines separate datasets into a combined station data dataframe so that all discrete sample data is in one place
#Load carbonate chemistry data from the Florida Keys
#Add nutrient data from samples taken at the same stations
#Add PAR data from MODIS satellite data via ERDDAP
#Add bathymetry data from the SLIM2D model mesh grid

##Heidi K. Hirsh
##Last edit: Feb 20, 2025

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('mapview','sf','stringr','dplyr','lubridate','ggplot2','patchwork','streamMetabolizer','rerddap')
# packageload <- c('magick','ggplot2','tidyverse','ggrepel','rstudioapi','stringr','sp','raster','patchwork','mapview','rerddap','leaflet','streamMetabolizer','sf','ggmap')
lapply(packageload, library, character.only = TRUE)


## LOAD Files:

## Load filtered carbonate chemistry data ------------------------------------
CC <- read.csv('Flowshed_Modeling_InputData/FLK_filtered_ve4.csv')
# dim(CC) #1612

## Load nutrient data --------------------------------------------------------
Nuts_original <- read.csv('Flowshed_Modeling_InputData/WS_Keys_Data_1998-2022_hh.csv')
# dim(Nuts_original) # 3739  

## Load bathymetry layer (from SLIM 2D model)
bathy_sf <- st_read(dsn = "Flowshed_Modeling_InputData//mesh_florida",layer="mesh_florida")

#----------------------------------------------------------

CC$jday= yday(CC$datetime)
CC$UTCDate_Time = as.POSIXct(CC$UTCDate_Time, tz="UTC")

## there is one point that does not have coordinates
# dim(subset(CC,is.na(CC$Longitude)))
# dim(subset(CC,is.na(CC$Latitude)))
## remove this point
CC=subset(CC,!is.na(CC$Latitude) & !is.na(CC$Longitude))
# dim(CC)

## What is the temporal coverage of data?
# temporal = ggplot(data=CC,aes(x=jday,y=DIC_umol_kg,color=as.factor(Year)))+
#   geom_point()+
#   facet_grid("Year")+
#   theme_bw()+
#   theme(legend.position="bottom")+
#   scale_x_continuous(breaks = seq(0, 365, by = 10))
# TS=Sys.time()
# temporal

## Add visitID (this will be use to match discrete carbonate chemistry to other paired data (nutrients, water mass history, etc.)
visitID_1 =  paste(CC$SiteID,CC$UTCDate_Time)
visitID_2 = gsub(" ", "_" , visitID_1, perl=TRUE)
CC$visitID =  gsub("[: -]", "" , visitID_2, perl=TRUE)


CC.sf = st_as_sf(CC, coords = c("Longitude","Latitude"), crs = st_crs(4326))  
# mapview(CC.sf,zcol='Zone')


## Rename nutrient columns to avoid confusion when comparing to carbonate chemistry coordinates
## Note, coordinates should be the same sites
Nuts= Nuts_original %>% dplyr::rename('Latitude.n'='Latitude', 'Longitude.n'='Longitude', 'Date.n'='Date')

## Name new coordinate columns so that this information is not lost when making it sf 
Nuts$Longitude = Nuts$Longitude.n
Nuts$Latitude = Nuts$Latitude.n
Nuts.sf = st_as_sf(Nuts, coords = c("Longitude","Latitude"), crs = st_crs(4326))  
# mapview(Nuts.sf,zcol='Station')

## We do not need the point on the West Florida Shelf.
# mapview(Nuts.sf,zcol="Station")

## That point is labeled station 19, but should not be grouped with that station
pesky = subset(Nuts.sf,Station %in% c('19')) 
# mapview(pesky,zcol="Station")
## The the "pesky" northern point will be identified later by visitID and removed

## Be consistent with station naming to match carbonate chemistry dataframe
Nuts.sf$Station[which(Nuts.sf$Station == "21")] = "21LK"

## Remove station KW1 (north of Key West)
Nuts.sf = Nuts.sf[which(Nuts$Station != "KW1"),] 

## Create the matching visitIDs for the nutrient data
## Nutrients data date formatting is mildly infuriating so we are going to fix that:
# Nuts.sf$Date.n    
# Nuts.sf$Date[16] #"1/30/98"
Nuts.sf$Date.yyyymmdd = as.Date(Nuts.sf$Date.n, format= "%m/%d/%y")
# Nuts.sf$Date.yyyymmdd[16] # "1998-01-30"

## Pair date and time
Nuts.sf$dateTime = str_c(Nuts.sf$Date.yyyymmdd, ' ', Nuts.sf$GMT)
Nuts.sf$UTCDate_Time = as.POSIXct(Nuts.sf$dateTime, tz="UTC") 

visitID_n1 = paste(Nuts.sf$Station,Nuts.sf$UTCDate_Time) #combine station name and datetime
visitID_n2 = gsub(" ", "_" ,visitID_n1, perl=TRUE) #separate station, date, and time by "_"
Nuts.sf$visitID =  gsub("[: -]", "" , visitID_n2, perl=TRUE) #remove all characters except "_"

 
## Now, we can remove the extra "19" northern point
## The pesky point is visitID = 19_20160728_013600
## Save Nuts without this point
Nuts.sf = Nuts.sf[which(Nuts.sf$visitID !='19_20160728_013600'),]  
# mapview(Nuts.sf) #gone!

##limit nutrients to same years as CC
Nuts.sf$Year = format(Nuts.sf$UTCDate_Time, format="%Y")
Nuts.sf$Year = as.integer(Nuts.sf$Year) 
CCyears = unique(CC$Year) 
Nuts.sf = subset(Nuts.sf,Year %in% CCyears)


## Which visitIDs in CC do not have matching nutrients?
noMatch = CC[which(is.na(match(CC$visitID,Nuts.sf$visitID))),]
# noMatch #one sample in CC is missing nutrients
# length(unique(noMatch$visitID))
# unique(noMatch$visitID) # "21LK_20201213_082700" #only one missing?

##remove that sample before merging with nutrients (Nuts.sf)
# dim(CC) #1611   53
# test = subset(CC, visitID==unique(noMatch$visitID))
CC=subset(CC, !visitID==unique(noMatch$visitID))
# dim(CC) #1610   53

CCnuts = left_join(CC, Nuts.sf, by=c("visitID","Year"))
# names(CCnuts)
# dim(CCnuts) #1610 


## Check that CC and nutrient coordinates match: (they do)
# ggplot()+
#   geom_point(data=CCnuts,aes(x=Latitude.n,y=Longitude.n),size=3)+
#   geom_point(data=CCnuts,aes(x=Latitude,y=Longitude),color='red',size=1)
# 
# a = ggplot()+ geom_point(data=CCnuts,aes(x=Latitude,y=Latitude.n)) + geom_abline(slope=1,linetype='dashed')
# b = ggplot()+ geom_point(data=CCnuts,aes(x=Longitude,y=Longitude.n)) + geom_abline(slope=1,linetype='dashed')
# a+b


## Now the dataframe we are working on is called CCmod (as in "carbonate chemistry model")
## This way we can keep "CCnuts" as a previous intermediate dataframe
CCmod=CCnuts
# head(CCmod)

##____Add PAR for each sampling location 

## Get hour of day for each sample --------------------------------------------------

# class(CCmod$UTCDate_Time.x) #"POSIXct" "POSIXt"
# table(is.na(CCmod$UTCTime)) #all samples have times
# table(is.na(CCmod$UTCDate)) #all samples have dates
CCmod$TIMESTAMP_UTC <- ymd_hms(CCmod$UTCDate_Time.x) 
# class(CCmod$TIMESTAMP_UTC)  #"POSIXct" "POSIXt" 
# CCmod$TIMESTAMP_UTC[1] # "2010-03-08 12:26:00 UTC"

#Find local solar time
CCmod$TIMESTAMP_LST <- convert_UTC_to_solartime(date.time = CCmod$TIMESTAMP_UTC,longitude = CCmod$Longitude,time.type = "apparent solar") 
# CCmod$TIMESTAMP_LST[1] #"2010-03-08 06:54:41 UTC"
# CCmod$ESTTime[1] #"07:26:00" #how are the minutes different? 
CCmod$jday.utc = yday(CCmod$TIMESTAMP_UTC) 
CCmod$jday.lst = yday(CCmod$TIMESTAMP_LST)
CCmod$hrod.lst = hour(CCmod$TIMESTAMP_LST)+minute(CCmod$TIMESTAMP_LST)/60+second(CCmod$TIMESTAMP_LST)/3600
# CCmod$hrod[1] #6.911396
# CCmod$ToD[1] #7.433333

#no samples lack coordinates 
# which(is.na(CCmod$Latitude))
# which(is.na(CCmod$Longitude))

# Add satellite PAR data from ERDDAP
CCmod$PAR_MODIS_DAILY=NA
CCmod$PAR_MODIS_8DAY=NA
CCmod$PAR_MODIS_MON=NA
plat=CCmod$Latitude
plon=CCmod$Longitude
ptime=as.POSIXct(CCmod$TIMESTAMP_UTC)
ptime=as.character(ptime)


for(i in 1:length(ptime)){
  par_g<-griddap("erdMH1par01day",
                 time=c(ptime[i],ptime[i]),
                 latitude=c(plat[i],plat[i]),
                 longitude=c(plon[i],plon[i]))
  CCmod$PAR_MODIS_DAILY[i]=par_g$data$par

  par_g8<-griddap("erdMH1par08day",
                  time=c(ptime[i],ptime[i]),
                  latitude=c(plat[i],plat[i]),
                  longitude=c(plon[i],plon[i]))
  CCmod$PAR_MODIS_8DAY[i]=par_g8$data$par

  par_gmon<-griddap("erdMH1par0mday",
                    time=c(ptime[i],ptime[i]),
                    latitude=c(plat[i],plat[i]),
                    longitude=c(plon[i],plon[i]))
  CCmod$PAR_MODIS_MON[i]=par_gmon$data$par

  print(i) #1610 total
  # print(t(CCmod[,c("PAR_MODIS_DAILY","PAR_MODIS_8DAY","PAR_MODIS_MON")]))
}

## Add bathymetry to the dataframe
CCpar_bathy= st_join(CCmod,bathy_sf) #dataframe with PAR and bathymetry
# head(CCpar_bathy)

#we now have a bathymetry column which is the depth of the water column where the sample was collected
colnames(CCpar_bathy)[colnames(CCpar_bathy) == 'bathymetry'] <- 'pointDepth'
# dim(CCpar_bathy) # 1612   84
# class(CCpar_bathy)

## Remove geometry to save intermediate df for the next steps
# CCout = CCpar_bathy %>% st_drop_geometry() 
## My intermediate file is "CCnutsparbath_9feb2024.csv" 

## NEXT STEP: overlap CC dataframe (carbonate chemistry + nutrients + par + bathymetry) with flowshed data 

