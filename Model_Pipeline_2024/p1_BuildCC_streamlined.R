## Read in separate data and combine into a new dataframe for the model
# 12/19 simplify/check code - next step will be combining/streamlining and renaming.
# 1/2/2025 further streamline and document in Whimsical

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('mapview','sf','stringr','dplyr','lubridate','ggplot2','patchwork','streamMetabolizer')
# packageload <- c('magick','ggplot2','tidyverse','ggrepel','rstudioapi','stringr','sp','raster','patchwork','mapview','rerddap','leaflet','streamMetabolizer','sf','ggmap')
lapply(packageload, library, character.only = TRUE)

# # ----Check which packages we actually use----
# # Find which packages do used functions belong to ----
# used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/Flowshed_Modeling/Model_Pipeline_2024/p1_BuildCC_streamlined.R", alphabetic = FALSE) |> print()
# # Find which loaded packages are not used ----
# used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
# unused.packages <- packageload[!(packageload %in% used.packages)] |> print()
# --------------------------------------------

## LOAD Files:

## Load filtered carbonate chemistry data (source: Ana Palacio) ----------------------------------------------------------
CC <- read.csv('/Users/heidi.k.hirsh/Desktop/Hirsh_FLKmodel_InputFiles/FLK_filtered_ve4.csv') #source this from somewhere else.
dim(CC) #1612   52

## Load nutrient data (source: Alex Fine) ----------------------------------------------------------
Nuts_original <- read.csv('/Users/heidi.k.hirsh/Desktop/Hirsh_FLKmodel_InputFiles/WS_Keys_Data_1998-2022_hh.csv')
dim(Nuts_original) # 3739   17

## Load bathymetry layer (from SLIM 2D model)
bathy_sf <- st_read(dsn = "/Users/heidi.k.hirsh/Desktop/Hirsh_FLKmodel_InputFiles/mesh_florida",layer="mesh_florida")

#----------------------------------------------------------

CC$jday= yday(CC$datetime)
unique(CC$Year) #2010 2011 2012 2014 2015 2016 2017 2018 2019 2020 2021 2022

CC$UTCDate_Time = as.POSIXct(CC$UTCDate_Time, tz="UTC")

#there is one point that does not have coordinates
dim(subset(CC,is.na(CC$Longitude)))
dim(subset(CC,is.na(CC$Latitude)))

#remove this point
CC=subset(CC,!is.na(CC$Latitude) & !is.na(CC$Longitude))
dim(CC) #1611   52 (one removed)

## What is the temporal coverage of data?
temporal = ggplot(data=CC,aes(x=jday,y=DIC_umol_kg,color=as.factor(Year)))+
  geom_point()+
  facet_grid("Year")+
  theme_bw()+
  theme(legend.position="bottom")+
  scale_x_continuous(breaks = seq(0, 365, by = 10))
TS=Sys.time()
temporal
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/PipelineFigs_Dec2024/TemporalCCcoverage_",TS,".png"),plot=temporal,width=20, height=12, dpi = 300)


## Add visitID (this will be use to match discrete carbonate chemistry to other paired data (nutrients, water mass history, etc.)
visitID_1 =  paste(CC$SiteID,CC$UTCDate_Time)
visitID_2 = gsub(" ", "_" , visitID_1, perl=TRUE)
CC$visitID =  gsub("[: -]", "" , visitID_2, perl=TRUE)


CC.sf = st_as_sf(CC, coords = c("Longitude","Latitude"), crs = st_crs(4326))  
mapview(CC.sf,zcol='Zone')


## Rename nutrient columns to avoid confusion when comparing to carbonate chemistry coordinates
## Note, coordinates should be the same sites
Nuts= Nuts_original %>% dplyr::rename('Latitude.n'='Latitude', 'Longitude.n'='Longitude', 'Date.n'='Date')
# names(Nuts)

#name new coordinate columns so that this information is not lost when making it sf 
Nuts$Longitude = Nuts$Longitude.n
Nuts$Latitude = Nuts$Latitude.n
Nuts.sf = st_as_sf(Nuts, coords = c("Longitude","Latitude"), crs = st_crs(4326))  
# mapview(Nuts.sf,zcol='Station')
head(Nuts.sf)

##we do not need the point on the West Florida Shelf.
mapview(Nuts.sf,zcol="Station")
##that point is labeled station 19, but should not be grouped with that station
pesky = subset(Nuts.sf,Station %in% c('19')) 
mapview(pesky,zcol="Station")
##the the "pesky" northern point will be identify later by visitID and removed

##Be consistent with station naming to match carbonate chemistry dataframe
Nuts.sf$Station[which(Nuts.sf$Station == "21")] = "21LK"

##Remove station KW1 (north of Key West)
Nuts.sf = Nuts.sf[which(Nuts$Station != "KW1"),] 


##Create the matching visitIDs for the nutrient data

##Nutrients data date formatting is mildly infuriating so we are going to fix that:
Nuts.sf$Date.n 
# Nuts.sf$Date[16] #"1/30/98"
Nuts.sf$Date.yyyymmdd = as.Date(Nuts.sf$Date.n, format= "%m/%d/%y")
# Nuts.sf$Date.yyyymmdd[16] # "1998-01-30"
#pair date and time
Nuts.sf$dateTime = str_c(Nuts.sf$Date.yyyymmdd, ' ', Nuts.sf$GMT)
# Nuts.sf$dateTime[16] #"1998-01-30 18:45"
Nuts.sf$UTCDate_Time = as.POSIXct(Nuts.sf$dateTime, tz="UTC") 
# Nuts.sf$UTCDate_Time[16] #"1998-01-30 18:45:00 UTC"

visitID_n1 = paste(Nuts.sf$Station,Nuts.sf$UTCDate_Time) #combine station name and datetime
visitID_n2 = gsub(" ", "_" ,visitID_n1, perl=TRUE) #separate station, date, and time by "_"
Nuts.sf$visitID =  gsub("[: -]", "" , visitID_n2, perl=TRUE) #remove all characters except "_"
# Nuts.sf$visitID[16] #"16_19980130_184500"
# dim(Nuts.sf) #3717   21

##Now, we can remove the extra "19" northern point
##the pesky point is visitID = 19_20160728_013600
##Save Nuts without this point
Nuts.sf = Nuts.sf[which(Nuts.sf$visitID !='19_20160728_013600'),]  
# mapview(Nuts.sf) #gone!

##limit nutrients to same years as CC
Nuts.sf$Year = format(Nuts.sf$UTCDate_Time, format="%Y")
Nuts.sf$Year = as.integer(Nuts.sf$Year) 
# unique(Nuts.sf$Year) #1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2014 2015 2016 2017 2018 2019 2020 2021 2022
CCyears = unique(CC$Year) #2010 2011 2012 2014 2015 2016 2017 2018 2019 2020 2021 2022 
Nuts.sf = subset(Nuts.sf,Year %in% CCyears)
# unique(Nuts.sf$Year) #2010 2011 2012 2014 2015 2016 2017 2018 2019 2020 2021 2022
# dim(Nuts.sf) # 2017   21
mapview(Nuts.sf,zcol='Year')

## Merge CC and Nuts.sf
# head(CC$visitID)
# length(unique(CC$visitID)) #1610
# head(Nuts.sf$visitID)
# length(unique(Nuts.sf$visitID)) #2017

#Which visitIDs in CC do not have matching nutrients?
noMatch = CC[which(is.na(match(CC$visitID,Nuts.sf$visitID))),]
# noMatch #one sample in CC is missing nutrients
length(unique(noMatch$visitID))
unique(noMatch$visitID) # "21LK_20201213_082700" #only one missing?

##remove that sample before merging with nutrients (Nuts.sf)
# dim(CC) #1611   53
# test = subset(CC, visitID==unique(noMatch$visitID))
CC=subset(CC, !visitID==unique(noMatch$visitID))
# dim(CC) #1610   53

CCnuts = left_join(CC, Nuts.sf, by=c("visitID","Year"))
# names(CCnuts)
# dim(CCnuts) #1610   74


##Check that CC and nutrient coordinates match: (they do)
ggplot()+
  geom_point(data=CCnuts,aes(x=Latitude.n,y=Longitude.n),size=3)+
  geom_point(data=CCnuts,aes(x=Latitude,y=Longitude),color='red',size=1)

a = ggplot()+ geom_point(data=CCnuts,aes(x=Latitude,y=Latitude.n)) + geom_abline(slope=1,linetype='dashed')
b = ggplot()+ geom_point(data=CCnuts,aes(x=Longitude,y=Longitude.n)) + geom_abline(slope=1,linetype='dashed')
a+b


#This is a good point to export the carbonate chemistry + nutrients data prior to bringing in spatial polygons
#remove geometry before saving
# dim(CCnuts) #1610   
CCnuts= CCnuts %>% st_drop_geometry() #this didn't work - still geometry?
# CCnuts=subset(CCnuts, select = -c(geometry))  #try this instead
dim(CCnuts) 
head(CCnuts)

#do not need to write this out in repository version
##______Save CCnuts______
# write.csv(CCnuts, file='/Users/heidi.k.hirsh/Desktop/CCnuts_Feb2024.csv',row.names=F)
# write.csv(CCnuts, file='/Users/heidi.k.hirsh/Documents/GitHub/test-flk/0_InputData/CCnuts_Jan2024.csv')
# CCnuts = read.csv(file='/Users/heidi.k.hirsh/Documents/GitHub/test-flk/0_InputData/CCnuts_Jan2024.csv')


##read in the CC+Nuts dataframe (or continue code)
##Now the dataframe we are working on is called CCmod (as in "carbonate chemistry model")
CCmod=CCnuts
# head(CCmod)

##____Add PAR (this wasn't working the last time I ran it so I read in a version with PAR already)

## Get hour of day for each sample --------------------------------------------------
## UTCDate_Time.x is the same as UTCDate_Time.y (duplicated from left join)
# sameDT = CCmod$UTCDate_Time.x==CCmod$UTCDate_Time.y
# table(sameDT) #all true (datetime is the same for both)

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

# Add satellite PAR data ---------FIX!----------------------------------------- (from PARflk.R originally)
# CCmod$PAR_MODIS_DAILY=NA
# CCmod$PAR_MODIS_8DAY=NA
# CCmod$PAR_MODIS_MON=NA
# plat=CCmod$Latitude
# plon=CCmod$Longitude
# ptime=as.POSIXct(CCmod$TIMESTAMP_UTC)
# ptime=character(ptime)
# 
# # i=763
# 
# # for(i in 763:length(ptime)){
# for(i in 1:length(ptime)){
#   par_g<-griddap("erdMH1par01day",
#                  time=c(ptime[i],ptime[i]),
#                  latitude=c(plat[i],plat[i]),
#                  longitude=c(plon[i],plon[i]))
#   CCflk$PAR_MODIS_DAILY[i]=par_g$data$par
#   
#   par_g8<-griddap("erdMH1par08day",
#                   time=c(ptime[i],ptime[i]),
#                   latitude=c(plat[i],plat[i]),
#                   longitude=c(plon[i],plon[i]))
#   CCflk$PAR_MODIS_8DAY[i]=par_g8$data$par
#   
#   par_gmon<-griddap("erdMH1par0mday",
#                     time=c(ptime[i],ptime[i]),
#                     latitude=c(plat[i],plat[i]),
#                     longitude=c(plon[i],plon[i]))
#   CCflk$PAR_MODIS_MON[i]=par_gmon$data$par
#   
#   print(i)
#   print(t(CCflk[,c("PAR_MODIS_DAILY","PAR_MODIS_8DAY","PAR_MODIS_MON")]))
# }

#PAR code not working. 
#steal par from old CC... 
CCp= read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/CC_dataframes/CCmodel_allYears+Nuts_13nov2023.csv')
# dim(CCp) #9639  100
# names(CCp) #PAR columns: "PAR_MODIS_DAILY"  "PAR_MODIS_8DAY"   "PAR_MODIS_MON" "visitID"

#simplify to only one ndays value
#this is necessary because the df I read in includes the backward bowties
CCp1 = CCp %>% filter(ndays==1) 
unique(CCp1$simu)
dim(CCp1) #1377  100

#simplify to only par columns and visitID
CCpS= CCp1[c("PAR_MODIS_DAILY","PAR_MODIS_8DAY","PAR_MODIS_MON","visitID")]
# dim(CCpS) #1377    4
# dim(CCmod) #1610   79

# CCmod$visitID
# CCpS$visitID

## READ IN JUST CCpar (simplify to only PAR columns in PAR script then all the problems are in PAR - not here)

#merge CC+Nuts dataframe with PAR daaframe
CCpar = left_join(CCmod, CCpS, by=c("visitID"))
# head(CCpar)
# View(CCpar)
CCpar$PAR_MODIS_DAILY
#there are NA values (as expected) for PAR in the years that weren't in my original CC df with PAR already paired
# unique(CCpar$Year)
# unique(CCmod$Year)
# unique(CCp$Year) #2012,2014-2021

# ggplot(CCpar,aes(y=PAR_MODIS_MON,PAR_MODIS_8DAY))+geom_point()+facet_wrap(.~is.na(PAR_MODIS_DAILY))
# ggplot(CCpar,aes(y=DIC_umol_kg,x=PAR_MODIS_8DAY))+geom_point()+facet_wrap(.~is.na(PAR_MODIS_DAILY))


## Add bathymetry/depth information for each sampling point
#Pull depth below samples (match bathymetry minus depth of sample)
unique(CCpar$Sample_Depth_m) #all surface samples (all zero depth)

## Convert CCpar to spatial dataframe to join with bathymetry data
#copy Lat and Lon columns otherwise they will be lost when we turn it into an sf dataframe
CCpar$LonCoord = CCpar$Longitude
CCpar$LatCoord = CCpar$Latitude

CCpar_sf =st_as_sf(CCpar, coords = c("LonCoord", "LatCoord")) %>% st_set_crs(st_crs(bathy_sf))
head(CCpar_sf)
# View(CCpar_sf)
# mapview(CCpar_sf) #duplicate names

## Add bathymetry to the dataframe
CCpar_bathy= st_join(CCpar_sf,bathy_sf)
head(CCpar_bathy)
mapview(CCpar_bathy,zcol="bathymetry") #doesn't plot ("Error: Field count reached: duplicate names present?")
mapview(CCpar_bathy,zcol="PAR_MODIS_MON") #doesn't plot ("Error: Field count reached: duplicate names present?")

#we now have a bathymetry column which is the depth of the water column where the sample was collected
colnames(CCpar_bathy)[colnames(CCpar_bathy) == 'bathymetry'] <- 'pointDepth'
# dim(CCpar_bathy) # 1612   84
# class(CCpar_bathy)

#remove geometry to save it as a df for the next steps
CCout = CCpar_bathy %>% st_drop_geometry()
# dim(CCout) #1612   83
# class(CCout)

# write.csv(CCout, file='/Users/heidi.k.hirsh/Documents/GitHub/test-flk/0_InputData/CCnutsparbath_1feb2024.csv')
# write.csv(CCout, file='/Users/heidi.k.hirsh/Desktop/CCnutsparbath_9feb2024.csv',row.names=F)
# write.csv(CCout, file='/Users/heidi.k.hirsh/Desktop/CC_dataframes/CCnutsparbath_2oct2024.csv',row.names=F)
# write.csv(CCout, file='/Users/heidi.k.hirsh/Desktop/PipelineFigs_Dec2024/CCnutsparbath_19dec2024.csv',row.names=F)

##NEXT STEP: overlap CC (carbonate chemistry + nutrients + par + bathymetry) with flowshed data 


