## This script combines pairs flowshed spatial data with the point data dataframe. This allows us to add benthic data to the modeling dataframe. 
## Relevant endmember data (based on relative water mass history north or south of the Keys) is also incorporated here. 
## Run this script AFTER running 'buildStationDF.R' (for sampling station dataframe, CC below) and 'benthicOverlap_saveFlowsheds.R' for corresponding benthic data (from flowsheds)

## Heidi K. Hirsh
## Last edit: Feb 20, 2025

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('ggplot2','mapview','sf')
# packageload <- c('magick','ggplot2','tidyverse','ggrepel','rstudioapi','stringr','sp','raster','patchwork','mapview','rerddap','leaflet','streamMetabolizer','sf')
lapply(packageload, library, character.only = TRUE)

sf_use_s2(FALSE) #nutrient point overlap with polygons will not work without this line!

## Load modeling dataframe built in P1 ----------------------------------------------------------
CC=NULL
# CC=CCpar_bathy #if you are running after 'buildStationDF.R'
CC <- read.csv(file='Flowshed_Modeling_InputData/CCnutsparbath_9feb2024.csv') #This dataframe is built in 'buildStationDF.R'

##__________pair with fraction of time water for each station comes from north or south of the Keys
#load fractions (proportional time north or south of the Keys)
ptime  <- st_read('Flowshed_Modeling_InputData/EndmemberFraction/EndmemberFraction.shp')

## Read in BBB (benthic bowties aka flowsheds with benthic data)
## this shapefile is built in 'benthicOverlap_saveFlowsheds.R'
BBB = st_read('Flowshed_Modeling_InputData/Concave_BowBenthic_14days_26april2024/Concave_BowBenthic_14days_26april2024.shp')

## Load points on west florida shelf
PTS = read.csv('Flowshed_Modeling_InputData/PTS.csv')

# names(ptime)
## Rename columns
names(ptime)[names(ptime) == 'sampl_d'] <- 'sample_id'
names(ptime)[names(ptime) == 'nrth_fr'] <- 'north_fraction'
names(ptime)[names(ptime) == 'sth_frc'] <- 'south_fraction'

names(ptime) #"simu" "sample_id" "ndays" "north_fraction" "south_fraction" "geometry"    
ptime$visitID = ptime$sample_id 

ptime=ptime %>% filter(simu=='backward') #only need backward flowsheds
# unique(ptime$simu)
# unique(ptime$ndays) #1-14
# nrow(ptime)/14 #1375
# dim(ptime) #19250     


CCp = NULL
CCp = left_join(ptime, CC, by=c("visitID"))
dim(CCp) #19250    89
# dim(CCp)/14 # 1375 

###???? Why 1375 versus #1612 in P1 CC? (years subset?)
unique(CCp$Year) #2012 2014 2015 2016 2017 2018 NA 2019 2020 2021 
#where is the NA from? ptime didn't have any year value. Is a ptime value missing year in the visitID?
unique(CC$Year)
table(CCp$Month)
table(CCp$Year) #search for year is na?


ggplot(CCp)+
  geom_jitter(aes(x=DIC_umol_kg,y=TA_umol_kg,color=Sub_region))+
  facet_wrap(~ndays)+
  theme_bw()

ggplot(CCp)+
  geom_jitter(aes(x=DIC_umol_kg,y=TA_umol_kg,color=Sub_region))+
  facet_wrap(~Zone)+
  theme_bw()
#there is an NA region here as well. 

#why are some still na for Sub_region
table(CCp$Sub_region)
unique(CCp$Sub_region) #"BB" "UK" "MK" "LK" NA  
table(CCp$Zone)
unique(CCp$Zone)# "Offshore"    "Mid channel" "Inshore"     "Oceanic"     NA    
CCp = subset(CCp,!is.na(CCp$Zone)) #This removes it but WHAT IS IT?


#now we have station carb chem and fractions saved together in CCp
#next: pair with benthic bow ties (BBB) 
## ???I think BBB already has benthic data paired with each bowtie.


## BENTHIC BOWS _____________________________
#this is output of cutting habitat by bow extents (for ndays = 1-7) 
# WHERE DOES THIS HAPPEN?
#keep forward and backward bows for now - I only use backward in this paper so we can cut forward
#does BBB have geometry? Or should I pair with benthos THEN add geometry back


# unique(BBB$n_days) #1-14
# unique(BBB$simu) #only backward
# names(BBB) #"n_days"   "vol_km3"  "simu"     "year"     "visitID"  "duratin"  "bowID"    "CALC_m2"  "ALGi_m2"  "SGi_m2"   "NBi_m2"   "NCo_m2"   "PrcntCh"  "geometry"
# 
# # table(BBB$duratin==BBB$n_days) #always true 
# dim(BBB)/14 #1379 1
# class(BBB)


#This is an annoying mistake from building BBB, but not worth rerunning since those loops take a LONG time.
names(BBB)[names(BBB) == 'n_days'] <- 'ndays'
names(BBB)[names(BBB) == 'duratin'] <- 'duration'

#subset to years for modeling (2015-2021)

#Pairing BBB to CC will pair appropriate benthic habitat indices with each row/sample for each number of days
#match using visitID
# dim(CC) #1612 83
dim(CCp) #19250 89
dim(BBB)/14 #1379 1
table(CCp$Year)
# 2012 2014 2015 2016 2017 2018 2019 2020 2021 
# 336  504 2632 3150 2562 2940 2100 2002 2996 
table(BBB$year)
# 2012 2014 2015 2016 2017 2018 2019 2020 2021 
# 168  252 1316 1575  189 1477 1050 1008 1505 

CCp$Year=as.character(CCp$Year)

## join CCp (with fractions 1-14 days) with Benthic Bows!
unique(BBB$ndays) #1-14

CC14d=NULL
class(BBB) #"sf"         "data.frame"
class(CCp) #"sf"         "data.frame"
CCp = st_drop_geometry(CCp)
head(CCp) #no more geometry

#join CCp to BBB: 
CC14d=CCp %>% left_join(BBB,by=c("visitID","simu","ndays")) 
CC14d
dim(CC14d) # 19292   99
dim(CC14d)/14 #1378



###_________model endmember chemistry, assign admixture chemistry, compute deltas__________
CCf=NULL
CCf=CC14d
dim(CCf)

## use stations from the west florida shelf
PTS$Lat=PTS$Latitude_Dec_Deg
PTS$Long=PTS$Longitude_Dec_Deg
PTS=st_as_sf(PTS, coords = c("Long","Lat"),crs = st_crs(4326)) 


# The most complete coverage is only available for Shark River (SR) and Middle Cape (MC) so we will use those only
iwant = c("SR","MC")
PTSsub= subset(PTS,line_id %in% iwant)
mapview(PTSsub,zcol='bathymetry')

PTSsub= PTSsub %>% filter(Latitude_Dec_Deg>25) #remove 2 points in the keys
dim(PTSsub)
mapview(PTSsub,zcol='bathymetry')

#sine and cosine fits for the year
PTSsub$xc<-cos(2*pi*PTSsub$YearDay_UTC/366)
PTSsub$xs<-sin(2*pi*PTSsub$YearDay_UTC/366)

#filter by depth (use 5m)
plot(sort(PTSsub$bathymetry))
abline(h=5)

PTSsub5=PTSsub %>% filter(bathymetry>5) 
#only use the samples on the transect where the depth was greater than 5m

#ultimately we chose to use the data filtered at 5m
PTSsub5 %>% ggplot()+geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+theme_bw()


##____________Model DIC___________ (no year in model)

DICmod_3_noY_5m=lm(DIC_umol.kg~xc+xs,data=PTSsub5)
summary(DICmod_3_noY_5m) #Multiple R-squared:  0.6204,	Adjusted R-squared:  0.6024
#DIC prediction: 
PTSsub5$DICpred_3_noY_5m=predict(DICmod_3_noY_5m,newdata=PTSsub5)

dic=ggplot(PTSsub5)+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_noY_5m),color='black',linetype='dashed',lwd=1)+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+
  theme_bw()

##____________Model TA___________ (no year)

TAmod_3_noY_5m=lm(TA_umol.kg~xc+xs,data=PTSsub5)
summary(TAmod_3_noY_5m) #Multiple R-squared:  0.4679,	Adjusted R-squared:  0.4425 
#TA prediction:
PTSsub5$TApred_3_noY_5m=predict(TAmod_3_noY_5m,newdata=PTSsub5)

ta=ggplot(PTSsub5)+
  geom_line(aes(x=YearDay_UTC,y=TApred_3_noY_5m),color='black',linetype='dashed',lwd=1)+
  geom_point(aes(x=YearDay_UTC,y=TA_umol.kg,color=factor(Year_UTC)))+
  theme_bw()
dic/ta

#Now apply those models to CCf (data frame including fractions) to determine relevant endmember chemistry
#then run models on data set that also includes source water fraction (north_fraction and south_fraction)

#north and south fractions should account for all samples
summary(CCf$north_fraction+CCf$south_fraction)


## DIC v. TA plots by region and zone:
# ggplot(CCf)+
#   geom_jitter(aes(x=DIC_umol_kg,y=TA_umol_kg,color=Sub_region))+
#   facet_wrap(~Sub_region)
# 
# ggplot(CCf)+
#   geom_jitter(aes(x=DIC_umol_kg,y=TA_umol_kg,color=Sub_region))+
#   facet_wrap(~Zone)


dim(CCf) # 19292
dim(subset(CCf,is.na(CCf$DIC_umol_kg)==T))  # 168  99
dim(subset(CCf,is.na(CCf$TA_umol_kg)==T))   # 70 99
summary(CCf$DIC_umol_kg)


#calculate oceanic means (for outermost zones on transects in the Keys)
CCoceanRef = CCf %>% dplyr::filter(Zone=="Oceanic") %>% dplyr::group_by(Year,Month) %>% 
  dplyr::summarize(TAoce_mean= mean(TA_umol_kg, na.rm=T),
                   DICoce_mean= mean(DIC_umol_kg, na.rm=T),
                   ARAGoce_mean= mean(Aragonite_Sat_W, na.rm=T),
                   Toce_mean= mean(Temperature_C, na.rm=T),
                   Soce_mean= mean(Salinity_CTD, na.rm=T))

CCf.ref = left_join(CCf,CCoceanRef,by=c("Year","Month"))
names(CCf.ref)
# Note: oce_mean values are mean of oceanic station values BELOW the Keys

CCf.ref$xc=cos(2*pi*CCf.ref$jday.utc/366)
CCf.ref$xs=sin(2*pi*CCf.ref$jday.utc/366)



# CCf.ref %>% filter(ndays==14) %>%
# # CCf.ref %>% filter(ndays==6) %>%
#   ggplot()+
#   geom_histogram(aes(x=north_fraction,stat(density)))+
#   facet_wrap('Sub_region')+
#   theme_bw()

##this didn't worka
# #we don't want to ever use north_fraction for BB
# CCf.ref %>% filter(ndays==14) %>% group_by(Sub_region) %>%
#   summarize(mean_nf=mean(north_fraction,na.rm=T))


##____________Predict DIC and TA endmember chemistry___________ 

#Use mod_3 models to predict northern reference (above Keys) from 5m filtered data
CCf.ref$DICrefN = predict(DICmod_3_noY_5m,newdata=CCf.ref)
CCf.ref$TArefN = predict(TAmod_3_noY_5m,newdata=CCf.ref)

#Recalculate Florida Bay "FB" fraction instead of northern_fraction (make sure BB never gets north fraction from FB)
CCf.ref$FBfraction = CCf.ref$north_fraction 
CCf.ref$FBfraction[CCf.ref$Sub_region=="BB"]=0

#Calculate south of Keys (Soce) fraction (acounting for the BB fractions always being entirely "south" water)
CCf.ref$Soce_fraction = 1-CCf.ref$FBfraction
# BBay = CCf.ref %>% filter(Sub_region=="BB")
# table(BBay$Soce_fraction) #all 1 (good)

#calculate admixture endmembers for DIC and TA
CCf.ref$DICrefNS = CCf.ref$FBfraction*CCf.ref$DICrefN + CCf.ref$Soce_fraction*CCf.ref$DICoce_mean
CCf.ref$TArefNS = CCf.ref$FBfraction*CCf.ref$TArefN + CCf.ref$Soce_fraction*CCf.ref$TAoce_mean

#Compare endmember chemistry!
JJ=8
end =CCf.ref%>% filter(ndays==14) %>% 
  # CCf.ref %>% filter(Year==2019) %>% filter(ndays==14) %>% 
  ggplot()+
  geom_jitter(width=JJ,height=JJ,aes(x=DICrefNS,y=TArefNS,color=factor(MoY)),shape=17,alpha=.75,size=2)+
  geom_point(aes(x=DICrefN,y=TArefN,fill=factor(MoY)),shape=21,size=3,color='black',alpha=.1)+
  geom_point(aes(x=DICoce_mean,y=TAoce_mean,fill=factor(MoY)),shape=24,size=3,color='black')+
  # add the special 6 points!
  facet_wrap('Sub_region')+
  theme_bw()
end
# ggsave(end,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/endmember.png")

CCmod=CCf.ref

CCmod$TA_delta.S = CCmod$TA_umol_kg - CCmod$TAoce_mean
CCmod$DIC_delta.S = CCmod$DIC_umol_kg - CCmod$DICoce_mean
CCmod$ARAG_delta.S = CCmod$Aragonite_Sat_W - CCmod$ARAGoce_mean

CCmod$TA_delta.NS = CCmod$TA_umol_kg - CCf.ref$TArefNS
CCmod$DIC_delta.NS = CCmod$DIC_umol_kg - CCf.ref$DICrefNS



A=ggplot(CCmod)+
  geom_point(aes(x=DIC_delta.NS,y=TA_delta.NS,color=ndays))+
  geom_point(aes(x=DIC_delta.S,y=TA_delta.S),color='red')+
  facet_wrap(~Sub_region)+
  theme_bw()
A
# ggsave(filename="/Users/heidi.k.hirsh/Desktop/deltaComp.png",plot=A,width = 12, height = 8, dpi = 300)
  

names(CCmod)
MODEL=CCmod

#output dataframe including relevant endmembers
# write.csv(CCmod,'/Users/heidi.k.hirsh/Desktop/CCmodel_10feb2024.csv',row.names=F)

msave = subset(MODEL, select = -c(geometry)) 
#no geometry!

#write CCmodelDF (save intermediate)
# write.csv(msave,file='/Users/heidi.k.hirsh/Desktop/CCmodelDF_April26.csv',row.names=FALSE)
dim(msave) # 19222   117 (I'm getting 19292 116 now)

## __ Now we have a dataframe that includes: 
#carbonate chemistry
#nutrients
#PAR
#bathymetry
#fractions
#benthic bowtie data
#oceanic reference data
#predicted northern and southern endmember chemistry


## Save complete cases (all input parameters available for each sample) for final model dataframe
CCfull.all = msave
# CCfull.all = read.csv(file='/Users/heidi.k.hirsh/Desktop/Hirsh_FLKmodel_InputFiles/CCmodelDF_April26.csv')
# unique(CCfull.all$ndays)
# dim(CCfull.all) / 14 #1376   


## Make sure all samples have zone information
# dim(CCfull.all) #19264   116
goodZone = which(!is.na(CCfull.all$Zone)) 
# length(goodZone) #19264 #all rows are good
CCfull.all = CCfull.all[goodZone,]
# dim(CCfull.all) / 14  #1376
CCfull=CCfull.all


#add inverse volume
CCfull$inverseVol = 1 / CCfull$vol_km3
#add inverse habitat 
CCfull$invHab = 1 / CCfull$pointDepth

## Convert to numbered months
CCfull$date=as.Date(CCfull$Date,format="%Y-%m-%d")
# class(CCfull$date)
CCfull$M=format(CCfull$date, format="%B")
CCfull$M2=format(CCfull$date, format="%m")
CCfull$month = as.numeric(CCfull$M2)
# class(CCfull$month)
sort(unique(CCfull$month))


##________________________________

## need to filter out NA values for model input
## look at just the input parameter columns(parameters that will be specifically included in the models)
IN = c("CALC_m2","ALGi_m2","SGi_m2","Chla","NO3","inverseVol","Month","month","Salinity_Bottle","Temperature_C","hrod.lst",
       "DIC_delta.S", "DIC_delta.NS", "TA_delta.S", "TA_delta.NS","Year")
# summary(CCfull[, IN])

## Data needs to be the same length for each model so we need to keep only the data where every sample has information for every parameter
CCgood = CCfull[complete.cases(CCfull[IN]),]
# dim(CCgood)/14 #984
# unique(CCgood$Zone)
# unique(CCgood$Sub_region)

## Save complete cases: 
# write.csv(CCgood, file="Flowshed_Modeling_IntermediateFiles/CC_completeCases.csv",row.names=FALSE)

