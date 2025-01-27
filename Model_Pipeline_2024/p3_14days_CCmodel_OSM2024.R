## P3. Just need to save complete cases dataframe to feed into model: 

#12/19 check code works. streamline
# 1/2/2025 further streamline and document in Whimsical

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('ggplot2','janitor')
# packageload <- c('magick','ggplot2','tidyverse','ggrepel','rstudioapi','stringr','sp','raster',
#                  'janitor','patchwork','mapview','rerddap','leaflet','streamMetabolizer')

lapply(packageload, library, character.only = TRUE)


# # ----Check which packages we actually use----
# # Find which packages do used functions belong to ----
# used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/Flowshed_Modeling/Model_Pipeline_2024/p3_14days_CCmodel_OSM2024.R", alphabetic = FALSE) |> print()
# # Find which loaded packages are not used ----
# used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
# unused.packages <- packageload[!(packageload %in% used.packages)] |> print()



#LOAD FILES 

CCfull.all = read.csv(file='/Users/heidi.k.hirsh/Desktop/Hirsh_FLKmodel_InputFiles/CCmodelDF_April26.csv')
unique(CCfull.all$ndays)
dim(CCfull.all) / 14 #1376   

noNa = na.omit(CCfull.all)
dim(noNa) #0 117 #every row has NA somewhere 

# table(table(which(is.na(CCfull.all),arr.ind=T)[,1])) 


unique(CCfull.all$simu) #already limited to backward only 
unique(CCfull.all$Sub_region) #no NA
unique(CCfull.all$Zone) #no NA

dim(CCfull.all) #19264   116
goodZone = which(!is.na(CCfull.all$Zone)) 
length(goodZone) #19264 #all rows are good
CCfull.all = CCfull.all[goodZone,]
dim(CCfull.all) / 14  #1376


unique(CCfull.all$ndays) #1-14

CCfull=CCfull.all


#add inverse volume
CCfull$inverseVol = 1 / CCfull$vol_km3
#add inverse habitat (why?)
CCfull$invHab = 1 / CCfull$pointDepth

#convert month to number: 
unique(CCfull$Month)

CCfull$date=as.Date(CCfull$Date,format="%Y-%m-%d")
# class(CCfull$date)
CCfull$M=format(CCfull$date, format="%B")
CCfull$M2=format(CCfull$date, format="%m")
CCfull$month = as.numeric(CCfull$M2)
# class(CCfull$month)
sort(unique(CCfull$month))


##________________________________

#need to filter out NA values for model input
#look at just the input parameter columns
IN = c("CALC_m2","ALGi_m2","SGi_m2","Chla","NO3","inverseVol","Month","month","Salinity_Bottle","Temperature_C","hrod.lst",
       "DIC_delta.S", "DIC_delta.NS", "TA_delta.S", "TA_delta.NS","Year")

summary(CCfull[, IN])

#Data needs to be the same length for each model
CCgood = CCfull[complete.cases(CCfull[IN]),]
dim(CCgood)/14 #984


#save complete cases: 
# write.csv(CCgood, file="/Users/heidi.k.hirsh/Desktop/CC_complete_cases_26april2024.csv",row.names=FALSE)

