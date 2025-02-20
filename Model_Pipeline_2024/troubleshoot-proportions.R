
#this is done better in the rmd file

#This script reformats data from Thomas Dobbelaere for more easily using with other FLK data
#Thomas calculated the fraction of time each particle spends either north or south of my "FLK line" for each number of days after release

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('sf','stringr','mapview','tidyr')
# "ggmap","ggplot2","patchwork","mapview","leaflet",dplyr
lapply(packageload, library, character.only = TRUE)

# # ----Check which packages we actually use----
# # Find which packages do used functions belong to ----
# used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/Flowshed_Modeling/Model_Pipeline_2024/troubleshoot-proportions.R", alphabetic = FALSE) |> print()
# # Find which loaded packages are not used ----
# used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
# unused.packages <- packageload[!(packageload %in% used.packages)] |> print()


## Load data from Thomas (for proportion of time on either side of the FLK line)
pTime= st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/fromDobby/refloridakeysline/time_north_shp/time_north.shp')
class(pTime )
summary(pTime$fraction)
dim(pTime)/2 #1375
head(pTime)
length(unique(pTime$sample_id))

## Restructure data from Thomas so that the columns for each time fraction (ex: north_1d) are in two columns for the north and south fractions respectively
pTime.L = pivot_longer(data=pTime,cols=-c(simu,sample_id,geometry),names_to="side.ndays",values_to="fraction")
head(pTime.L)
summary(pTime.L$fraction) #226 NA values

## Separate the side of the line and the number of days into new columns 
pTime.L <- pTime.L %>% separate(side.ndays, c("side", "ndays"), "_")
head(pTime.L)

pTime.L$ndays= as.numeric(str_extract(pTime.L $ndays, "[0-9]+")) #drop the "d" and only keep number of days
pTime.W = pivot_wider(data=pTime.L,names_from='side',values_from='fraction')
head(pTime.W)
# final = reshape(test, idvar = "side", timevar = "fraction", direction = "wide")
# final = pivot_wider(data=test,cols=c(Side,fraction),names_from='Side',values_from='fraction') #do not specify cols? 

#save geometry to add back on 
# geoCol = pTime.W$geometry
# geoCol

# #try this because there is an issue with renaming an sf...
# pTime.W <- pTime.W %>%
#   st_drop_geometry() %>%  # Temporarily drop geometry to a regular data frame
#   dplyr::rename(north_fraction = north, south_fraction = south) #%>%
#   # st_as_sf()  # Convert back to an sf object
# pTime.W
# 
# pTime.W$geometry=geoCol #would be smarter to merge new and old dataframes using sample_id to match
# pTime.W

#rename fraction columns
# pTime.W=pTime.W %>% dplyr::rename(north_fraction=north,south_fraction=south) #rename fraction columns

#new idea. just add duplicate fraction columns then drop old one. 
pTime.W$north_fraction = pTime.W$north
pTime.W$south_fraction = pTime.W$south
#drop north and south 
pTime.W %>% dplyr::select(-north) %>% dplyr::select(-south)

head(pTime.W)
dim(pTime.W)
test = as.data.frame(pTime.W)
# write out new dataframe
class(pTime.W)
# pTime.W=
# st_write(pTime.W,'/Users/heidi.k.hirsh/Desktop/FLK_data/BowtieFractions_Jan10.shp')

#why are some fractions NA? 
summary(pTime.W$north_fraction)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00000 0.00000 0.00000 0.08140 0.02251 0.99845     113 
summary(pTime.W$south_fraction)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00155 0.97749 1.00000 0.91860 1.00000 1.00000     113 

## Why are some fractions NA? 
# Thomas first thought this was because of dividing by 0 somewhere. 
# He investigated and found that they appear when particles left the mesh domain in less than 14 days. 

## Does this bias my data?  
#are they mostly forward or backward?
#what number of days? (guessing longer)
## Take Home: No bias. I'm not even using forward polygons in the model.

#limit data to just the NAs and explore
#check if north_fraction is NA then south_fraction is also NA (yes)
summary(subset(pTime.W, is.na(pTime.W$north_fraction)) == subset(pTime.W, is.na(pTime.W$south_fraction)))

north_na = subset(pTime.W, is.na(pTime.W$north_fraction))
dim(north_na)
head(north_na)

length(unique(north_na$sample_id)) #41
#how many sites and dates?
north_na = north_na %>% separate(sample_id, c("site", "date", "time"), "_")
length(unique(north_na$site)) #8 # "15.5" "3"    "6"    "21.5" "9.5"  "22.5" "1"    "6.5" 
length(unique(north_na$date)) #26
length(unique(north_na$time)) #40 (not meaningful because not paired with date)
length(unique(north_na$ndays)) #9 (6-14 days)
length(unique(north_na$simu)) #all forward

mapview(north_na,zcol="site")


#NEXT: pair this data with CC dataframe to match up site zones and regions for further exploration


