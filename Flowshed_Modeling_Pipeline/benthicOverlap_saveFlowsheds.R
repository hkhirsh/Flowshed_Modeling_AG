##This script uses the flowshed polygons from T. Dobbelaere to crop and pair the appropriate habitat data from the Florida's Unified Reef Map and associate it with each sampling station as a benthic index.
## Note: flowsheds are referred to herein as "bow ties." This was their original name since the first versions resembled bow ties with (usually teardrop-shaped) areas extending forward and backward from each station. 
##Heidi K. Hirsh
##Last edit: Feb 20, 2025


rm(list = ls()) #clear environment

# Load packages ----
packageload <- c("sf","stringr","raster","dplyr","sp","mapview","leaflet")
lapply(packageload, library, character.only = TRUE)



#####__________________Load Data___________________#####

# Read in FL unified reef map habitat data:
coral_sf = st_read(dsn = "Flowshed_Modeling_InputData/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer = "UnifiedReefMap")

# Load benthic composition lookup table:
benthicLU = read.csv("Flowshed_Modeling_InputData/ClassLv4_BenthicLookup_V1_Rank_fixed_NOland_Clean.csv")
#translates benthic categories in the unified reef map into target taxonomy

# List bow tie files (14 day backward and forward extended triangles)
bow_fl = list.files("Flowshed_Modeling_InputData/bow_ties_shp_extended_tri_ALL/14days", full.names = T) #1376 full file version
# length(bow_fl) #1376

# Loop through each bow tie shapefile and buffer out/in to create a more complete "footprint" of the water mass.
# Without the buffer out/in step, there can be lots of discontinuous polygons if the particle "jumped" between time points and did not occupy ever mesh grid cell. 

yearBows = NULL  #note: we call it yearBows because in a previous version I looped through each year first (easier to keep naming consistent)

for (f_i in 1:length(bow_fl)) {
  tick = Sys.time()  
  splitBFile = str_split(string = bow_fl[f_i], "/")
  # layerName = splitBFile[[1]][10]
  layerName = splitBFile[[1]][4]
  layerName
  
  bows=NULL
  bows.original = st_read(dsn = bow_fl[f_i], layer = layerName)
  bows_utm17 = st_transform(bows.original, 26917)
  
  #Concave buffer out, then back in
  D = 300 #(buffer width in meters)
  bows.bo = st_buffer(bows_utm17, dist = D, nQuadSegs = 30)
  bows.bi = st_buffer(bows.bo, dist = -D, nQuadSegs = 30)


  ## pull visitID information and separate out year
  splitName = str_split(string = layerName, "_")
  visitID = sub("_[^_]*$", "", layerName)  #drop "_14days"
  ID_cuTime = sub("_[^_]*$", "", visitID)   
  ID_date = strsplit(ID_cuTime, "_(?!.*_)", perl = TRUE)[[1]]
  Date = ID_date[-1]
  Year = substr(Date, 1, 4)
  
  bows=bows.bi
  bows$year = Year

  ## isolate year from the name? since sites are sometimes separated by _)
  bows$visitID = visitID
  bows$duration = bows$n_days
  bows$bowID = paste0(bows$visitID, "_", bows$simu, "_", bows$n_days)

  yearBows =  rbind(yearBows, bows)

  print(paste0(f_i, ' of ', length(bow_fl)))
  tock = Sys.time()  
  #calculate estimated time remaining
  elmin = round(difftime(tock, tick, units = 'secs'), 1)
  print(paste0("Completed ", f_i," of ", length(bow_fl), " in ",elmin, " seconds. Estimated time remaining: ",(length(bow_fl) - f_i) * elmin / 60, ' min. Current time: ',Sys.time()))
        
}

# ## Save output file (intermediate)
# ## The dataframe is too large to save as a single file/folder
# ## Instead save it as four separate shapefiles
# 
# length(yearBows)
# #38612x8 
# # 38612/4 #9653
# 
# ## save in 4 parts
# rows=nrow(yearBows)
# d=4
# 
# y1=yearBows[1:(rows/d),] 
# y2=yearBows[(rows/d+1):(2*rows/d),] 
# y3=yearBows[(2*rows/d+1):(3*rows/d),] 
# y4=yearBows[(3*rows/d+1):rows,] 
# 
# # st_write(y1,'Flowshed_Modeling_InputData/Concave14Bows_AllYears_22april2024_y1of4.shp')
# # st_write(y2,'Flowshed_Modeling_InputDataConcave14Bows_AllYears_22april2024_y2of4.shp')
# # st_write(y3,'Flowshed_Modeling_InputData/Concave14Bows_AllYears_22april2024_y3of4.shp')
# # st_write(y4,'Flowshed_Modeling_InputData/Concave14Bows_AllYears_22april2024_y4of4.shp')
# ## These intermediate files are saved in a folder called "yearBows_22april2024"


# ## Plot bows
# mapview(bows_utm17, color='red')+mapview(bows.bo)
# 
# ploThis = bows.bi
# crs(ploThis)
# ploThis_t = st_transform(ploThis,crs='+proj=longlat +datum=WGS84')
# crs(ploThis)
# 
# clrs = rainbow(14, start = 0, end = 0.8)
# clrs_rev <- rev(rainbow(14, start = 0, end = 0.8))
# d=ploThis_t$n_days
# pal = colorNumeric(palette = clrs_rev, domain = min(d):max(d))
# leaflet(data=ploThis_t) %>%
#   addProviderTiles('Esri.WorldImagery') %>%
#   addPolygons(data=subset(ploThis_t,simu=='backward'),fillOpacity=0.1,weight=1,color = ~pal(d))



# ________________________________ADD BENTHIC INDICES_____________________________________

## Read in yearBows (if not already loaded from running the section above)
shp.list = list.files(path = paste0("Flowshed_Modeling_InputData/yearBows_22april2024"), pattern="*shp", full.names = T) 
length(shp.list)
shapefile_list = lapply(shp.list,read_sf)
all.yearBows = rbind(shapefile_list[[1]],shapefile_list[[2]],shapefile_list[[3]],shapefile_list[[4]])

yearBows=all.yearBows
# dim(yearBows) #38612
# class(yearBows)

benthicBows=NULL
benthicBows=yearBows
# dim(benthicBows)
# table(benthicBows$n_days)

benthicBows = benthicBows[which(benthicBows$simu == 'backward'), ] #don't need forward bows since we only care where water came from.
# dim(benthicBows) #19306
# table(benthicBows$n_days)

## Initialize benthic index columns
benthicBows[, 'CALCi.m2'] = NA
benthicBows[, 'ALGi.m2'] = NA
benthicBows[, 'SGi.m2'] = NA
benthicBows[, 'NBi.m2'] = NA
benthicBows[, 'NCo.m2'] = NA
benthicBows[, 'PercentCheck'] = NA
# head(benthicBows)


## Tranform coral_sf to match Bbow CRS:
coral_sf.t = st_transform(coral_sf, st_crs(benthicBows))
coral_sf.tmv = st_make_valid(coral_sf.t)
coral_sf.tv = coral_sf.tmv[which(st_is_valid(coral_sf.tmv)),] #only use valid polygons


## check/confirm validity of coral_sf.tmv - reef map habitat polygons after transformation and making valid
which(!st_is_valid(coral_sf.tmv)) #none - all are valid
#check validity of benthicBows
which(!st_is_valid(benthicBows)) #none - all are valid


for (b_i in 1:nrow(benthicBows)) {
  
  #***edit loop to optimize intersection
  #do extent (Bbow)
  #st_crop on coral_sf (only works for bounding box)

  Bbow = benthicBows[b_i, ]
  
  print(paste0('starting bow ', b_i, ' of ', nrow(benthicBows)))
  tick = Sys.time()
  
  #crop coral map
  coral_crop = st_crop(coral_sf.tv,st_bbox(Bbow))

  over_crop_bow = st_intersection(coral_crop, Bbow)
  #get intersection of every independent polygon in habitat map with my bowtie

  print('done with intersection')
  
  over_bow=over_crop_bow
  
  #Get area of each benthic class
  over_bow$class_area = as.numeric(st_area(over_bow)) #m2 get actually area of segmented polygon
  bowComp = over_bow %>% st_drop_geometry() %>% dplyr::select(ClassLv4, class_area) %>% group_by(ClassLv4) %>% summarize(a =sum(class_area, na.rm = TRUE))
  #each individual class in the bowtie now has a summed area
  #removed shape_area from select()
  bowComp$p = bowComp$a / st_area(Bbow) #!!change to Bbow.t if I do the tranformation in the other direction
  bowComp %>% arrange(desc(p))
  #same as desc(a) 
  print(paste0('Percent sum check: ', sum(bowComp$p), ' (should be <1)'))
  

  BowWeight = left_join(bowComp, benthicLU, by = 'ClassLv4')
  
  #biomass index for each class
  BowWeight$Ci = BowWeight$a * BowWeight$Coral
  BowWeight$Ai = BowWeight$a * BowWeight$Algae 
  BowWeight$Si = BowWeight$a * BowWeight$Seagrass
  BowWeight$NBi = BowWeight$a * BowWeight$NotBio
  BowWeight$NCo = BowWeight$a * BowWeight$BioNoCoral #should be sum of seagrass and algae
  
  #summarize for each biomass index (all classes contribute to each)
  benthicBows$CALCi.m2[b_i] = sum(BowWeight$Ci, na.rm = T)
  benthicBows$ALGi.m2[b_i] = sum(BowWeight$Ai, na.rm = T)
  benthicBows$SGi.m2[b_i] = sum(BowWeight$Si, na.rm = T)
  benthicBows$NBi.m2[b_i] = sum(BowWeight$NBi, na.rm = T)
  benthicBows$NCo.m2[b_i] = sum(BowWeight$NCo, na.rm = T)
  benthicBows$PercentCheck[b_i] = sum(bowComp$p)
  
  
  # bow_area = st_area(test)
  # sum(BowWeight$a)/bow_area #(it should be less than 1)
  # sum(BowWeight$p)
  
  tock = Sys.time()
  elmin = round(difftime(tock, tick, units = 'mins'), 1)
  
  print(
    paste0(
      "Completed ",
      b_i,
      " of ",
      nrow(benthicBows),
      " in ",
      elmin,
      " min. Estimated time remaining: ",
      (nrow(benthicBows) - b_i) * elmin,
      ' min. Current time: ',
      Sys.time()
    )
  )
  
  
  
} #end loop to pull benthic info

# dim(benthicBows)
# class(benthicBows)
# head(benthicBows)
# summary(benthicBows$PercentCheck)
# max(benthicBows$PercentCheck) #should not be above 1

# st_write(benthicBows,'Flowshed_Modeling_InputData/Concave_BowBenthic_14days.shp')
## my intermediate file for this step is 'Concave_BowBenthic_14days_26april2024.shp'


