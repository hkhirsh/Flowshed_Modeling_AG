#Editing to rerun with ALL (none missing) bowties, April 2024
# Review script Dec 13, 2024 (what is loaded and written)


rm(list = ls()) #clear environment

# Load packages ----
# packageload <- c("sf","tidyverse","ggmap","ggrepel","rstudioapi","stringr","sp","raster","rgeos","patchwork","mapview","leaflet","ggplot2")   #plotting/mapping packages: "ggmap","ggplot2","patchwork","mapview","leaflet"
packageload <- c("sf","stringr","raster","dplyr")
lapply(packageload, library, character.only = TRUE)

# ----Check which packages we actually use----
# Find which packages do used functions belong to ----
used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/test-flk/SaveBows&Benthos+Bathy_Concave_28Dec2023.R", alphabetic = FALSE) |> print()

# Find which loaded packages are not used ----
used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
unused.packages <- packageload[!(packageload %in% used.packages)] |> print()


#####______________________________________#####


# Read in FL unified reef map habitat data:
coral_sf =  st_read(dsn = "/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Final_UnifiedReefMap_Version2.0/FWC_UnifiedFloridaReefMap_v2.0.gdb", layer = "UnifiedReefMap")

# Load benthic composition lookup table:
benthicLU = read.csv("/Users/heidi.k.hirsh/Documents/Git_Projects/FLK-Modeling-Hirsh-copy-DONTTOUCH/ClassLv4_BenthicLookup_V1_Rank_fixed_NOland_Clean.csv")
#translates benthic categories into target taxonomy

# List bow tie files (14 day backward and forward extended triangles)
bow_fl = list.files(path = paste0("/Users/heidi.k.hirsh/Desktop/fromDobby/BowTies/bow_ties_shp_extended/bow_ties_shp_extended_tri_ALL/14days" ), full.names = T) #New 1376 full file version
# bow_fl = list.files(path = paste0("/Users/heidi.k.hirsh/Desktop/fromDobby/BowTies/bow_ties_shp_extended/bow_ties_shp_extended_tri/14days" ), full.names = T) #1220 file version
# bow_fl = list.files(path = paste0("/Users/heidi.k.hirsh/Desktop/bow_ties_shp_extended/bow_ties_shp_extended_tri/14days" ), full.names = T)

# length(bow_fl) #1220 #why only 1220 (not 1376??)
#should now be 1376
length(bow_fl)


# Loop through each bow tie shapefile and buffer out/in to create a more complete "footprint" of the water mass
#running this loop at 10:28am on April 22, 2024
#at 11:46 it is at 687/1376
#finish at 13:34
#DON'T RERUN during check. 

yearBows = NULL  #note: we call it yearBows because in a previous version I looped through each year first

# f_i=1
#run subset
# for (f_i in 1:3) {
  

for (f_i in 1:length(bow_fl)) {
  tick = Sys.time()  
  splitBFile = str_split(string = bow_fl[f_i], "/")
  # splitBFile
  layerName = splitBFile[[1]][10]
  # layerName
  
  bows=NULL

  bows.original = st_read(dsn = bow_fl[f_i], layer = layerName)
  # head(bows.original)
  # dim(bows.original)

  #concave buffer out and in
  bows_utm17 = st_transform(bows.original, 26917)
  # View(bows_utm17)
  # class(bows_utm17$geometry)
  # plot(bows_utm17)
  # mapview(bows_utm17)
  crs(bows_utm17) #NAD83
  # 
  D = 300 #(buffer width in meters)
  bows.bo = st_buffer(bows_utm17, dist = D, nQuadSegs = 30)
  bows.bi = st_buffer(bows.bo, dist = -D, nQuadSegs = 30)
  # View(bows.bi)

  #pull visitID information and separate out year
  splitName = str_split(string = layerName, "_")
  # splitName
  visitID = sub("_[^_]*$", "", layerName)  #drop "_14days"
  # visitID
  ID_cuTime = sub("_[^_]*$", "", visitID)   
  # ID_cuTime 
  ID_date = strsplit(ID_cuTime, "_(?!.*_)", perl = TRUE)[[1]]
  # ID_date
  Date = ID_date[-1]
  # Date
  Year = substr(Date, 1, 4)
  # Year
  
  bows=bows.bi
  bows$year = Year
  # bows$year
  #how can I isolate year from the name? since sites are sometimes separated by _)
  bows$visitID = visitID
  # bows$visitID
  bows$duration = bows$n_days
  # bows$duration
  bows$bowID = paste0(bows$visitID, "_", bows$simu, "_", bows$n_days)
  # bows$bowID  

  # head(bows)

  yearBows =  rbind(yearBows, bows)
  
  # dim(yearBows)
  print(paste0(f_i, ' of ', length(bow_fl)))
  tock = Sys.time()  
  #calculate estimated time remaining
  elmin = round(difftime(tock, tick, units = 'secs'), 1)
  print(paste0("Completed ", f_i," of ", length(bow_fl), " in ",elmin, " seconds. Estimated time remaining: ",(length(bow_fl) - f_i) * elmin / 60, ' min. Current time: ',Sys.time()))
        
}

dim(yearBows)

#save output file
# st_write(yearBows,'/Users/heidi.k.hirsh/Desktop/Concave14Bows_AllYears_28dec223.shp')
# st_write(yearBows,'/Users/heidi.k.hirsh/Desktop/Concave14Bows_AllYears_run22april2024.shp')

#saving failed for all at once
length(yearBows)
#38612x8 
38612/4 #9653


# y1=yearBows[1:(rows/4),] #1:7722
# st_write(y1,'/Users/heidi.k.hirsh/Desktop/Concave14Bows_AllYears_22april2024_y1of4.shp')
#save in 4 parts
rows=nrow(yearBows)
d=4

y1=yearBows[1:(rows/d),] 
y2=yearBows[(rows/d+1):(2*rows/d),] 
y3=yearBows[(2*rows/d+1):(3*rows/d),] 
y4=yearBows[(3*rows/d+1):rows,] 

# dim(y1)
# dim(y2)
# dim(y3)
# dim(y4)
# 
# st_write(y1,'/Users/heidi.k.hirsh/Desktop/Concave14Bows_AllYears_22april2024_y1of4.shp')
# st_write(y2,'/Users/heidi.k.hirsh/Desktop/Concave14Bows_AllYears_22april2024_y2of4.shp')
# st_write(y3,'/Users/heidi.k.hirsh/Desktop/Concave14Bows_AllYears_22april2024_y3of4.shp')
# st_write(y4,'/Users/heidi.k.hirsh/Desktop/Concave14Bows_AllYears_22april2024_y4of4.shp')


#don't do volume intersection since ThomasD already calculated volume. 
#Buffer out and back in (to capture shallow areas)
#CRAP. is the volume inaccurate now because I buffered out - not a meaningful difference

#PLOT BOWS
# mapview(bows_utm17, color='red')+mapview(bows.bo)
# 
# # ploThis = bows.original
# # ploThis = bows.bo
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

#***CHECK CRS for combining with other data!


# ________________________________ADD BENTHIC INDICES_____________________________________

# yearBows = st_read('/Users/heidi.k.hirsh/Desktop/Concave14Bows_AllYears_run20april2024.shp')

#Read in yearBows (if not already loaded from running the section above)
shp.list = list.files(path = paste0("/Users/heidi.k.hirsh/Documents/FLK_Model1/yearBows_22april2024"), pattern="*shp", full.names = T) 
length(shp.list)
shapefile_list = lapply(shp.list,read_sf)
all.yearBows = rbind(shapefile_list[[1]],shapefile_list[[2]],shapefile_list[[3]],shapefile_list[[4]])
# dim(all.yearBows)
# summary(all.yearBows)
# class(all.yearBows)

yearBows=all.yearBows

dim(yearBows)
# summary(yearBows)
class(yearBows)
# head(yearBows)


#start benthic overlap loop at 8:10am Dec 29 (estimated 14 hours)
benthicBows=NULL
benthicBows=yearBows
dim(benthicBows)
table(benthicBows$n_days)

benthicBows = benthicBows[which(benthicBows$simu == 'backward'), ] #don't need forward bows
# benthicBows = benthicBows[which(benthicBows$n_days <=7),] #limit to 7 days (or fewer)
dim(benthicBows)
table(benthicBows$n_days)
# unique(benthicBows$simu)
# unique(benthicBows$n_days)

# st_write(benthicBows,'/Users/heidi.k.hirsh/Desktop/7days.backward.yearBows.preBenthicCut.shp')


#initialize benthic index columns
benthicBows[, 'CALCi.m2'] = NA
benthicBows[, 'ALGi.m2'] = NA
benthicBows[, 'SGi.m2'] = NA
benthicBows[, 'NBi.m2'] = NA
benthicBows[, 'NCo.m2'] = NA
benthicBows[, 'PercentCheck'] = NA

head(benthicBows)



#test Bbow
# Bbow = yearBows[1,]

#Bbow is not defined so st_crs for something else must be used below. 

# tick = Sys.time()
#Tranform coral_sf to match Bbow CRS:
coral_sf.t = st_transform(coral_sf, st_crs(benthicBows))
coral_sf.tmv = st_make_valid(coral_sf.t)
#only use valid polygons:
coral_sf.tv = coral_sf.tmv[which(st_is_valid(coral_sf.tmv)),]
# tock = Sys.time()
# elmin = round(difftime(tock, tick, units = 'secs'), 1)
# elmin


#check validity of coral_sf.tmv - reef map habitat polygons after transformation and making valid
which(!st_is_valid(coral_sf.tmv))
#check validity of benthicBows
which(!st_is_valid(benthicBows))

#save this version and load it early

# b_i=1
# nrow(benthicBows) #9653

for (b_i in 1:nrow(benthicBows)) {

#test
# for (b_i in 1:2) {
  
  #***edit loop to optimize intersection
  #do extent (Bbow)
  #st_crop on coral_sf (only works for bounding box)

  Bbow = benthicBows[b_i, ]
  
  print(paste0('starting bow ', b_i, ' of ', nrow(benthicBows)))
  tick = Sys.time()
  # st_crs(Bbow) #D_Unknown_based_on_WGS84_ellipsoid
  # st_crs(coral_sf) #NAD83
  
  #crop coral map
  coral_crop = st_crop(coral_sf.tv,st_bbox(Bbow))

  
  # tick = Sys.time()
  # over_bow = st_intersection(coral_sf.tv, Bbow)
  # tock = Sys.time()
  # elmin = round(difftime(tock, tick, units = 'secs'), 1)
  
  # tick2 = Sys.time()
  over_crop_bow = st_intersection(coral_crop, Bbow)
  #get intersection of every independent polygon in habitat map with my bowtie
  
  # tock2 = Sys.time()
  # elmin2 = round(difftime(tock2, tick2, units = 'secs'), 1)
  # # mapview(Bbow)+mapview(over_bow,color='yellow')
  # elmin
  # elmin2
  # 
  #***get warning: attribute variables are assumed to be spatially constant throughout all geometries 
  #https://github.com/r-spatial/sf/issues/406
  
  # st_crs(over_bow)
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
  
  # mapview(over_bow,zcol="ClassLv4")
  # plot(Bbow)
  
  BowWeight = left_join(bowComp, benthicLU, by = 'ClassLv4')
  # BowWeight$NBTest = 1-BowWeight$Bio
  # BowWeight$NBTestIndex = BowWeight$a*BowWeight$NBTest
  #NONONO BowWeight$NotBio = 1-BowWeight$Bio
  # I think this calculation would include land categories. Instead, use NotBio from LU
  
  #biomass index for each class
  BowWeight$Ci = BowWeight$a * BowWeight$Coral
  BowWeight$Ai = BowWeight$a * BowWeight$Algae #fix spelling!
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
  
  # BowWeight$tIndex = BowWeight$Ci + BowWeight$Ai + BowWeight$Si + BowWeight$NBi  #should this end up being the total area?
  # BowWeight$tIndex - BowWeight$a #why isn't this always 0?
  
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

dim(benthicBows)
class(benthicBows)

head(benthicBows)
summary(benthicBows$PercentCheck)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.1983  0.7509  0.6099  0.9957  1.0003 
max(benthicBows$PercentCheck)
#1.000321

# st_write(benthicBows,'/Users/heidi.k.hirsh/Desktop/Concave_BowBenthic_7days_29dec2023full.shp')
# st_write(benthicBows,'/Users/heidi.k.hirsh/Desktop/Concave_BowBenthic_14days_26april2024.shp')
# st_write(benthicBows,'/Users/heidi.k.hirsh/Desktop/Concave_BowBenthic_14days_26april2024_copy2.shp')



