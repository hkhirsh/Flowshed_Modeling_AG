## Try determining density of coverage by flowsheds for different areas. 



rm(list=ls())
library(PNWColors)
library(colorspace)
library(RColorBrewer)
library(ghibli)
library(htmlwidgets)
library(sf)
library(magick)
library(gtools)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(ggrepel)
library(rstudioapi)
library(stringr)
library(sp)
library(raster)
library(mapview)
library(leaflet)
library(dplyr)
library(ggplot2)
library(webshot)

#######_____________________________________________
FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')
# FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')

## Read in carbonate chemistry station data
CC = read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/CCflk_plusBathy.csv',na.strings=c(""," "))

#subset the carb chem data to represent the years we want and a subset of SiteIDs
CCyr = subset(CC,Year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')) #subset of chemistry samples that match bow ties:
## assign visitID columns to allow us to pair CC to the spatial polygon data:
CCyr$visitID_ch1 =  paste(CCyr$SiteID,CCyr$UTCDate_Time)
CCyr$visitID_ch2 =    gsub(" ", "_" , CCyr$visitID_ch1, perl=TRUE)
CCyr$visitID =  gsub("[: -]", "" , CCyr$visitID_ch2, perl=TRUE)

plotCCyr = st_as_sf(CCyr, coords = c("Longitude","Latitude"), crs = st_crs(4326))
# mapview(plotCCyr,zcol="SiteID")
# plotIN = subset(plotCCyr,Zone=='Inshore')
# unique(plotIN$Zone)
# mapview(plotIN,zcol="SiteID")

# #Simplify CCyr to just the columns required for plotting: 
CCyr.s = CCyr[c('visitID','SiteID','Latitude', 'Longitude','dec.lat','dec.lon','Year','Sub_region','Zone','Season','Month','MY','MoY','pointDepth','Precipitation')]

## include shelf points
PTS = read.csv('/Users/heidi.k.hirsh/Documents/FLK_Model1/CC_dataframes/PTS.csv')
PTS$Lat=PTS$Latitude_Dec_Deg
PTS$Long=PTS$Longitude_Dec_Deg
PTS=st_as_sf(PTS, coords = c("Long","Lat"),crs = st_crs(4326)) 
iwant = c("SR","MC") #limit to shark river and middle cape
PTSsub= subset(PTS,line_id %in% iwant)
PTSsub= subset(PTSsub,Latitude_Dec_Deg>25) #remove 2 points in the keys
PTSsub5=subset(PTSsub,bathymetry>5) #only use points where depth is greater than 5m
plotShelfa = subset(PTSsub5,Year_UTC==2018) #don't need duplicate points, and 2018 has all 6
unique(plotShelfa$Year_UTC)
plotShelf = subset(plotShelfa,Month_UTC==5)
# mapview(plotShelf)

#######_____________________________________________

##only test on inshore first (load concave.bo for inshore stations)
load("/Users/heidi.k.hirsh/Desktop/SaveBufferedBows/inshoreAllYrs_concave_bi.RData")

plotData = concave.bi
# dim(plotData)
# plotData_t = st_transform(plotData,crs='+proj=longlat +datum=WGS84')
# unique(plotData$SiteID)
# plotData_t$Season = factor(plotData$Season, levels = c("Winter","Spring","Summer","Fall"))
# levels(plotData_t$Season)

#######_____________________________________________
##site color palette
nColor <- length(unique(plotData$SiteID))
# nColor

#!!Replace greens with standalone visible color!!
# clrs0 = rainbow(nColor)
# scales::show_col(clrs0)
clrs = rainbow(nColor, start = 0, end =0.75)
# scales::show_col(clrs)

#SIMPLE
site_colors <- clrs
site_COLOR_scale  <- scale_color_manual(name = "Site", values = site_colors,
                                        breaks = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'),
                                        labels = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'))

site_FILL_scale  <- scale_fill_manual(name = "Site", values = site_colors,
                                      breaks = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'),
                                      labels = c('1','EK_IN','4','5','UK_IN','7','10','13','16','19','24'))

#######_____________________________________________

## year color scale
nYears=length(unique(plotData$Year))
nYears
# pnw=pnw_palette(name="Bay",n=nyears,type="discrete")
# pnw
Yclrs = rainbow(nYears, start = 0, end =0.75)
scales::show_col(Yclrs)


colors <- hcl.colors(9, palette = "Set2") 
scales::show_col(colors)
pnw=pnw_palette(name="Bay",n=14,type="continuous")
# pnw


#SIMPLE
year_colors <- colors
year_COLOR_scale  <- scale_color_manual(name = "Year", values = year_colors,
                                        breaks = c('2012','2014','2015','2016','2017','2018','2019','2020','2021'),
                                        labels = c('2012','2014','2015','2016','2017','2018','2019','2020','2021'))

year_FILL_scale  <- scale_fill_manual(name = "Year", values = year_colors,
                                      breaks = c('2012','2014','2015','2016','2017','2018','2019','2020','2021'),
                                      labels = c('2012','2014','2015','2016','2017','2018','2019','2020','2021'))
#######_____________________________________________
plotData_t = st_transform(plotData,crs='+proj=longlat +datum=WGS84')
unique(plotData$SiteID)
plotData_t$Season = factor(plotData$Season, levels = c("Winter","Spring","Summer","Fall"))
levels(plotData_t$Season)

## for looping
# plotData_t$Season = factor(plotData$Season, levels = c("Winter","Spring","Summer","Fall"))
sites=unique(plotData_t$SiteID)
Yrs = unique(plotData_t$Year)
Seas= levels(plotData_t$Season)
Reg = unique(plotData_t$Sub_region)
nn=7


for (s_i in 1:length(Seas)) {
  timeSeason=Seas[s_i]
  ploThis1 = subset(plotData_t,Season==timeSeason)
  
  ploThis = subset(ploThis1,n_days==nn)
  unique(ploThis$Year)
  
  BB = ggplot()+
    geom_sf(data=FLKs1,fill = "gray17", lwd = 0)+                                           #Florida shoreline
    geom_sf(data=ploThis,aes(color=as.factor(Year)),inherit.aes = FALSE,alpha=0)+
    year_COLOR_scale+
    # year_FILL_scale+                                                         #Color points by unique site ID
    geom_point(data=CCyr.s,aes(x=dec.lon,y=dec.lat),color="black",size=.3)+                 #Plot all stations
    geom_point(data=plotShelf,aes(x=Longitude_Dec_Deg,y=Latitude_Dec_Deg),color="black",size=.3)+  #add West Florida Shelf points
    geom_point(data=ploThis,aes(x=dec.lon,y=dec.lat,fill=SiteID),color='black',pch=21,size=1.5,stroke=1)+  #plot points associated with flowsheds in plot
    # facet_wrap(~Year,nrow=4)+
    ylab('Latitude')+ xlab('Longitude')+
    # coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
    coord_sf(xlim = c(-82.7, -80), ylim = c(24.35, 26), expand = FALSE) + #for backward halos
    ggtitle(paste0(timeSeason," backward trajectories (",nn,"-day)"))+
    theme_bw()+
    theme(axis.line = element_line(color='black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  BB
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/FlowshedAnalysis/FacetSeasonYear_Saturday/Inshore7day_",timeSeason,"_YearColor3.png"),plot=BB,width=10, height=10, dpi = 300)
}



#### Now I want to know which areas are covered most by bowties: 
#you can use cumulative spatial overlays or density visualizations
#fullinshore data plotData_t
dim(plotData_t)

small = subset(plotData_t, n_days==7 & Year==2018 & Month=="Aug" & Zone=="Inshore" & Sub_region=='MK')
dim(small)
head(small)

smallish = subset(plotData_t, Year==2018 & Month=="Aug" & Zone=="Inshore" & Sub_region=="LK")# n_days==7
dim(smallish)
your_sf_dataframe = smallish[1:4,]
dim(your_sf_dataframe)

p1=small[1,]
p2=small[2,]
p3=small[3,]

# poly1=vect(p1$geometry)
# poly2=vect(p2$geometry)
# poly3=vect(p3$geometry)

poly1=p1
poly2=p2
poly3=p3

class(poly1) #these are dataframe rows with everything (including geometry)

### METHOD A: stack and overlap polygons

# # Example: Polygons for two time periods
# poly1 <- st_read("polygon_time1.geojson")
# poly2 <- st_read("polygon_time2.geojson")

# Make an example dataset out of two polygons.

# Assign a time or count attribute to each polygon
poly1$time <- 1
poly2$time <- 1
poly3$time <- 1

# Combine polygons
all_polys <- rbind(poly1, poly2, poly3)
class(all_polys)
all_polys = small
class(all_polys)
# all_polys <- rbind(poly1, poly2)
# Dissolve and count overlaps
overlap <- all_polys %>%
  dplyr::group_by(geometry) %>%
  summarise(frequency = sum(time))  # Count overlap frequencies #this would be the number of times. (This doesn't work with my version of example data)

# Plot overlap# Plot overlappoly3
library(ggplot2)
ggplot(data = overlap) +
  geom_sf(aes(fill = frequency)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Overlap Frequency Map", fill = "Frequency")

#frequency was artificially assigned here so doesn't mean anything.I think this example really only works for overlapping 2.



#### B: Rasterize overlap
library(terra)

# Example Polygons
# poly1 <- vect("polygon_time1.geojson")
# poly2 <- vect("polygon_time2.geojson")

# Convert to Raster with Specific Resolution
rast <- rast(ext(poly1), resolution = 0.01)
poly1_rast <- rasterize(poly1, rast, field = 1)  # Rasterize first polygon
poly2_rast <- rasterize(poly2, rast, field = 1)  # Rasterize second polygon
poly3_rast <- rasterize(poly3, rast, field = 1)  # Rasterize second polygon


# Stack and Sum Raster Layers
# overlap_raster <- sum(poly1_rast, poly2_rast, na.rm = TRUE)
overlap_raster <- sum(poly1_rast,poly2_rast,poly3_rast, na.rm = TRUE)
plot(overlap_raster)

##better rasterization for number of overlapping polygons (loses some of the specificity though (and would cover last))

# Visualize the Raster
library(tmap)
tm_shape(overlap_raster) +
  tm_raster(title = "Frequency", palette = "-viridis") +
  tm_layout(main.title = "Overlap Frequency") 

### use full dataframe instead (beyond 2-3 polygons)
dim(plotData_t)

small = subset(plotData_t, n_days==7 & Year==2018 & Month=="Aug" & Zone=="Inshore" & Sub_region=='MK')
your_sf_dataframe = small

##rasterize polygons for overlap analysis: 
# Define a raster template based on the extent and resolution
raster_template <- rast(ext(your_sf_dataframe), resolution = 0.01)

plot(raster_template)
class(raster_template)

# Rasterize polygons grouped by year
rasterized_by_year <- your_sf_dataframe %>%
  group_by(Year) %>%
  group_map(~ rasterize(.x, raster_template, field = 1, fun = "count"))  # A list of rasters



# Combine rasters to calculate cumulative overlap
overlap_raster <- do.call(sum, rasterized_by_year)

# Normalize by maximum overlap value
normalized_overlap <- overlap_raster / max(values(overlap_raster), na.rm = TRUE)

## visualize: 
overlap_df <- as.data.frame(overlap_raster, xy = TRUE)
colnames(overlap_df) <- c("x", "y", "frequency")

library(ggplot2)
ggplot(overlap_df, aes(x = x, y = y, fill = frequency)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Overlap Frequency") +
  labs(title = "Spatial Overlap Heatmap") +
  coord_fixed() +
  theme_minimal()

##OR
library(tmap)
tm_shape(overlap_raster) +
  tm_raster(title = "Overlap Frequency", palette = "plasma") +
  tm_layout(title = "Spatial Overlap")


#### compare without rasterizing: 
#calculate intersection: 
smallish = subset(plotData_t, Year==2018 & Month=="Aug" & Zone=="Inshore" & Sub_region=="LK")# n_days==7
dim(smallish)
your_sf_dataframe = smallish[1:4,]
dim(your_sf_dataframe)

# # Simplify geometries to reduce complexity
# your_sf_dataframe <- st_simplify(your_sf_dataframe, dTolerance = 0.01)

# # Check for invalid geometries
# invalid_geometries <- st_is_valid(your_sf_dataframe)
# invalid_geometries 
# # View invalid geometries
# your_sf_dataframe[!invalid_geometries, ]
# 
# # Fix invalid geometries
# your_sf_dataframe <- st_make_valid(your_sf_dataframe)
# # View(your_sf_dataframe)


# # Convert to a consistent geometry type, e.g., multipolygons
# your_sf_dataframe <- st_cast(your_sf_dataframe, "MULTIPOLYGON")
# 
# # Reproject to a common CRS (if needed)
# your_sf_dataframe <- st_transform(your_sf_dataframe, crs = 4326)


# ##debug: 
# problematic_rows <- your_sf_dataframe[!st_is_valid(your_sf_dataframe), ]
# print(problematic_rows)
# plot(st_geometry(problematic_rows))

# Assume your_sf_dataframe has a 'year' column and geometry
# Calculate intersections
overlaps <- your_sf_dataframe %>%
  st_intersection() %>%
  mutate(overlap_count = row_number())  # Optional: Track number of overlaps
# overlaps
# plot(overlaps)
dim(overlaps)


# Count how many polygons overlap each area
overlap_summary <- overlaps %>%
  group_by(geometry) %>%
  summarise(frequency = n())  # Frequency of overlaps

# Plot overlapping polygons with color intensity representing frequency
ggplot(overlap_summary) +
  geom_sf(aes(fill = frequency)) +
  scale_fill_viridis_c(name = "Overlap Frequency") +
  theme_minimal() +
  labs(title = "Polygon Overlap Frequency")

##how are they all overlap frequency =1? 
#some must overlap with multiple right? 


#### group overlaps: 
# Add a 'year' column to overlaps
overlaps_grouped <- overlaps %>%
  group_by(year, geometry) %>%
  summarise(frequency = n())

# Plot grouped overlaps
ggplot(overlaps_grouped) +
  geom_sf(aes(fill = frequency)) +
  facet_wrap(~year) +
  scale_fill_viridis_c(name = "Overlap Frequency") +
  theme_minimal() +
  labs(title = "Overlap Frequency by Year")


# 
# 
# #### further debugging: 
# # Identify invalid geometries
# invalid <- st_is_valid(your_sf_dataframe)
# 
# # View invalid geometries
# problematic_geometries <- your_sf_dataframe[!invalid, ]
# print(problematic_geometries)
# 
# # Fix invalid geometries
# your_sf_dataframe <- st_make_valid(your_sf_dataframe)
# 
# ##everything is valid
# ## try detecting self intersection: 
# self_intersections <- st_intersects(your_sf_dataframe, sparse = FALSE)
# diag(self_intersections) <- FALSE  # Exclude self-matching
# problematic <- which(rowSums(self_intersections) > 0)
# 
# # Inspect problematic polygons
# st_geometry(your_sf_dataframe[problematic, ])
# 
# # Split problematic geometries into smaller segments
# your_sf_dataframe <- st_make_valid(your_sf_dataframe)
# splits <- st_split(your_sf_dataframe, st_geometry(your_sf_dataframe))
# your_sf_dataframe <- do.call(rbind, splits)
# 
# # Simplify all geometries to reduce computational load
# your_sf_dataframe <- st_simplify(your_sf_dataframe, dTolerance = 0.001, preserveTopology = TRUE)


library(terra)
### go back to rasterizing... 
polygons=smallish
# Create a raster template
extent_area <- st_bbox(polygons) # Get bounding box of the polygons
raster_template <- rast(extent = extent_area, resolution = 100, crs = st_crs(polygons)$proj4string)

# Rasterize polygons and compute the overlap frequency
overlap_raster <- rasterize(polygons, raster_template, fun = function(vals) length(vals), background = 0).  ##not working

# Plot the overlap frequency raster
plot(overlap_raster, main = "Polygon Overlap Frequency")

# Save the raster to a file
writeRaster(overlap_raster, "overlap_frequency.tif", format = "GTiff")



#### different approach: 
# Convert polygons to terra's SpatVector
polygons <- st_transform(polygons, crs = 32633)

polygons_vect <- vect(polygons)
# Create an empty raster template
raster_template <- rast(extent = ext(polygons_vect), resolution = 100, crs = crs(polygons_vect))
# Rasterize each polygon individually
rasters_list <- lapply(1:length(polygons_vect), function(i) {
  rasterize(polygons_vect[i, ], raster_template, field = 1, background = 0)
})

# Stack and sum the rasters to calculate overlap frequency
rasters_stack <- rast(rasters_list)
overlap_raster <- sum(rasters_stack)
# Plot the overlap frequency raster
plot(overlap_raster, main = "Polygon Overlap Frequency")

# # Save the raster to a file
# writeRaster(overlap_raster, "overlap_frequency.tif", format = "GTiff")




##### another try: # Convert the spatial dataframe (sf) to a SpatVector
polygons = smallish
polygons_vect <- vect(polygons)  # 'polygons' is your sf dataframe

# Define a raster template for the entire dataset
raster_template <- rast(ext(polygons_vect), resolution = 0.01, crs = crs(polygons_vect))

# Rasterize each polygon and store in a list
rasters_list <- lapply(1:nrow(polygons_vect), function(i) {
  rasterize(polygons_vect[i, ], raster_template, field = 1, background = 0)
})
# Stack all rasters
rasters_stack <- rast(rasters_list)

# Calculate the overlap frequency
overlap_raster <- sum(rasters_stack, na.rm = TRUE)

# Plot the overlap frequency raster
plot(overlap_raster, main = "Polygon Overlap Frequency")


#### I might need to chunkify: 
# Process in chunks of 1000 polygons
chunk_size <- 1000
n_chunks <- ceiling(nrow(polygons_vect) / chunk_size)

# Initialize an empty raster for summing results
overlap_raster <- init(raster_template, 0)

for (chunk in 1:n_chunks) {
  # Subset polygons for the current chunk
  start_idx <- (chunk - 1) * chunk_size + 1
  end_idx <- min(chunk * chunk_size, nrow(polygons_vect))
  subset_polygons <- polygons_vect[start_idx:end_idx, ]
  
  # Rasterize and sum the subset
  rasters_list <- lapply(1:length(subset_polygons), function(i) {
    rasterize(subset_polygons[i, ], raster_template, field = 1, background = 0)
  })
  overlap_raster <- overlap_raster + sum(rast(rasters_list), na.rm = TRUE)
}

# Save the raster to a file
# writeRaster(overlap_raster, "overlap_frequency.tif", format = "GTiff", overwrite = TRUE)

# ggplot()+
#  geom_spatraster(overlap_raster)

# need to convert to dataframe to plot in ggplot
library(terra)
library(ggplot2)
FLKs1=st_read('/Users/heidi.k.hirsh/Documents/FLK_Model1/FLK_data/Florida_Shoreline_(1_to_12%2C000_Scale)/Florida_Shoreline_(1_to_12%2C000_Scale).shp')

# Assuming overlap_raster is your SpatRaster
raster_df <- as.data.frame(overlap_raster, xy = TRUE)  # Include coordinates (x, y)

# Check the structure of the data
head(raster_df)

ggplot(data = raster_df, aes(x = x, y = y, fill = sum)) +  # Replace 'lyr.1' with your column name
  geom_raster() +
  # geom_sf(data = FLKs1, fill = "darkgray", lwd = 0)+          
  scale_fill_viridis_c(option = "viridis") +  # Use a color scale
  coord_equal() +  # Ensure square pixels
  theme_minimal() +
  labs(title = "Polygon Overlap Frequency",
       fill = "Frequency")

ggplot() +
  geom_sf(data=FLKs1,fill = "black", lwd = 0)+
  geom_raster(data = raster_df, aes(x = x, y = y, fill = sum),alpha=.6) +  # Plot the raster data
  scale_fill_viridis_c(option = "magma") +  # Adjust color scale
  # geom_sf(data = FLKs1, color = "blue", size = 0.5) +  # Add the shoreline
  # coord_equal() +
  coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
  theme_minimal() +
  labs(title = "Polygon Overlap Frequency",
       x = "Longitude",
       y = "Latitude",
       fill = "Overlap Count")


# ggplot(data = raster_df, aes(x = x, y = y, fill = lyr.1)) +
#   geom_raster() +  # Plot the raster
#   scale_fill_viridis_c(option = "magma") +  # Adjust color scale
#   geom_sf(data = shoreline, color = "blue", size = 0.5) +  # Add shoreline
#   coord_sf(crs = st_crs(raster_df)) +  # Use coord_sf with the correct CRS
#   theme_minimal() +
#   labs(title = "Polygon Overlap with Shoreline",
#        x = "Longitude",
#        y = "Latitude",
#        fill = "Overlap Count")

# ggplot()+
#   geom_sf(data=FLKs1,fill = "darkgray", lwd = 0)
#   


## what I want: loop through overlapping polygons for different seasons and years to compare variability or hotspots where what always comes from. 
#Code that worked (ish): 

#select a subset of polygons of interest. 
smallish = subset(plotData_t, Year==2018 & Month=="Aug" & Zone=="Inshore" & Sub_region=="LK")# n_days==7

subsetPolys = subset(plotData_t, Year==2018 & Season=="Winter" & Zone=="Inshore" & Sub_region=="LK")# n_days==7
subsetPolys = subset(plotData_t, Year==2018 & Season=="Summer" & Zone=="Inshore" & Sub_region=="LK")# n_days==7

polygons = subsetPolys
polygons_vect <- vect(polygons)  # 'polygons' is your sf dataframe

# Define a raster template for the entire dataset
raster_template <- rast(ext(polygons_vect), resolution = 0.01, crs = crs(polygons_vect))

# Rasterize each polygon and store in a list
rasters_list <- lapply(1:nrow(polygons_vect), function(i) {
  rasterize(polygons_vect[i, ], raster_template, field = 1, background = 0)
})
# Stack all rasters
rasters_stack <- rast(rasters_list)

# Calculate the overlap frequency
overlap_raster <- sum(rasters_stack, na.rm = TRUE)

# ##Plot the overlap frequency raster
# plot(overlap_raster, main = "Polygon Overlap Frequency")



###Good plot: 
#improvements. loop through and label plot with year, season etc. 
#add site points. 
#basically run polygon plotting loops but loop through raster overlap in each loop iteration. 


ggplot() +
  geom_sf(data=FLKs1,fill = "black", lwd = 0)+
  geom_raster(data = raster_df, aes(x = x, y = y, fill = sum),alpha=.6) +  # Plot the raster data
  scale_fill_viridis_c(option = "magma") +  # Adjust color scale
  # geom_sf(data = FLKs1, color = "blue", size = 0.5) +  # Add the shoreline
  # coord_equal() +
  # facet_wrap(~Sub_region,nrow=4)+
  coord_sf(xlim = c(-82.7, -80), ylim = c(24, 26), expand = FALSE) + #for backward halos
  theme_minimal() +
  labs(title = "Polygon Overlap Frequency",
       x = "Longitude",
       y = "Latitude",
       fill = "Overlap Count")
