#summarize model input parameters (this is the testing/exploration file - messier)
#see "SummarizeParams_cln" for more final tables.

rm(list = ls()) #clear environment

library(tidyverse)
library(patchwork)
library(MuMIn)
library(PNWColors)
library(ghibli)
library(ggtext)
library(stringr)
# library(dplyr)
library(ggmap)
library(tidyr)
library(factoextra)
library(sf)
library(mapview)
library(geosphere) #distance between points
library(vegan)
library(ggfortify)
library(lwgeom)
library(gridExtra)
library(ggpubr)
# library(patchwork)
library(cowplot)
# library(PNWColors)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(viridis)
library(ggnewscale)
library(ggpubr)
library(gtsummary)

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('stats')
lapply(packageload, library, character.only = TRUE)

# # ----Check which packages we actually use----
# # Find which packages do used functions belong to ----
# used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/Flowshed_Modeling/SummarizeParams.R", alphabetic = FALSE) |> print()
# # Find which loaded packages are not used ----
# used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
# unused.packages <- packageload[!(packageload %in% used.packages)] |> print()

# CCc %>% tbl_summary #simplify first to the parameters I want

# CCc=read.csv("/Users/heidi.k.hirsh/Desktop/Hirsh_FLKmodel_InputFiles/CC_complete_cases_26april2024.csv")
CCc=read.csv("Flowshed_Modeling_InputData/CC_complete_cases_26april2024.csv")

# unique(CCc$ndays)

# unique(CCc$Sub_region)
# CCc=CCc %>% dplyr::mutate(Sub_region=factor(Sub_region,levels=c("BB","UK","MK","LK")))
# unique(CCc$Sub_region)
# 
# unique(CCc$Zone)
# CCc=CCc %>% dplyr::mutate(Zone=factor(Zone,levels=c("Inshore","Mid channel","Offshore","Oceanic")))
# unique(CCc$Zone)


CClim=select(CCc, c('Zone','Sub_region','Year','PAR_MODIS_MON','pointDepth','vol_km3','Salinity_Bottle','Temperature_C','Chla','NO3','DIC_umol_kg','TA_umol_kg'))
# summary(CClim)
# tbl_summary(CClim)


  
# tbl_summary(CClim)
theme_gtsummary_language("en", big.mark = "") #remove the comma in the numbers

# 
# CCtab <-
#   CCc %>% 
#   # select('Zone','Sub_region','Year','PAR_MODIS_MON','pointDepth','vol_km3','Salinity_Bottle','Temperature_C','Chla','NO3','DIC_umol_kg','TA_umol_kg') %>% 
#   # select('Zone','Sub_region','PAR_MODIS_MON','pointDepth','Salinity_Bottle','Temperature_C','Chla','NO3','DIC_umol_kg','TA_umol_kg') %>% #remove year and volume
#   select('Zone','Sub_region','pointDepth','Temperature_C','Salinity_Bottle','DIC_umol_kg','TA_umol_kg','Chla','NO3','PAR_MODIS_MON') %>% #remove year and volume
#   dplyr::mutate(
#     Sub_region=factor(Sub_region,labels=c("Biscayne Bay","Upper Keys","Middle Keys","Lower Keys"))
#     # Zone=factor(Zone,labels=c("Inshore","Midchannel","Offshore","Oceanic"))
#   ) %>% 
#   dplyr::rename(
#     PAR=PAR_MODIS_MON, TA=TA_umol_kg, DIC=DIC_umol_kg,Chlorophyll=Chla, Nitrate=NO3,Temperature=Temperature_C,Salinity=Salinity_Bottle,Depth=pointDepth
#   ) %>% 
#   tbl_strata(
#     strata=Sub_region,
#     ~.x %>% 
#       tbl_summary(
#         by=Zone,
#         # include = c(PAR_MODIS_MON,pointDepth,vol_km3,Salinity_Bottle,Temperature_C,Chla,NO3,DIC_umol_kg,TA_umol_kg),
#         type = where(is.numeric) ~ "continuous2",
#         # statistic = all_continuous() ~ "{min}-{max}"
#         # statistic = all_continuous() ~ "{min},{max}",
#         # statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
#         statistic = all_continuous() ~ c("{min}", "{max}"),
#        
#         # statistic = list(all_continuous() ~ c("{min}","{max}")),
#         missing="no"
#       ) %>% 
#       # add_n() %>% 
#       modify_header(all_stat_cols() ~ "**{level}**") %>% 
#       modify_header(label="**Parameter**")
#   ) -> CCtab.bySbyZ
#   # ) %>% 
#   # modify_footnote(everything() ~ NA)
# 
# CCtab.bySbyZ
# class(CCtab.bySbyZ)
# 
# # tf= tempfile("table",fileext='.docx')
# CCtab.bySbyZ = CCtab.bySbyZ %>% as_gt()
# # class(CCtab)
# CCtab.bySbyZ %>% gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummTable.png')
# CCtab.bySbyZ %>% gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummTable2.pdf')


#clean up the input data to summarize
CClim2 = 
  CCc %>% 
  select('Zone','Sub_region','pointDepth','Temperature_C','Salinity_Bottle','DIC_umol_kg','TA_umol_kg','Chla','NO3','PAR_MODIS_MON') %>% #remove year and volume
  dplyr::mutate(Sub_region=factor(Sub_region,labels=c("Biscayne Bay","Upper Keys","Middle Keys","Lower Keys"))) %>% 
  dplyr::mutate(Zone=factor(Zone,labels=c("Inshore","Mid-channel","Offshore","Oceanic"))) %>% 
  dplyr::rename(Region=Sub_region,PAR=PAR_MODIS_MON, TA=TA_umol_kg, DIC=DIC_umol_kg,Chlorophyll=Chla, 
                Nitrate=NO3,Temperature=Temperature_C,Salinity=Salinity_Bottle,Depth=pointDepth)
head(CClim2)
# unique(CClim2$Region)
# unique(CClim2$Zone)

# devtools::install_version("glue", version = "1.7.0")
## need to redownload gtsummary (update) or else there is an issue with glue in table function
library(pak)
pak::pkg_install("ddsjoberg/gtsummary", dependencies = TRUE)

##_______Summarize all data together (no zone or region %; separate column for max and min)
TableAll = c("min", "max") %>%
  lapply(
    function(.x) {
      tbl_summary(
        data = CClim2,
        include = c(Depth,Temperature,Salinity,DIC,TA,Chlorophyll,Nitrate,PAR),
        statistic = everything() ~ paste0("{", .x, "}"),
        # .envir = environment(),
        missing = "no"
      ) %>%
        modify_header(all_stat_cols() ~ glue::glue("**{.x}**"))
    }
  ) %>%
  tbl_merge() %>%
  modify_spanning_header(everything() ~ NA) %>%
  modify_footnote(everything() ~ NA) %>%
  modify_header(label="**Parameter**") 
TableAll

# TableAll %>% as_gt() %>% 
#   # gt() %>% cols_width(everything() ~px(150)) %>% 
#   gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeAll_simple.png')
# # gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeAll_simple.docx')



#report range, mean (sd) instead
TableAll = c("min","max","mean","sd") %>%
  lapply(
    function(.x) {
      tbl_summary(
        data = CClim2,
        include = c(Depth,Temperature,Salinity,DIC,TA,Chlorophyll,Nitrate,PAR),
        statistic = everything() ~ paste0("{", .x, "}"),
        missing = "no"
      ) %>%
        modify_header(all_stat_cols() ~ glue::glue("**{.x}**"))
    }
  ) %>%
  tbl_merge() %>%
  modify_spanning_header(everything() ~ NA) %>%
  modify_footnote(everything() ~ NA) %>%
  modify_header(label="**Parameter**") 
TableAll


# trial |> select(age, grade, trt) |> 
#   tbl_summary( 
#     by = trt, 
#     type = all_continuous() ~ "continuous2", 
#     statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}- {max}"), 
#     ) |> 
#   add_stat_label(label = age ~ c("IQR", "Range"))

CClim2 |> select(Depth,Temperature,Salinity,DIC,TA,Chlorophyll,Nitrate,PAR) |> 
  tbl_summary( 
    # by = trt, 
    type = all_continuous() ~ "continuous2", 
    statistic = all_continuous() ~ c("{mean}({sd})", "{min}-{max}"), 
  ) |> 
  add_stat_label(label = ~ c("Mean (sd)", "Range"))


sd_gts <- function(data, variable, ...) {
  sd(data[[variable]], na.rm = TRUE)
}

  trial %>%
  select(trt, age, marker) %>%
  tbl_strata(
    strata = trt, 
    ~.x %>%
      tbl_summary(
        statistic = all_continuous() ~ "{mean} ({sd})",
        missing = "no"
      ) %>%
      modify_footnote(all_stat_cols() ~ NA) %>%
      # add_stat(fns = all_continuous() ~ sd_gts) %>%
      add_stat(fns = all_continuous() ~ "{min}") %>%
      modify_fmt_fun(list(add_stat_1 ~ partial(style_number, digits = 2))) %>%
      modify_header(list(stat_0 ~ "**Mean**",
                         add_stat_1 ~ "**SD**"))
  )












CClim3 = CClim2 %>% select(c(Depth,Temperature,Salinity,DIC,TA,Chlorophyll,Nitrate,PAR))
head(CClim3)



TableAllnew = c("{min}-{max}","{mean} ({sd})") %>%
  lapply(
    function(.x) {
      tbl_summary(
        data = CClim2,
        include = c(Depth,Temperature,Salinity,DIC,TA,Chlorophyll,Nitrate,PAR),
        statistic = everything() ~ .x,
        missing = "no"
      ) %>%
        modify_header(all_stat_cols() ~ "edit here")  #spend too much time trying to change these labels to "range" and "mean (sd)"
    }
  ) %>%
  tbl_merge() %>%
  modify_spanning_header(everything() ~ NA) %>%
  modify_footnote(everything() ~ NA) %>%
  modify_header(label="**Parameter**") 
TableAllnew
TableAllnew %>% as_gt() %>%
  # gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeAll_range_meansd.docx')
# gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeAll_range_meansd.png')

# #this works but it looks messy in one column all together
# TableAll2= CClim3 %>% 
#   tbl_summary(
#     statistic = all_continuous() ~ "{min}-{max}, {mean}({sd})",
#     missing = "no") %>%
#   modify_header(label="**Parameter**") 
# TableAll2

# tbl_summary(CClim3)


## stats by rows
CCtab.all <-
A=  CClim2 %>% 
  tbl_summary(
    include = c(Depth,Temperature,Salinity,DIC,TA,Chlorophyll,Nitrate,PAR),
    type = where(is.numeric) ~ "continuous2",
    # by=Region,
    statistic = all_continuous() ~ c("{min}-{max}","{mean} ({sd})"),
    missing="no"
  ) %>% 
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_header(label="**Parameter**") %>% 
  modify_spanning_header(everything() ~ NA) %>%
  modify_footnote(everything() ~ NA) %>%
  bold_labels() %>% 
  italicize_levels()

A

# CCtab.all
CCtab.all %>% as_gt() %>%
  gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeAll_rows.png')





##_______Summarize all data together (no zone or region separation)
Table.ALL = CClim2 %>% 
tbl_summary(
            # statistic = everything() ~ paste0("{", .x, "}"),
            statistic = all_continuous() ~ "{min}, {max}",
            missing = "no") %>%
  modify_header(label="**Parameter**") 
Table.ALL


# # gt::gtsave(Table.ALL,filename='/Users/heidi.k.hirsh/Desktop/SummarizeAll.png')
# Table.ALL %>% as_gt() %>% gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeAll.png')

            # #previous version with min, max on separate rows 
            # CClim2 %>% tbl_summary(
            #     type = where(is.numeric) ~ "continuous2",
            #     statistic = all_continuous() ~ "{min}, {max}",
            #     # statistic = all_continuous() ~ c("{min}", "{max}"),
            #     missing="no"
            #   ) %>% 
            #   modify_header(all_stat_cols() ~ "**{level}**") %>% 
            #   modify_header(label="**Parameter**") %>% 
            #   modify_footnote(everything() ~ NA) 


##_______Summarize all with breakdown by region
CCtab.sr <-
  CClim2 %>% 
  tbl_summary(
    # include = c(PAR_MODIS_MON,pointDepth,vol_km3,Salinity_Bottle,Temperature_C,Chla,NO3,DIC_umol_kg,TA_umol_kg),
    type = where(is.numeric) ~ "continuous2",
    by=Region,
    # statistic = all_continuous() ~ "{min}-{max}"
    statistic = all_continuous() ~ c("{min}", "{max}"),
    missing="no"
  ) %>% 
  # add_n() %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_header(label="**Parameter**") %>% 
  modify_spanning_header(everything() ~ NA) %>%
  modify_footnote(everything() ~ NA) %>% 
  bold_labels() %>% 
  italicize_levels()
CCtab.sr
# CCtab.sr %>% as_gt() %>%
#   gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeByRegion2.png')

# #further modify so zones are abbreviated
# unique(CCc$Zone) #"Inshore"     "Mid channel" "Offshore"    "Oceanic"   
# unique(CCc$Sub_region) # "BB" "UK" "MK" "LK"
# 
# 
# CClim3 = 
#   CCc %>% 
#   select('Zone','Sub_region','pointDepth','Temperature_C','Salinity_Bottle','DIC_umol_kg','TA_umol_kg','Chla','NO3','PAR_MODIS_MON') %>% #remove year and volume
#   dplyr::mutate(Sub_region=factor(Sub_region,labels=c("Biscayne Bay","Upper Keys","Middle Keys","Lower Keys"))) %>% 
#   # dplyr::mutate(Zone=factor(Zone,labels=c("Inshore","Mid-channel","Offshore","Oceanic"))) %>% 
#   dplyr::mutate(Zone=factor(Zone,labels=c("In","Mid","Off","Oce"))) %>% 
#   dplyr::rename(Region=Sub_region,PAR=PAR_MODIS_MON, TA=TA_umol_kg, DIC=DIC_umol_kg,Chlorophyll=Chla, 
#                 Nitrate=NO3,Temperature=Temperature_C,Salinity=Salinity_Bottle,Depth=pointDepth)
# 
# unique(CClim3$Zone) #In Mid Off Oce
# unique(CClim3$Region)

##_______Summarize all with breakdown by region and by zone
head(CCc)

CCtab.sz <-
  # CClim3 %>% 
  CCc %>%
  # select('Zone','Sub_region','Year','PAR_MODIS_MON','pointDepth','vol_km3','Salinity_Bottle','Temperature_C','Chla','NO3','DIC_umol_kg','TA_umol_kg') %>%
  select('Zone','Sub_region','pointDepth','Temperature_C','Salinity_Bottle','DIC_umol_kg','TA_umol_kg','Chla','NO3','PAR_MODIS_MON') %>% #remove year and volume
  dplyr::mutate(Sub_region=factor(Sub_region,labels=c("Biscayne Bay","Upper Keys","Middle Keys","Lower Keys"))) %>%
  # dplyr::mutate(Zone=factor(Zone,labels=c("In","Mid","Off","Ocean"))) %>%
  dplyr::rename(
    PAR=PAR_MODIS_MON, TA=TA_umol_kg, DIC=DIC_umol_kg,Chlorophyll=Chla, Nitrate=NO3,Temperature=Temperature_C,Salinity=Salinity_Bottle,Depth=pointDepth
  ) %>%
  tbl_strata(
    strata=Sub_region,
    # strata=Region,
    .tbl_fun=~.x %>% 
      tbl_summary(
        by=Zone,
        type = where(is.numeric) ~ "continuous2",
        statistic = all_continuous() ~ c("{min}", "{max}"),
        missing="no"
      ) 
    %>% 
      # add_n() %>%
      modify_header(all_stat_cols() ~ "**{level}**") %>% 
      modify_header(label="**Parameter**") %>% 
      bold_labels() %>% 
      italicize_levels()
  )
CCtab.sz

# tab_style(
#   style = cell_borders(
#     sides = c("left", "right"),
#     weight = px(2)),
#   locations = cells_body(
#     columns = c(year, bdy_style, msrp)
#   )
# )

# CCtab.sz=as_gt(CCtab.sz)
# class(CCtab.sz)
# 
# test=tab_style(CCtab.sz, 
#           style = cell_borders(sides = c("left", "right"),weight = px(2)),
#           locations = cells_body(columns = c())
#           )

# show_header_names(CCtab)
CCtab.sz %>% as_gt() %>%
# CCtab.sz %>%  
  gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeByRegionZone.png',vwidth = 1500, vheight = 1000)
