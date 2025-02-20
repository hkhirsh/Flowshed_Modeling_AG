#summarize model input parameters (this is the testing/exploration file - messier)
#see "SummarizeParams_cln" for more final tables.

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c("ggpubr","gtsummary","dplyr",'viridis')
# packageload <- c("gtsummary")
lapply(packageload, library, character.only = TRUE)

# ----Check which packages we actually use----
# Find which packages do used functions belong to ----
used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/Flowshed_Modeling_AG/SummarizeParams_cln.R", alphabetic = FALSE) |> print()

# Find which loaded packages are not used ----
used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
unused.packages <- packageload[!(packageload %in% used.packages)] |> print()


CCc=read.csv("/Users/heidi.k.hirsh/Desktop/Hirsh_FLKmodel_InputFiles/CC_complete_cases_26april2024.csv")

#Subset to parameters I was to summarize:
CClim=select(CCc, c('Zone','Sub_region','Year','PAR_MODIS_MON','pointDepth','vol_km3','Salinity_Bottle','Temperature_C','Chla','NO3','DIC_umol_kg','TA_umol_kg'))
# summary(CClim)
# tbl_summary(CClim)

# tbl_summary(CClim)
theme_gtsummary_language("en", big.mark = "") #remove the comma in the numbers


#clean up the input data to summarize
CClim2 = 
  CCc %>% 
  select('Zone','Sub_region','pointDepth','Temperature_C','Salinity_Bottle','DIC_umol_kg','TA_umol_kg','Chla','NO3','PAR_MODIS_MON') %>% #remove year and volume
  dplyr::mutate(Sub_region=factor(Sub_region,labels=c("Biscayne","Upper Keys","Middle Keys","Lower Keys"))) %>% 
  dplyr::mutate(Zone=factor(Zone,labels=c("Inshore","Mid-channel","Offshore","Oceanic"))) %>% 
  dplyr::rename(Region=Sub_region,PAR=PAR_MODIS_MON, TA=TA_umol_kg, DIC=DIC_umol_kg,Chlorophyll=Chla, 
                Nitrate=NO3,Temperature=Temperature_C,Salinity=Salinity_Bottle,Depth=pointDepth)
head(CClim2)
dim(CClim2)

##don't include oceanic
CClim3 = subset(CClim2, Zone != "Oceanic")
table(CClim3$Zone)
dim(CClim3)

table(CClim2$Region,CClim2$Zone)
table(CClim3$Region,CClim3$Zone)
##_______Summarize all data together (no zone or region %; separate columns for stats)
#report range and mean (sd) 

TableAllcols = c("{min}-{max}","{mean} ({sd})") %>%
  lapply(
    function(.x) {
      tbl_summary(
        # data = CClim2,#includes oceanic
        data = CClim3,
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
TableAllcols

library(webshot2)

TableAllcols %>% as_gt() %>%
# gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeAll_range_meansd.docx')
gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeAll_range_meansd_noOce.docx')

##_______Summarize all data together (no zone or region %; rows for stats)


#separate rows
CClim3 |> select(Depth,Temperature,Salinity,DIC,TA,Chlorophyll,Nitrate,PAR) |> 
  tbl_summary( 
    type = all_continuous() ~ "continuous2", 
    statistic = all_continuous() ~ c("{mean}({sd})", "{min}-{max}"), 
  ) |> 
  add_stat_label(label = ~ c("Mean (sd)", "Range"))%>%
  bold_labels() %>% 
  italicize_levels()


## stats by rows (not saving correctly)
# CCtab.all <-
CClim3 %>% 
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

# CCtab.all
# CCtab.all %>% as_gt() %>%
#   gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeAll_rows.png')




#Summarize zone and region breakdown
Table.ALL = CClim2 %>% 
  tbl_summary(
    include = c(Region,Zone),
    missing = "no") %>%
  modify_footnote(everything() ~ NA) %>%
  modify_header(label="**Model Input**") %>% 
  bold_labels() %>% 
  italicize_levels()
Table.ALL
# Table.ALL %>% as_gt() %>%
#   gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeZR.png')









# ## _______Summarize all data together (no zone or region separation)
# CClim2 %>%
# tbl_summary(
#             # statistic = everything() ~ paste0("{", .x, "}"),
#             statistic = all_continuous() ~ "{min}, {max}",
#             missing = "no") %>%
#   modify_header(label="**Parameter**")


# ##_______Summarize all with breakdown by region
# CCtab.sr <-
#   CClim2 %>% 
#   tbl_summary(
#     type = where(is.numeric) ~ "continuous2",
#     by=Region,
#     statistic = all_continuous() ~ c("{min}", "{max}"),
#     missing="no"
#   ) %>% 
#   modify_header(all_stat_cols() ~ "**{level}**") %>%
#   modify_header(label="**Parameter**") %>% 
#   modify_spanning_header(everything() ~ NA) %>%
#   modify_footnote(everything() ~ NA) %>% 
#   bold_labels() %>% 
#   italicize_levels()
# CCtab.sr
# # CCtab.sr %>% as_gt() %>%
# #   gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeByRegion2.png')


# ##_______Summarize all with breakdown by region and by zone
# CCtab.sz <-
#   CCc %>%
#   select('Zone','Sub_region','pointDepth','Temperature_C','Salinity_Bottle','DIC_umol_kg','TA_umol_kg','Chla','NO3','PAR_MODIS_MON') %>% #remove year and volume
#   dplyr::mutate(Sub_region=factor(Sub_region,labels=c("Biscayne Bay","Upper Keys","Middle Keys","Lower Keys"))) %>%
#   dplyr::rename(
#     PAR=PAR_MODIS_MON, TA=TA_umol_kg, DIC=DIC_umol_kg,Chlorophyll=Chla, Nitrate=NO3,Temperature=Temperature_C,Salinity=Salinity_Bottle,Depth=pointDepth
#   ) %>%
#   tbl_strata(
#     strata=Sub_region,
#     .tbl_fun=~.x %>% 
#       tbl_summary(
#         by=Zone,
#         type = where(is.numeric) ~ "continuous2",
#         statistic = all_continuous() ~ c("{min}", "{max}"),
#         missing="no"
#       ) 
#     %>% 
#       modify_header(all_stat_cols() ~ "**{level}**") %>% 
#       modify_header(label="**Parameter**") %>% 
#       bold_labels() %>% 
#       italicize_levels()
#   )
# CCtab.sz
# # CCtab.sz %>% as_gt() %>%
# #   gt::gtsave(filename='/Users/heidi.k.hirsh/Desktop/SummarizeByRegionZone.png',vwidth = 1500, vheight = 1000)
