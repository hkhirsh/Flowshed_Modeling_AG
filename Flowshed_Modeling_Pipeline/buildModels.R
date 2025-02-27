##This script is where each of the (increasingly complex) models is built. Model output is saved here (variable name = ModCol)

##Heidi K. Hirsh
##Last edit: Feb 19, 2025

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c("ggmagnify","ggpubr","gtsummary","dplyr",'viridis')
# packageload <- c('tidyverse','patchwork','MuMIn','PNWColors','ghibli','ggtext','ggfx','ggmagnify')
lapply(packageload, library, character.only = TRUE)

## Necessary Functions ---------------------------------------------------------

## Train Test
traintest=function(mod,train_p=.8,rep=F,nruns=100,metric="adj.r.squared"){
  if(mod$call[[2]]=="RY ~ 1"){
    rsqtt=summary(mod)[metric]
  }else{
    modData=mod$model
    dataN=nrow(modData)
    trainN=floor(dataN*train_p)
    rsqtt=rep(NA,nruns)
    for(i in 1:nruns){
      trainset=sample(x = 1:dataN,size = trainN,replace = rep)
      testset=setdiff(1:dataN,trainset)
      trainData=modData[trainset,]
      testData=modData[testset,]
      ttmod_call=mod$call
      ttmod_call[[3]]=trainData
      ttmod=eval(ttmod_call)
      sttmod=summary(ttmod)
      ptt=predict(ttmod,newdata=testData)
      opmod=lm(testData$RY~ptt)
      sopmod=summary(opmod)
      rsqtt[i]=unlist(sopmod[metric])
    }
  }
  return(rsqtt)
}

## Standard Error
se = function(x,na.rm=T){return(sd(x,na.rm=na.rm)/sqrt(length(x)))}

## Load dataframe with complete input parameters for each sample: 
CCorig=read.csv('Flowshed_Modeling_InputData/CC_complete_cases_26april2024.csv')

## Do not use Oceanic zone data for training the models
CCc_notOcean = subset(CCorig, Zone %in% c("Inshore","Mid channel","Offshore"))
CCc=NULL
CCc=CCc_notOcean

## Define daily (DV), monthly (MV) and hourly (HV) time and volume interactions with the benthos
## These parameters capture "modeled metabolic contribution"
CCc$CosHr=cos(2*pi*CCc$hrod.lst/24)
CCc$SinHr=sin(2*pi*CCc$hrod.lst/24)
CCc$CosMOY=cos(2*pi*CCc$MoY/12)
CCc$SinMOY=sin(2*pi*CCc$MoY/12)
CCc=CCc %>% mutate(
  CALC_DV=CALC_m2*ndays*inverseVol,  #
  CALC_cHV=CALC_m2*CosHr*inverseVol,
  CALC_sHV=CALC_m2*SinHr*inverseVol,
  CALC_cMV=CALC_m2*CosMOY*inverseVol,
  CALC_sMV=CALC_m2*SinMOY*inverseVol,
  ALGi_DV=ALGi_m2*ndays*inverseVol,  #
  ALGi_cHV=ALGi_m2*CosHr*inverseVol,
  ALGi_sHV=ALGi_m2*SinHr*inverseVol,
  ALGi_cMV=ALGi_m2*CosMOY*inverseVol,
  ALGi_sMV=ALGi_m2*SinMOY*inverseVol,
  SGi_DV=SGi_m2*ndays*inverseVol,  #
  SGi_cHV=SGi_m2*CosHr*inverseVol,
  SGi_sHV=SGi_m2*SinHr*inverseVol,
  SGi_cMV=SGi_m2*CosMOY*inverseVol,
  SGi_sMV=SGi_m2*SinMOY*inverseVol
)

Resids=NULL
ResidAdd=NULL
ModAdd = NULL
ModCol = NULL

RYs = c("DIC_umol_kg", "TA_umol_kg") #response variables
RTs = unique(CCc$ndays) #1-14
ZNs = unique(CCc$Zone) #"Inshore" "Mid channel" "Offshore" "Oceanic"
SRs = unique(CCc$Sub_region)  #"BB" "UK" "MK" "LK"

#also need to loop through OCE refs (TA or DIC) and fraction refs (endmember chemistry)
OCs = c("DICoce_mean", "TAoce_mean")
RNs = c("DICrefN", "TArefN")

for (RYi in 1:length(RYs)) {
  # define appropriate southern "oce" (oceanic) reference column name
  OCchosen.txt = OCs[RYi] #use index for response variable to indicate whether to use DIC or TA
  CCc$OC = CCc[, OCchosen.txt]

  # define appropriate northern reference column name
  RNchosen.txt = RNs[RYi] #use index for response variable to indicate whether to use DIC or TA
  CCc$RN = CCc[, RNchosen.txt]

  for (RTi in 1:length(RTs)) {
    RYchosen.txt = RYs[RYi]
    CCc$RY = CCc[, RYchosen.txt]

    CCrt = CCc[which(CCc$ndays == RTi),]
    CCm = CCrt

    ###_______simple (Benthic indices + Volume + Atlantic (S) endmember)
    m0.0 = lm(
      RY ~
        CALC_m2 +
        ALGi_m2 +
        SGi_m2 +
        inverseVol+
        OC,
      data = CCm
    )
    sm0.0 = summary(m0.0)
    tt0.0=traintest(m0.0) #ADD THIS TO OTHER MODELS
    m0.0.tt = mean(tt0.0)
    se0.0.tt= se(tt0.0)
    sm0.0$adj.r.squared
    AIC(m0.0)

    ###_______add weighted endmember (fractions of N and S endmembers)
    m0.1 = lm(
      RY ~
        CALC_m2 +
        ALGi_m2 +
        SGi_m2 +
        inverseVol +
        (1-FBfraction) * OC + #should confirm that FBfraction is always 0 for BB
        FBfraction * RN,
      data = CCm
    )
    sm0.1 = summary(m0.1)
    # m0.1.tt = mean(traintest(m0.1))
    tt0.1=traintest(m0.1)
    m0.1.tt = mean(tt0.1)
    se0.1.tt= se(tt0.1)
    sm0.1$adj.r.squared
    AIC(m0.1)


    ###_______add cyclical time
    m0.2 = lm(
      RY ~
        CALC_m2 +
        ALGi_m2 +
        SGi_m2 +
        inverseVol +
        (1-FBfraction) * OC +
        FBfraction * RN +
        CosHr +
        SinHr +
        CosMOY +
        SinMOY,
      data = CCm
    )
    sm0.2 = summary(m0.2)
    # m0.2.tt = mean(traintest(m0.2))
    tt0.2=traintest(m0.2)
    m0.2.tt = mean(tt0.2)
    se0.2.tt= se(tt0.2)
    sm0.2$adj.r.squared
    AIC(m0.2)


    ###_______add environmental parameters (PAR, T, S, Chla, NO3)
    m0.3 = lm(
      RY ~
        CALC_m2 +
        ALGi_m2 +
        SGi_m2 +
        inverseVol +
        (1-FBfraction) * OC +
        FBfraction * RN +
        CosHr +
        SinHr +
        CosMOY +
        SinMOY +
        PAR_MODIS_MON +
        Salinity_Bottle +
        Temperature_C +
        Chla +
        NO3,
      data = CCm
    )
    sm0.3 = summary(m0.3)
    # m0.3.tt = mean(traintest(m0.3))
    tt0.3=traintest(m0.3)
    m0.3.tt = mean(tt0.3)
    se0.3.tt= se(tt0.3)
    sm0.3$adj.r.squared
    AIC(m0.3)

    ###_______add "modeled metabolic contribution" (benthic index * cyclical time * volume)
    m0.4 = lm(
      RY ~
        # CALC_m2 +
        # ALGi_m2 +
        # SGi_m2 +
        # inverseVol +
        (1-FBfraction) * OC +
        FBfraction * RN +
        # CosHr +
        # SinHr +
        # CosMOY +
        # SinMOY +
        PAR_MODIS_MON +
        Salinity_Bottle +
        Temperature_C +
        Chla +
        NO3+
        CALC_DV +
        CALC_cHV +
        CALC_sHV +
        CALC_cMV +
        CALC_sMV +
        ALGi_DV +
        ALGi_cHV +
        ALGi_sHV +
        ALGi_cMV +
        ALGi_sMV +
        SGi_DV +
        SGi_cHV +
        SGi_sHV +
        SGi_cMV +
        SGi_sMV,
      data = CCm
    )
    sm0.4 = summary(m0.4)
    # m0.4.tt = mean(traintest(m0.4))
    tt0.4=traintest(m0.4)
    m0.4.tt = mean(tt0.4)
    se0.4.tt= se(tt0.4)
    sm0.4$adj.r.squared
    AIC(m0.4)


    ###_______add year (*replace with hurricane index)
    m0.5 = lm(
      RY ~
        # CALC_m2 +
        # ALGi_m2 +
        # SGi_m2 +
        # inverseVol +
        (1-FBfraction) * OC +
        FBfraction * RN +
        # CosHr +
        # SinHr +
        # CosMOY +
        # SinMOY +
        PAR_MODIS_MON +
        Salinity_Bottle +
        Temperature_C +
        Chla +
        NO3+
        CALC_DV +
        CALC_cHV +
        CALC_sHV +
        CALC_cMV +
        CALC_sMV +
        ALGi_DV +
        ALGi_cHV +
        ALGi_sHV +
        ALGi_cMV +
        ALGi_sMV +
        SGi_DV +
        SGi_cHV +
        SGi_sHV +
        SGi_cMV +
        SGi_sMV +
        Year,
      data = CCm
    )
    sm0.5 = summary(m0.5)
    # m0.5.tt = mean(traintest(m0.5))
    tt0.5=traintest(m0.5)
    m0.5.tt = mean(tt0.5)
    se0.5.tt= se(tt0.5)
    sm0.5$adj.r.squared
    AIC(m0.5)

    ###_______add subregion (without year)
    m0.6 = lm(
      RY ~
        # CALC_m2 +
        # ALGi_m2 +
        # SGi_m2 +
        # inverseVol +
        (1-FBfraction) * OC +
        FBfraction * RN +
        # CosHr +
        # SinHr +
        # CosMOY +
        # SinMOY +
        PAR_MODIS_MON +
        Salinity_Bottle +
        Temperature_C +
        Chla +
        NO3+
        CALC_DV +
        CALC_cHV +
        CALC_sHV +
        CALC_cMV +
        CALC_sMV +
        ALGi_DV +
        ALGi_cHV +
        ALGi_sHV +
        ALGi_cMV +
        ALGi_sMV +
        SGi_DV +
        SGi_cHV +
        SGi_sHV +
        SGi_cMV +
        SGi_sMV +
        # Year +
        Sub_region,
      data = CCm
    )
    sm0.6 = summary(m0.6)
    # m0.6.tt = mean(traintest(m0.6))
    tt0.6=traintest(m0.6)
    m0.6.tt = mean(tt0.6)
    se0.6.tt= se(tt0.6)
    sm0.6$adj.r.squared
    AIC(m0.6)

    ###_______add subregion (keep year too)
    m0.7 = lm(
      RY ~
        # CALC_m2 +
        # ALGi_m2 +
        # SGi_m2 +
        # inverseVol +
        (1-FBfraction) * OC +
        FBfraction * RN +
        # CosHr +
        # SinHr +
        # CosMOY +
        # SinMOY +
        PAR_MODIS_MON +
        Salinity_Bottle +
        Temperature_C +
        Chla +
        NO3+
        CALC_DV +
        CALC_cHV +
        CALC_sHV +
        CALC_cMV +
        CALC_sMV +
        ALGi_DV +
        ALGi_cHV +
        ALGi_sHV +
        ALGi_cMV +
        ALGi_sMV +
        SGi_DV +
        SGi_cHV +
        SGi_sHV +
        SGi_cMV +
        SGi_sMV +
        Year +
        Sub_region,
      data = CCm
    )
    sm0.7 = summary(m0.7)
    # m0.7.tt = mean(traintest(m0.7))
    tt0.7=traintest(m0.7)
    m0.7.tt = mean(tt0.7)
    se0.7.tt= se(tt0.7)
    sm0.7$adj.r.squared
    AIC(m0.7)


    ModAdd = data.frame(
      ModName = c(
        'simple',
        'weightedEndmember',
        'cyclicalTime',
        'environmental',
        'modMetabolism',
        'year',
        'SRnoYear',
        'subregion'
      ),
      Adj.Rsq = c(
        sm0.0$adj.r.squared,
        sm0.1$adj.r.squared,
        sm0.2$adj.r.squared,
        sm0.3$adj.r.squared,
        sm0.4$adj.r.squared,
        sm0.5$adj.r.squared,
        sm0.6$adj.r.squared,
        sm0.7$adj.r.squared
      ),
      AIC = AIC(m0.0, m0.1, m0.2, m0.3, m0.4, m0.5, m0.6, m0.7)$AIC,
      dAIC = min(AIC(m0.0, m0.1, m0.2, m0.3, m0.4, m0.5, m0.6, m0.7)$AIC) - AIC(m0.0, m0.1, m0.2, m0.3, m0.4, m0.5, m0.6, m0.7)$AIC,
      BIC = BIC(m0.0, m0.1, m0.2, m0.3, m0.4, m0.5, m0.6, m0.7)$BIC,
      dBIC = min(BIC(m0.0, m0.1, m0.2, m0.3, m0.4, m0.5, m0.6, m0.7)$BIC) - BIC(m0.0, m0.1, m0.2, m0.3, m0.4, m0.5, m0.6, m0.7)$BIC,
      Adj.Rsq.TrainTest = c(m0.0.tt, m0.1.tt, m0.2.tt, m0.3.tt, m0.4.tt, m0.5.tt, m0.6.tt, m0.7.tt), #mean train test R squared
      seTT = c(se0.0.tt, se0.1.tt, se0.2.tt, se0.3.tt, se0.4.tt, se0.5.tt, se0.6.tt, se0.7.tt), #standard error train test R squared
      Response = RYchosen.txt,
      S_ref = OCchosen.txt,
      N_ref = RNchosen.txt,
      N = nrow(CCm),
      ndays = RTi
    )

    ModCol = rbind(ModCol, ModAdd)

    #model residuals
    ResidAdd=CCm
    ResidAdd$resids = resid(m0.7)
    ResidAdd$Response = RYchosen.txt
    Resids = rbind(Resids, ResidAdd)

    print(paste0(RYi, "_", RTi, " of 2_14"))
  }

}

## Save output
# write.csv(ModCol, file="Flowshed_Modeling_IntermediateFiles/ModCol_output.csv",row.names=FALSE)

## Load this output to recreate Figures 6 & 7
## My model output file is "ModCol_TrainTest_notOceanic_v2_1.csv"

## Summarize parameter input in table (This is Table 2 in the manuscript)
theme_gtsummary_language("en", big.mark = "") #remove the comma in the numbers
#Subset to parameters I was to summarize:
CCtab=subset(CCorig, ndays==1)
# table(CCorig$ndays)
# table(CCtab$ndays)

#clean up the input data to summarize
CClim2 = 
  CCtab %>% 
  # CCorig %>% 
  select('Zone','Sub_region','pointDepth','Temperature_C','Salinity_Bottle','DIC_umol_kg','TA_umol_kg','Chla','NO3','PAR_MODIS_MON') %>% #remove year and volume
  dplyr::mutate(Sub_region=factor(Sub_region,labels=c("Biscayne","Upper Keys","Middle Keys","Lower Keys"))) %>% 
  dplyr::mutate(Zone=factor(Zone,labels=c("Inshore","Mid-channel","Offshore","Oceanic"))) %>% 
  dplyr::rename(Region=Sub_region,PAR=PAR_MODIS_MON, TA=TA_umol_kg, DIC=DIC_umol_kg,Chlorophyll=Chla, 
                Nitrate=NO3,Temperature=Temperature_C,Salinity=Salinity_Bottle,Depth=pointDepth)
dim(CClim2) #984
table(CClim2$Zone)

##don't include oceanic
CClim3 = subset(CClim2, Zone != "Oceanic")
table(CClim3$Zone)    
dim(CClim3) #665

# table(CClim2$Region,CClim2$Zone)
# table(CClim3$Region,CClim3$Zone)


## summarize and report range and mean (sd) 

TableAllcols = c("{min}-{max}","{mean} ({sd})") %>%
  lapply(
    function(.x) {
      tbl_summary(
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



#separate rows
CClim3 |> select(Depth,Temperature,Salinity,DIC,TA,Chlorophyll,Nitrate,PAR) |> 
  tbl_summary( 
    type = all_continuous() ~ "continuous2", 
    statistic = all_continuous() ~ c("{mean}({sd})", "{min}-{max}"), 
  ) |> 
  add_stat_label(label = ~ c("Mean (sd)", "Range"))%>%
  bold_labels() %>% 
  italicize_levels()



#Summarize zone and region breakdown
Table.ALL = CClim3 %>% 
  tbl_summary(
    include = c(Region,Zone),
    missing = "no") %>%
  modify_footnote(everything() ~ NA) %>%
  modify_header(label="**Model Input**") %>% 
  bold_labels() %>% 
  italicize_levels()
Table.ALL




