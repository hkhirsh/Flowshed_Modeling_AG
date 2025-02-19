#run final models for manuscript and plot relative model skill.
#this is where the actual modeling happens
# 12/19 edit for clarity/streamline 
# 1/2/2025 further streamline and document in Whimsical

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c("ggmagnify")
# packageload <- c('tidyverse','patchwork','MuMIn','PNWColors','ghibli','ggtext','ggfx','ggmagnify')

lapply(packageload, library, character.only = TRUE)

# # ----Check which packages we actually use----
# # Find which packages do used functions belong to ----
# used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/Flowshed_Modeling/Model_Pipeline_2024/p3_14days_CCmodel_OSM2024.R", alphabetic = FALSE) |> print()
# # Find which loaded packages are not used ----
# used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
# unused.packages <- packageload[!(packageload %in% used.packages)] |> print()


## Necessary Function(s)
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

se = function(x,na.rm=T){return(sd(x,na.rm=na.rm)/sqrt(length(x)))}

## Load data
CCorig=read.csv("/Users/heidi.k.hirsh/Desktop/Hirsh_FLKmodel_InputFiles/CC_complete_cases_26april2024.csv") #This is written in 'p3_14days_CCmodel_OSM2024.R
table(CCorig$ndays) #all have 984 samples (originally 1376)
table(CCorig$Sub_region)
table(CCorig$Zone)/14
# 
# CCc_sub = subset(CCc, Zone %in% c("Inshore","Mid channel") & ndays==5)
# CCc_reef = subset(CCorig, Zone %in% c("Inshore","Mid channel"))
# CCc_deep = subset(CCorig, Zone %in% c("Offshore","Oceanic"))
CCc_notOcean = subset(CCorig, Zone %in% c("Inshore","Mid channel","Offshore"))

#uncomment to rerun model.
CCc=NULL
CCc=CCc_notOcean

table(CCc_notOcean$Zone)/14
dim(CCc_notOcean) #12054   122
dim(subset(CCc_notOcean, ndays==1)) #861 122
unique(CCc_notOcean$simu) #only backward

#define daily (DV), monthly (MV) and hourly (HV) time and volume interactions with the benthos
#capture "modeled metabolic contribution"
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

# RYi = 1;RTi = 1;OCi = 1;RNi = 1

for (RYi in 1:length(RYs)) {
  # define appropriate southern "oce" (oceanic) reference column name
  OCchosen.txt = OCs[RYi] #use index for response variable to indicate whether to use DIC or TA
  CCc$OC = CCc[, OCchosen.txt]

  # define appropriate northern reference column name
  RNchosen.txt = RNs[RYi] #use index for response variable to indicate whether to use DIC or TA
  CCc$RN = CCc[, RNchosen.txt]

  # for (ZNi in 1:length(ZNs)) {
  # for (SRi in 1:length(SRs)) {
  for (RTi in 1:length(RTs)) {
    RYchosen.txt = RYs[RYi]
    CCc$RY = CCc[, RYchosen.txt]

    CCrt = CCc[which(CCc$ndays == RTi),]
    # CCz = CCrt[which(CCrt$Zone == ZNs[ZNi]),]
    # CCsr = CCz[which(CCz$Sub_region == SRs[SRi]),]
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
      # zone = ZNs[ZNi],
      # region = SRs[SRi],
      ndays = RTi
    )

    ModCol = rbind(ModCol, ModAdd)
    # write.csv(ModCol,"/Users/heidi.k.hirsh/Desktop/ModCol_Oct3.csv")

    #model residuals
    ResidAdd=CCm
    ResidAdd$resids = resid(m0.7)
    ResidAdd$Response = RYchosen.txt
    Resids = rbind(Resids, ResidAdd)

    print(paste0(RYi, "_", RTi, " of 2_14"))
  }

}

#confirmed it runs (2_14 of 2_14) ON DEC 19, 2024
dim(ModCol)
# dim(ModCol) # 224  13
# head(ModCol)

ModCol=read.csv('/Users/heidi.k.hirsh/Desktop/Hirsh_FLKmodel_InputFiles/ModCol_TrainTest_notOceanic_v2_1.csv') #this is the 2024-10-04 14/44/08 file

myColors=pnw_palette(name="Bay",n=14,type="continuous")

model_shapes      <-c(16,4,21,3,22,24,25,23)
model_SHAPE_scale  <- scale_shape_manual(name = "Model Parameters Used", values = model_shapes,
                                         breaks = c('simple','weightedEndmember','cyclicalTime','environmental',
                                                    'modMetabolism','year','SRnoYear','subregion'),
                                         labels = c('0. Simple Model','1. Weighted Endmember','2. Cyclical Time','3. Environmental Parameters',
                                                    '4. Modeled Metabolism','5. Year without Subregion','6. Subregion without Year','7. Year and Subregion'))


#facet labels:
response.labs = c("DIC", "TA")
names(response.labs)= c("DIC_umol_kg","TA_umol_kg")
ModCol$Response2= factor(ModCol$Response, labels = c("bold(DIC)~(mu*mol~kg^-1)", "bold(TA)~(mu*mol~kg^-1)"))

#magnify most complex models to show relative comparison
# from.dic <- c(xmin = -8, xmax = 4, ymin = .614, ymax = .6225)
# Fdic.xmin = -45 #for 4/5/6/7 REEF
Fdic.xmax = 1.5 #for 5/6 noOCE
Fdic.xmin = -7.5 #for 5/6 noOCE
# Fdic.xmax = 2
# Fdic.xmax = 4 #for 4/5/6/7 REEF
# Fdic.xmin = -20

# Fdic.ymin = .622   #for 5 days (FINAL)
# Fdic.ymax = .6265    #for 5 days (FINAL)
# Fdic.ymin = .616 #for 6 days
# Fdic.ymax = .622 #for 6 days
# Fdic.ymin = .608   #for 5/6/7 days
# Fdic.ymax = .63    #for 5/6/7 days

# Fdic.ymin = .608   #for 4/5/6/7 days
# Fdic.ymax = .63    #for 4/5/6/7 days
#traintest 4/5/6/7
# Fdic.ymin = .55  #for 4/5/6/7 days
# Fdic.ymax = .608   #for 4/5/6/7 days
Fdic.ymin = .584  #for 4/5/6/7 days noOCE
Fdic.ymax = .609   #for 4/5/6/7 days noOCE
# Fdic.ymin = .59  #for 4/5/6/7 REEF
# Fdic.ymax = .63   #for 4/5/6/7 REEF

# from.ta <- c(xmin = -15, xmax = 4, ymin = .5, ymax = .6)
# Fta.xmin = -70 #for 4/5/6/7 REEF
# Fta.xmax = 5 #for 4/5/6/7 REEF
Fta.xmin = -15
Fta.xmax = 2
# Fta.xmin = -15
# Fta.xmax = 4

# Fta.ymin = .534 #for 6 days (FINAL)
# Fta.ymax = .545 #for 6 days (FINAL)

# Fta.ymin = .53 #for 5/6/7 days
# Fta.ymax = .546 #for 5/6/7 days
# Fta.ymin = .52 #for 4/5/6/7 days
# Fta.ymax = .546 #for 4/5/6/7 days
#traintest 4/5/6/7
# Fta.ymin = .45 #for 4/5/6/7 days
# Fta.ymax = .52 #for 4/5/6/7 days
Fta.ymin = .48 #for 4/5/6/7 days noOCE
Fta.ymax = .52 #for 4/5/6/7 days noOCE
# Fta.ymin = .455 #for 4/5/6/7 REEF
# Fta.ymax = .54 #for 4/5/6/7 REEF

# Names xmin, xmax, ymin, ymax are optional:
# to.dic <- c(-650, -350,.47, .63)
Tdic.xmin = -500
Tdic.xmax = -250
# Tdic.xmin = -650
# Tdic.xmax = -250
Tdic.ymin = .46
Tdic.ymax = .62

# to.ta <- c(-600, -300,.47, .63)
Tta.xmin = -500
Tta.xmax = -250
# Tta.xmin = -650
# Tta.xmax = -350
Tta.ymin = .46
Tta.ymax = .62

# #Add from column and to column to df then match by response
# ModCol$From=NULL
ModCol$From.xmin=NULL
ModCol$From.xmax=NULL
ModCol$From.ymin=NULL
ModCol$From.ymax=NULL
# ModCol$To=NULL
ModCol$To.xmin=NULL
ModCol$To.xmax=NULL
ModCol$To.ymin=NULL
ModCol$To.ymax=NULL

for (i in 1:dim(ModCol)[1]) {
  if (ModCol$Response[i] == "TA_umol_kg") {
    ModCol$From.xmin[i]=Fta.xmin
    ModCol$From.xmax[i]=Fta.xmax
    ModCol$From.ymin[i]=Fta.ymin
    ModCol$From.ymax[i]=Fta.ymax
    ModCol$To.xmin[i]=Tta.xmin
    ModCol$To.xmax[i]=Tta.xmax
    ModCol$To.ymin[i]=Tta.ymin
    ModCol$To.ymax[i]=Tta.ymax
  } else {
    ModCol$From.xmin[i]=Fdic.xmin
    ModCol$From.xmax[i]=Fdic.xmax
    ModCol$From.ymin[i]=Fdic.ymin
    ModCol$From.ymax[i]=Fdic.ymax
    ModCol$To.xmin[i]=Tdic.xmin
    ModCol$To.xmax[i]=Tdic.xmax
    ModCol$To.ymin[i]=Tdic.ymin
    ModCol$To.ymax[i]=Tdic.ymax
  }
}


From_list <- vector(mode = "list", length = length(ModCol))
To_list <- vector(mode = "list", length = length(ModCol))
for (i in 1:dim(ModCol)[1]) {
  From_list[[i]] =  c(ModCol$From.xmin[i],ModCol$From.xmax[i],ModCol$From.ymin[i],ModCol$From.ymax[i])
  To_list[[i]] =  c(ModCol$To.xmin[i],ModCol$To.xmax[i],ModCol$To.ymin[i],ModCol$To.ymax[i])
}


#Manuscript Figure 6 - compare model skill and complexity

##***Need to limit DIC to ndays=5 and limit TA to ndays=6
#new plotting df: 
# bestDIC=subset(ModCol,ndays==5)
# bestTA= subset(ModCol,ndays==6)
bestMods= subset(ModCol, ndays==5 & Response=='DIC_umol_kg' | ndays==6 & Response=='TA_umol_kg')
# dim(bestMods)

#double check relative comparison (again)

#make sure the correct inset limits are set!

skill=ggplot(ModCol)+
  # geom_point(data=bestMods,aes(x=dAIC,y=Adj.Rsq,shape=ModName),size=9,stroke=1.3)+
  # scale_colour_gradientn(colours = myColors[1:14])+
  
  # geom_point(data=subset(ModCol,ndays==6),aes(x=dAIC,y=Adj.Rsq,shape=ModName),size=9,stroke=1.3)+
  # geom_point(data=subset(ModCol,ndays %in% c(4,5,6,7)),aes(x=dAIC,y=Adj.Rsq,shape=ModName,color=ndays),size=9,stroke=1.3)+
  # scale_colour_gradientn(colours = myColors[4:7])+
  
  # geom_point(data=subset(ModCol,ndays %in% c(4,5,6,7)),aes(x=dAIC,y=Adj.Rsq,shape=ModName,color=as.factor(ndays)),size=9,stroke=1.3)+
  # ylab("Adjusted R-squared")+
  
  # geom_point(data=subset(ModCol,ndays %in% c(4,5,6,7)),aes(x=dAIC,y=Adj.Rsq.TrainTest,shape=ModName,color=as.factor(ndays)),size=9,stroke=1.3)+
  # geom_errorbar(data=subset(ModCol,ndays %in% c(4,5,6,7)),aes(x=dAIC,y=Adj.Rsq.TrainTest,ymin=Adj.Rsq.TrainTest-seTT, ymax=Adj.Rsq.TrainTest+seTT,color=as.factor(ndays)),width = 1)+

  #just show 5 and 6 days
  geom_point(data=subset(ModCol,ndays==5),aes(x=dAIC,y=Adj.Rsq.TrainTest,shape=ModName),size=9,stroke=1.3)+
  geom_errorbar(data=subset(ModCol,ndays==5),aes(x=dAIC,y=Adj.Rsq.TrainTest,ymin=Adj.Rsq.TrainTest-seTT,ymax=Adj.Rsq.TrainTest+seTT),
                color=myColors[5],width = 1,size=.6)+
  # geom_errorbar(data=subset(ModCol,ndays==5),aes(x=dAIC,y=Adj.Rsq.TrainTest,ymin=Adj.Rsq.TrainTest-seTT,ymax=Adj.Rsq.TrainTest+seTT),color='dodgerblue',width = 1)+
  
  
  # geom_errorbar()
  ylab("Train-Test Adjusted R-squared")+
  # ylim(min(ModCol$Adj.Rsq.TrainTest),.65)+
  # ndays_COLOR_scale+
  facet_wrap(~Response2, labeller=label_parsed)+
  model_SHAPE_scale+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  # ylab("Adjusted R-squared")+
  xlab(expression(paste(Delta, "AIC within model cohort")))+
  scale_x_continuous(minor_breaks=seq(-800,100,10))+
  scale_y_continuous(minor_breaks=seq(.1,.63,.01))+
  theme(legend.spacing.y = unit(3, 'mm')) +
  guides(shape = guide_legend(override.aes = list(size = 5),byrow = TRUE))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),strip.text= element_text(size=20),
        legend.title=element_text(size=20),legend.text=element_text(size=19))+
  theme(legend.position = c(0.86, 0.22),
      # legend.background = element_rect(fill = "white", colour = NA),
      legend.background = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA),
      legend.box.background = element_rect(colour = "black"))+
  theme(legend.key.size = unit(2, 'lines'))+
  geom_magnify(aes(from = From_list, to = To_list), colour='grey',
               shadow = TRUE,axes='xy')
skill #FIGURE 6
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/CodeRepo_Dec24/Manuscript_Fig4_TTnotocean_5d_",Sys.time(),".png"),plot=skill,width=20, height=12, dpi = 300)

# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/modelResults/Manuscript_Fig4_TTnotocean_5d_",Sys.time(),".png"),plot=skill,width=20, height=12, dpi = 300)






#_______________Plot simple plot of all model skill
# simple=ggplot(data=ModCol)+
#   # geom_point(data=subset(ModCol,ndays==6),aes(x=dAIC,y=Adj.Rsq.TrainTest,shape=ModName),size=6,stroke=1.3)+
#   geom_point(data=subset(ModCol,ndays %in% c(4,5,6,7)),aes(x=dAIC,y=Adj.Rsq.TrainTest,shape=ModName,color=as.factor(ndays)),size=9,stroke=1.3)+
#   geom_errorbar(data=subset(ModCol,ndays %in% c(4,5,6,7)),aes(x=dAIC,y=Adj.Rsq.TrainTest,ymin=Adj.Rsq.TrainTest-seTT, ymax=Adj.Rsq.TrainTest+seTT,color=as.factor(ndays)),width = 1)+
#   ylab("Train-Test Adjusted R-squared")+
#   # geom_crossbar(data=subset(ModCol,ndays %in% c(4,5,6,7)),aes(x=dAIC,y=Adj.Rsq.TrainTest,ymin=Adj.Rsq.TrainTest-seTT, ymax=Adj.Rsq.TrainTest+seTT,color=a
#   facet_wrap(~Response) +
#   # facet_wrap(~Response, labeller=labeller(Response=response.labs))+
#   # scale_colour_gradientn(colours = myColors)+
#   model_SHAPE_scale+
#   theme_bw()+
#   theme(axis.text=element_text(size=20),axis.title=element_text(size=24),strip.text= element_text(size=20))+
#   # xlab(' ')+ylab(' ')+
#   ylab("Linear Model Skill,\n Adjusted R-squared")+
#   xlab("Model Skill, penalized for model complexity \n (delta AIC within model cohort)") #+
# # theme(legend.position = "none")
# # simple
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/modelResults/simpleskill_TTnotocean_",Sys.time(),".png"),plot=simple,width = 15, height = 8, dpi = 300)



## Manuscript Figure 7 - ndays by r squared
# unique(ModCol$ndays)
ds14=ggplot(ModCol)+
  geom_point(data=subset(ModCol,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,color=ndays,shape=ModName),size=6,stroke=1.3)+
  ylab("Train-Test Adjusted R-squared")+
  # geom_errorbar(data=subset(ModCol,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,ymin=Adj.Rsq.TrainTest-seTT,ymax=Adj.Rsq.TrainTest+seTT,color=ndays),width = 1)+
  
  scale_colour_gradientn(colours = myColors[1:14])+
  
  # geom_point(data=subset(ModCol,ndays<=14),aes(x=ndays,y=Adj.Rsq,color=ndays,shape=ModName),size=6,stroke=1.3)+ #try non train test
  # ylab("Adjusted R-squared")+
  # scale_colour_gradientn(colours = myColors[1:14])+

  # #No TT, highlight nday 4-7
  # geom_point(data=subset(ModCol,ndays<=14),aes(x=ndays,y=Adj.Rsq,shape=ModName),color='gray',size=6,stroke=1.3)+
  # geom_point(data=subset(ModCol,ndays %in% c(4,5,6,7)),aes(x=ndays,y=Adj.Rsq,color=as.factor(ndays),shape=ModName),size=6,stroke=1.3)+
  # ylab("Adjusted R-squared")+

  #Train test data, highlight nday 4-7
  # geom_point(data=subset(ModCol,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,shape=ModName),color='gray',size=6,stroke=1.3)+
  # geom_point(data=subset(ModCol,ndays %in% c(4,5,6,7)),aes(x=ndays,y=Adj.Rsq.TrainTest,color=as.factor(ndays),shape=ModName),size=6,stroke=1.3)+
  # ylab("Train-Test Adjusted R-squared")+
  
  # geom_point(data=subset(ModCol,ndays<=7),aes(x=ndays,y=Adj.Rsq.TrainTest,color=ndays,shape=ModName),size=6,stroke=1.3)+
  # scale_colour_gradientn(colours = myColors[1:7])+
  model_SHAPE_scale+
  facet_wrap(~Response2, labeller=label_parsed)+
  theme(legend.spacing.y = unit(5, 'mm'))+
  guides(shape = guide_legend(override.aes = list(size = 5),byrow = TRUE))+
  guides(color='none')+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),strip.text= element_text(size=22),
        legend.title=element_text(size=18),legend.text=element_text(size=18))+
  theme(panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks=1:1:14)+
  theme(legend.position = 'bottom',legend.title=element_blank())+
  # ylab("Train-Test Adjusted R-squared")+
  xlab("Number of Days")
ds14
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/CodeRepo_Dec24/ndaysComp14L_TTnotOcean_",Sys.time(),".png"),plot=ds14,width =15, height = 8, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/modelResults/ndaysComp14L_TTnotOcean_",Sys.time(),".png"),plot=ds14,width =15, height = 8, dpi = 300)


# 
##Limit models to most complex (to assess differences at high end of model skill):
ModColsub=NULL
ModColsub=dplyr::filter(ModCol, ModName %in%  c("environmental","modMetabolism","year","SRnoYear", "subregion"))
# ModColsub=filter(ModCol, ModName %in%  c("modMetabolism","year","SRnoYear", "subregion"))
unique(ModCol$ModName)
unique(ModColsub$ModName)
# dim(ModColsub)

complex=ggplot()+
  # geom_point(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,shape=ModName),color='gray',size=6,stroke=1.3)+
  # geom_point(data=subset(ModColsub,ndays %in% c(4,5,6,7)),aes(x=ndays,y=Adj.Rsq.TrainTest,color=as.factor(ndays),shape=ModName),size=6,stroke=1.3)+
  
  # geom_point(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,color=ndays,shape=ModName),size=6,stroke=1.3)+
  # scale_colour_gradientn(colours = myColors[1:14])+
  # geom_errorbar(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,ymin=Adj.Rsq.TrainTest-seTT,ymax=Adj.Rsq.TrainTest+seTT,color=ndays),width = 1)+
  
  geom_point(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,shape=ModName),color='gray',size=6,stroke=1.3)+
  geom_errorbar(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,ymin=Adj.Rsq.TrainTest-seTT,ymax=Adj.Rsq.TrainTest+seTT,color=ModName),width = 1)+

  model_SHAPE_scale+
  # facet_wrap(~Response2, labeller=label_parsed,scales="free")+
  facet_grid(Response~ModName,scales="free")+
  # facet_wrap(~Response2, labeller=label_parsed)+
  # facet_wrap(~Response,scales="free")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=20),strip.text= element_text(size=20))+
  scale_x_continuous(breaks=1:1:14)+
  ylab("Train-Test Adjusted R-squared")+
  xlab("Number of Days")+
  theme(legend.position = "none")
  
complex
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/CodeRepo_Dec24/ndaysComp14_complexOnly_notfree+envir_",Sys.time(),".png"),plot=complex,width =15, height = 8, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/modelResults/ndaysComp14_complexOnly_notfree+envir_",Sys.time(),".png"),plot=complex,width =15, height = 8, dpi = 300)


complex2=ggplot()+
  # geom_point(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,shape=ModName),color='gray',size=6,stroke=1.3)+
  # geom_point(data=subset(ModColsub,ndays %in% c(4,5,6,7)),aes(x=ndays,y=Adj.Rsq.TrainTest,color=as.factor(ndays),shape=ModName),size=6,stroke=1.3)+
  
  geom_point(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,color=ndays,shape=ModName),size=6,stroke=1.3)+
  scale_colour_gradientn(colours = myColors[1:14])+
  # geom_errorbar(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,ymin=Adj.Rsq.TrainTest-seTT,ymax=Adj.Rsq.TrainTest+seTT,color=ndays),width = 1)+
  
  # geom_point(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,shape=ModName),color='gray',size=6,stroke=1.3)+
  # geom_errorbar(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,ymin=Adj.Rsq.TrainTest-seTT,ymax=Adj.Rsq.TrainTest+seTT,color=ModName),width = 1)+
  
  model_SHAPE_scale+
  facet_wrap(~Response2, labeller=label_parsed,scales="free")+
  
  # facet_grid(Response~ModName,,scales="free")+
  # facet_wrap(~Response2, labeller=label_parsed)+
  # facet_wrap(~Response,scales="free")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=20),strip.text= element_text(size=20))+
  scale_x_continuous(breaks=1:1:14)+
  ylab("Train-Test Adjusted R-squared")+
  xlab("Number of Days")+
  theme(legend.position = "none")

complex2
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/CodeRepo_Dec24/ndaysComp14_complex2Only_notfree+envir_",Sys.time(),".png"),plot=complex2,width =15, height = 8, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/modelResults/ndaysComp14_complexOnly_notfree+envir_",Sys.time(),".png"),plot=complex,width =15, height = 8, dpi = 300)







##_______________Plot Residuals_______________
  
Resids$Sub_region = factor(Resids$Sub_region, levels = c("BB","UK","MK","LK"))
Resids$Zone = factor(Resids$Zone, levels = c("Inshore","Mid channel","Offshore","Oceanic"))

##_____Plot residuals of the best model (color by nday; xaxis = north fraction)
for (RYi in 1:length(RYs)) {
  for (RTi in 1:length(RTs)) {
  plotMe=Resids %>% filter(Response==RYs[RYi]) %>% filter(ndays==RTs[RTi]) 
  res=
    ggplot(plotMe)+
    geom_point(aes(x=north_fraction,y=resids,color=Zone))+
    geom_hline(yintercept=0)+
    # scale_color_manual(values= wes_palette("GrandBudapest2", n = 7))+
    # scale_colour_gradientn(colours = myColors)+
    facet_wrap(~Sub_region,nrow=4)+
    # facet_grid(Zone~Sub_region)+
    ggtitle(paste("Response=",RYs[RYi]," ndays=",RTs[RTi]))+
    theme_bw()
  res
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/modelResults/Residuals_",RYs[RYi],"_ndays=",RTs[RTi],"_facetSR_",Sys.time(),".png"),plot=res,width = 10, height = 7, dpi = 300)
  }
}

# pnw=pnw_palette(name="Bay",n=7,type="continuous")
# pnw=pnw_palette(name="Bay",n=14,type="continuous")
# pnw=pnw_palette(name="Anemone",n=7,type="continuous")

##_____Color by north fraction (xaxis = ndays)
for (RYi in 1:length(RYs)) {
  plotMe=Resids %>% filter(Response==RYs[RYi])
  dim(plotMe)/7
  res=
    ggplot(plotMe)+
    geom_point(aes(x=ndays,y=resids,color=north_fraction))+
    geom_hline(yintercept=0)+
    # scale_colour_gradientn(colours = myColors)+
    scale_color_gradientn(colours = pnw) +
    facet_grid(Zone~Sub_region)+
    ggtitle(paste("Response=",RYs[RYi]))+
    theme_bw()
  res
  # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/modelResults/Residuals_",RYs[RYi],"_fractCol_facetZNSR_",Sys.time(),".png"),plot=res,width = 10, height = 7, dpi = 300)
}

##_____Compare ndays (assess residuals of the best model)
for (RYi in 1:length(RYs)) {
    plotMe=Resids %>% filter(Response==RYs[RYi])
    dim(plotMe)/7
    res=
      ggplot(plotMe)+
      geom_point(aes(x=north_fraction,y=resids,color=ndays))+
      geom_hline(yintercept=0)+
      scale_colour_gradientn(colours = myColors)+
      facet_grid(Zone~Sub_region)+
      ggtitle(paste("Response=",RYs[RYi]))+
      theme_bw()
    res
    # ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/modelResults/Residuals_",RYs[RYi],"_ALLndays_facetZNSR_",Sys.time(),".png"),plot=res,width = 10, height = 7, dpi = 300)
  }




#### complex comparison for poster: 
complex3=ggplot()+
  # geom_point(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,shape=ModName),color='gray',size=6,stroke=1.3)+
  # geom_point(data=subset(ModColsub,ndays %in% c(4,5,6,7)),aes(x=ndays,y=Adj.Rsq.TrainTest,color=as.factor(ndays),shape=ModName),size=6,stroke=1.3)+
  
  geom_point(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,color=ndays,shape=ModName),size=6,stroke=1.3)+
  # scale_colour_gradientn(colours = myColors[1:14])+
  scale_color_gradientn(colours=myColors[1:14], breaks=c(1,7,14),labels=c('1 day','7 days','14 days'))+
  # geom_errorbar(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,ymin=Adj.Rsq.TrainTest-seTT,ymax=Adj.Rsq.TrainTest+seTT,color=ndays),width = 1)+
  
  # geom_point(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,shape=ModName),color='gray',size=6,stroke=1.3)+
  # geom_errorbar(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,ymin=Adj.Rsq.TrainTest-seTT,ymax=Adj.Rsq.TrainTest+seTT,color=ModName),width = 1)+
  
  model_SHAPE_scale+
  facet_wrap(~Response2, labeller=label_parsed,scales="free")+
  
  # facet_grid(Response~ModName,,scales="free")+
  # facet_wrap(~Response2, labeller=label_parsed)+
  # facet_wrap(~Response,scales="free")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=20),strip.text= element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=1:1:14)+
  # ylab("Train-Test Adjusted R-squared")+
  # xlab("Number of Days")+
  ylab("")+
  xlab("")+
  guides(shape = "none") +
  # theme(legend.position = c(0.85, 0.15),legend.title = element_blank(),legend.direction='horizontal',
  #       legend.text = element_text(size = 12),  # Increase legend text size
  #       legend.key.size = unit(1.5, "lines") )#, legend.direction = "horizontal")

  # theme(legend.position = "bottom",legend.title = element_blank())#, legend.direction = "horizontal")
theme(legend.position = "none")

complex3
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/complex4.png"),plot=complex3,width =13, height = 8, dpi = 300)
