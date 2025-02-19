## Supplemental Plots showing variability in input 
## Heidi K. Hirsh
## Last edit: Feb 19, 2025


## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('tidyverse','patchwork','MuMIn','PNWColors','ghibli','ggtext','ggfx','RColorBrewer')
lapply(packageload, library, character.only = TRUE)

se = function(x,na.rm=T){return(sd(x,na.rm=na.rm)/sqrt(length(x)))}

## Load data
CCorig=read.csv("Flowshed_Modeling_InputData/CC_complete_cases_26april2024.csv") 
CCc_notOcean = subset(CCorig, Zone %in% c("Inshore","Mid channel","Offshore"))

CCc=NULL
CCc=CCc_notOcean


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

#Calculate DIC/TA Admixed
CCc=CCc %>% mutate(DICadmix=(1-FBfraction) * DICoce_mean + FBfraction * DICrefN,
                   TAadmix=(1-FBfraction) * TAoce_mean + FBfraction * TArefN)
CCc$Sub_region = factor(CCc$Sub_region, levels = c("BB","UK","MK","LK"))


DIC.ad.plot=CCc %>% filter(
  Zone=="Inshore",
  ndays=="5") %>% 
  group_by(Sub_region,jday,Year) %>% 
  summarize(
    FBfraction.mn=mean(FBfraction),
    DICad.mn=mean(DICadmix,na.rm=T),
    DICad.se=se(DICadmix,na.rm=T),
    TAad.mn=mean(TAadmix,na.rm=T),
    TAad.se=se(TAadmix,na.rm=T)
  ) %>% 
  ggplot(aes(x=jday,y=DICad.mn,
             ymin=DICad.mn-DICad.se,
             ymax=DICad.mn+DICad.se))+
  geom_point(aes(fill=FBfraction.mn,size=FBfraction.mn),color="black",shape=21)+
  geom_line(aes(group=Year,color=factor(Year)))+
  # scale_color_brewer('Year',palette = "Dark2")+
  geom_errorbar()+
  scale_fill_continuous(type="viridis")+
  facet_grid(Sub_region~.)+theme_bw()+xlab("Julian Day")+ylab("Admixed DIC (umol/kg)")+
  labs(size="Northern Endmember Fraction",fill="Northern Endmember Fraction",color="Year")+
  theme(legend.position = "bottom",
        legend.box = "vertical")#+
DIC.ad.plot



TA.ad.plot=CCc %>% filter(
  Zone=="Inshore",
  ndays=="5") %>% 
  group_by(Sub_region,jday,Year) %>% 
  summarize(
    FBfraction.mn=mean(FBfraction),
    DICad.mn=mean(DICadmix,na.rm=T),
    DICad.se=se(DICadmix,na.rm=T),
    TAad.mn=mean(TAadmix,na.rm=T),
    TAad.se=se(TAadmix,na.rm=T)
  ) %>% 
  ggplot(aes(x=jday,y=TAad.mn,
             ymin=TAad.mn-TAad.se,
             ymax=TAad.mn+TAad.se))+
  geom_point(aes(fill=FBfraction.mn,size=FBfraction.mn),color="black",shape=21)+
  geom_line(aes(group=Year,color=factor(Year)))+
  geom_errorbar()+
  scale_fill_continuous(type="viridis")+
  labs(size="Northern Endmember Fraction",fill="Northern Endmember Fraction",color="Year")+
  facet_grid(Sub_region~.)+theme_bw()+xlab("Julian Day")+ylab("Admixed TA (umol/kg)")+
  theme(legend.position = "bottom",
        legend.box = "vertical")
TA.ad.plot


VOL.plot=CCc %>% filter(
  Zone=="Inshore",
  ndays=="5") %>% 
  group_by(Sub_region,jday,Year) %>% 
  summarize(
    Vol.mn=mean(vol_km3,na.rm=T),
    Vol.se=se(vol_km3,na.rm=T)
  ) %>% 
  ggplot(aes(x=jday,y=Vol.mn,
             ymin=Vol.mn-Vol.se,
             ymax=Vol.mn+Vol.se))+
  geom_point(aes(),color='black',shape=21)+
  geom_line(aes(group=Year,color=factor(Year)))+
  geom_errorbar()+
  labs(color="Year")+
  scale_y_log10()+# scale_fill_continuous(type="viridis")+
  facet_grid(Sub_region~.)+theme_bw()+xlab("Julian Day")+ylab("Flowshed Volume (km3)")+
  theme(legend.position = "bottom",
        legend.box = "vertical")
VOL.plot


#benthic colors
myColors = c('coral1','palegreen3','steelblue1')

# model_shapes      <-c(16,4,21,3,22,24,25,23)
ben_FILL_scale  <- scale_fill_manual(name = "Benthic Habitat Class", values = myColors,
                                         breaks = c('CALC.mn','SGi.mn','ALGi.mn'),
                                         labels = c('Calcifiers','Seagrass','Non-calcifying Algae'))

BENcc = CCc %>% filter(
    Zone=="Inshore",
    ndays=="5",
    Year==2019) %>% 
  group_by(Sub_region,MY,Year) %>% 
  summarize(
    CALC.mn=mean(CALC_m2,na.rm=T),
    SGi.mn=mean(SGi_m2,na.rm=T),
    ALGi.mn=mean(ALGi_m2,na.rm=T),
  ) %>% 
  pivot_longer(cols=c(CALC.mn,SGi.mn,ALGi.mn),names_to ='BEN',values_to = "Area.m2")

BENcc$BEN = factor(BENcc$BEN , levels = c("SGi.mn","ALGi.mn","CALC.mn"))


BEN.plot = BENcc %>% 
ggplot(aes(x=MY,y=Area.m2,fill=BEN))+
    geom_col(position="stack")+
    ben_FILL_scale  +
    facet_grid(Sub_region~.)+theme_bw()+xlab("Julian Day")+ylab("Benthic Index (m2)")+
    labs(fill="Benthic Class")+
    theme(legend.position = "bottom",
          legend.box = "vertical")
BEN.plot

