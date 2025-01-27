#12/17 endmember prediction plot. Actual endemember col predictions are in P2 of pipeline


#Seasonality Model for DIC and TA (endmember prediction)

#need PTS dataframe with the transect stations from south florida to predict northern DIC and TA
#need CC dataframe including fractions to calculate admixture endmember chemistry

#based on 'EndMemberFits.R' from Tom Oliver, Feb 7, 2024


## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('stringr','dplyr','ggmap','tidyr','factoextra','sf','mapview','geosphere','ggfortify','lwgeom','gridExtra','ggpubr','patchwork')

#vegan?
lapply(packageload, library, character.only = TRUE)

#  ##!!it can't find the file for checking packages
# # ----Check which packages we actually use----
# # Find which packages do used functions belong to ----
# used.functions <- NCmisc::list.functions.in.file(filename = "/Users/heidi.k.hirsh/Documents/GitHub/Flowshed_Modeling/Model_Pipeline_2024/SFlor_endemember.R", alphabetic = FALSE) |> print()
# # Find which loaded packages are not used ----
# used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
# unused.packages <- packageload[!(packageload %in% used.packages)] |> print()

#_______load northern endmember station data
PTS = read.csv('/Users/heidi.k.hirsh/Documents/FlK_Model1/CC_dataframes/PTS.csv')
PTS$Lat=PTS$Latitude_Dec_Deg
PTS$Long=PTS$Longitude_Dec_Deg
PTS=st_as_sf(PTS, coords = c("Long","Lat"),crs = st_crs(4326)) 
# PTS=cbind(PTS,st_coordinates(PTS))
mapview(PTS,zcol='Year_UTC')

iwant = c("SR","MC")
PTSsub= subset(PTS,line_id %in% iwant)
mapview(PTSsub,zcol='bathymetry')
PTSsub= PTSsub %>% filter(Latitude_Dec_Deg>25) #remove 2 points in the keys
dim(PTSsub)
mapview(PTSsub,zcol='bathymetry')

# testPTSsub = read.csv('/Users/heidi.k.hirsh/Desktop/PTSsub.csv') #only points in the two southern transects
# testPTSsub$Lat=testPTSsub$Latitude_Dec_Deg
# testPTSsub$Long=testPTSsub$Longitude_Dec_Deg
# testPTSsub=st_as_sf(testPTSsub, coords = c("Long","Lat"),crs = st_crs(4326)) 
# mapview(testPTSsub)

PTSsub$xc<-cos(2*pi*PTSsub$YearDay_UTC/366)
PTSsub$xs<-sin(2*pi*PTSsub$YearDay_UTC/366)

#filter by depth (use 5m)
class(PTSsub)
PTSsub3=PTSsub %>% dplyr::filter(bathymetry>3) 
PTSsub4=PTSsub %>% dplyr::filter(bathymetry>4) 
PTSsub5=PTSsub %>% dplyr::filter(bathymetry>5) 
PTSsub6=PTSsub %>% dplyr::filter(bathymetry>6) 


hist(PTSsub3$bathymetry)

plot(sort(PTSsub3$bathymetry))
abline(h=6)
abline(h=5,col='red',lwd=2)
abline(h=4)
abline(h=3)

#ultimately we chose to use the data filtered at 5m

PTSsub3 %>% ggplot()+geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+theme_bw()
# a= PTSsub %>% ggplot()+geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+theme_bw()
# b= PTSsub3 %>% ggplot()+geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+theme_bw()
# c= PTSsub4 %>% ggplot()+geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+theme_bw()
# a//b/c


##_______models
#compare models using data filtered by 3m depth
names(PTSsub3)

##____________Model DIC___________ (with year)
#these models all use the PTSsub3 (3m filter) data

DICmod_0_yr_3m=lm(DIC_umol.kg~YearDay_UTC+Year_UTC,data=PTSsub3)
DICmod_1_yr_3m=lm(DIC_umol.kg~poly(YearDay_UTC,2)+Year_UTC,data=PTSsub3)
DICmod_2_yr_3m=lm(DIC_umol.kg~poly(YearDay_UTC,3)+Year_UTC,data=PTSsub3)
DICmod_3_yr_3m=lm(DIC_umol.kg~xc+xs+Year_UTC,data=PTSsub3) #best
DICmod_4_yr_3m=lm(DIC_umol.kg~xs+Year_UTC,data=PTSsub3)
DICmod_5_yr_3m=lm(DIC_umol.kg~xc+Year_UTC,data=PTSsub3)

anova(DICmod_0_yr_3m,DICmod_1_yr_3m,DICmod_2_yr_3m,DICmod_3_yr_3m,DICmod_4_yr_3m,DICmod_5_yr_3m)
AIC(DICmod_0_yr_3m,DICmod_1_yr_3m,DICmod_2_yr_3m,DICmod_3_yr_3m,DICmod_4_yr_3m,DICmod_5_yr_3m) #DICmod_3_yr_3m is best


#DIC predictions (using year)
PTSsub3$DICpred_0_yr_3m=predict(DICmod_0_yr_3m,newdata=PTSsub3)
PTSsub3$DICpred_1_yr_3m=predict(DICmod_1_yr_3m,newdata=PTSsub3)
PTSsub3$DICpred_2_yr_3m=predict(DICmod_2_yr_3m,newdata=PTSsub3)
PTSsub3$DICpred_3_yr_3m=predict(DICmod_3_yr_3m,newdata=PTSsub3) #best
PTSsub3$DICpred_4_yr_3m=predict(DICmod_4_yr_3m,newdata=PTSsub3)
PTSsub3$DICpred_5_yr_3m=predict(DICmod_5_yr_3m,newdata=PTSsub3)

#DICmod_3 for all depths 
DICmod_3_yr_4m=lm(DIC_umol.kg~xc+xs+Year_UTC,data=PTSsub4)
DICmod_3_yr_5m=lm(DIC_umol.kg~xc+xs+Year_UTC,data=PTSsub5)
DICmod_3_yr_6m=lm(DIC_umol.kg~xc+xs+Year_UTC,data=PTSsub6)

PTSsub4$DICpred_3_yr_4m=predict(DICmod_3_yr_3m,newdata=PTSsub4)
PTSsub5$DICpred_3_yr_5m=predict(DICmod_3_yr_3m,newdata=PTSsub5)
PTSsub6$DICpred_3_yr_6m=predict(DICmod_3_yr_3m,newdata=PTSsub6)


#Plot data versus predicted DIC
ggplot(PTSsub3)+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_yr_3m,color=factor(Year_UTC)))+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+
  theme_bw()



##____________Model DIC___________ (no year)
#also assess which depth filter is best

#How does DICmod_3 without year compare (for PTSsub3) to the version with year
DICmod_3_noY_3m=lm(DIC_umol.kg~xc+xs,data=PTSsub3)
summary(DICmod_3_yr_3m)    #Multiple R-squared:  0.537,	Adjusted R-squared:  0.518 
summary(DICmod_3_noY_3m) #Multiple R-squared:  0.5071,	Adjusted R-squared:  0.4938 

PTSsub3$DICpred_3_noY_3m=predict(DICmod_3_noY_3m,newdata=PTSsub3)

ggplot(PTSsub3)+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_yr_3m,color=factor(Year_UTC)))+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_noY_3m),color='black',linetype='dashed',lwd=1)+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+
  theme_bw()
#the version without year is probably better

##____________Model DIC (cont.)___________ (no year)
#compare depth filters

# DICmod_3noY (calculated for 3m filter above)
DICmod_3_noY_4m=lm(DIC_umol.kg~xc+xs,data=PTSsub4)
DICmod_3_noY_5m=lm(DIC_umol.kg~xc+xs,data=PTSsub5)
DICmod_3_noY_6m=lm(DIC_umol.kg~xc+xs,data=PTSsub6)
# summary(DICmod_3_noY_4m) #Multiple R-squared:  0.5807,	Adjusted R-squared:  0.5612 
# summary(DICmod_3_noY_5m) #Multiple R-squared:  0.6204,	Adjusted R-squared:  0.6024 
# summary(DICmod_3_noY_6m) #Multiple R-squared:  0.651,	Adjusted R-squared:  0.6269 #best but it cuts out a lot of data

PTSsub4$DICpred_3_noY_4m=predict(DICmod_3_noY_5m,newdata=PTSsub4)
PTSsub5$DICpred_3_noY_5m=predict(DICmod_3_noY_5m,newdata=PTSsub5)
PTSsub6$DICpred_3_noY_6m=predict(DICmod_3_noY_6m,newdata=PTSsub6)

#compare filters (3,4,5,6), and yr versus noY for DICmod_3
a=ggplot(PTSsub3)+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_yr_3m,color=factor(Year_UTC)))+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_noY_3m),color='black')+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+
  theme_bw()

b=ggplot(PTSsub4)+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_yr_4m,color=factor(Year_UTC)))+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_noY_4m),color='black')+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+
  theme_bw()

c=ggplot(PTSsub5)+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_yr_5m,color=factor(Year_UTC)))+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_noY_5m),color='black')+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+
  theme_bw()

d=ggplot(PTSsub6)+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_yr_6m,color=factor(Year_UTC)))+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_noY_6m),color='black')+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+
  theme_bw()

a/b/c/d
#use 5m filter (6m cuts out too much)


#double check that the polynomial isn't the right call (DICmod_2) for 5m filter
DICmod_2_yr_5m=lm(DIC_umol.kg~poly(YearDay_UTC,3),data=PTSsub5)#includes year
PTSsub5$DICpred_2_yr_5m=predict(DICmod_2_yr_5m,newdata=PTSsub5) 
# summary(DICmod_2_noY_5m) #Multiple R-squared:  0.5922,	Adjusted R-squared:  0.5624 #not as good as above
AIC(DICmod_3_noY_5m,DICmod_2_noY_5m)

ggplot(PTSsub5)+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_yr_5m,color=factor(Year_UTC)))+ #with year
  geom_line(aes(x=YearDay_UTC,y=DICpred_2_yr_5m,color=factor(Year_UTC)),linetype='dotted')+ #polynomial
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_noY_5m),color='black',linetype='dashed',lwd=1)+ #without year
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+
  theme_bw()

#tried adding predicted DIC into TA model, but it doesn't work with xc and xs because of collinearity 
# lm(DIC_umol.kg~xc+xs+DICpred_3_noY_5m,data=PTSsub5)
# graphics::pairs(PTSsub5[,c('DICpred_3_noY_5m','xs','xc')]) #checking why model won't work with modeled DIC added with xs and xc
plot(PTSsub5$DICpred_3_noY_5m,PTSsub5$xs)
plot(PTSsub5$DICpred_3_noY_5m,PTSsub5$xc)


TAmod_3_noY_5m=lm(TA_umol.kg~xc+xs,data=PTSsub5)
summary(TAmod_3_noY_5m) #Multiple R-squared:  0.4679,	Adjusted R-squared:  0.4425 
PTSsub5$TApred_3_noY_5m=predict(TAmod_3_noY_5m,newdata=PTSsub5)

T=ggplot(PTSsub5)+
  geom_line(aes(x=YearDay_UTC,y=TApred_3_noY_5m),color='black',linetype='dashed',lwd=1)+
  geom_point(aes(x=YearDay_UTC,y=TA_umol.kg,color=factor(Year_UTC)))+
  theme_bw()

D=ggplot(PTSsub5)+
  geom_line(aes(x=YearDay_UTC,y=DICpred_3_noY_5m),color='black',linetype='dashed',lwd=1)+
  geom_point(aes(x=YearDay_UTC,y=DIC_umol.kg,color=factor(Year_UTC)))+
  theme_bw()

D/T
#****of all the code above, we NEED to run the DICmod_3 and TAmod_3 without year for 5m filtered data
#Then apply those models to CCf (data frame including fractions) to determine relevant endmember


#now run models on data set that also includes source water fraction (north_fraction and south_fraction)

CCf= read.csv('/Users/heidi.k.hirsh/Documents/GitHub/test-flk/0_InputData/CCfractions_fix.csv')
# View(CCf)
# write.csv(CCf,'/Users/heidi.k.hirsh/Desktop/CCfTEST.csv',row.names=F)
# CCftest = read.csv('/Users/heidi.k.hirsh/Desktop/CCfTEST.csv')
# View(CCftest)
CCf$Lat=CCf$Latitude
CCf$Long=CCf$Longitude
CCf.sf = st_as_sf(CCf, coords = c("Long","Lat"),crs = st_crs(4326)) 

#map northern reference points with southern oceaneanic points 
mapview(PTSsub5,zcol='Month_UTC')+
  mapview(subset(CCf.sf,Zone=='Oceanic'),zcol='MoY')


CCoceanRef = CCf %>% dplyr::filter(Zone=="Oceanic") %>% dplyr::group_by(Year,Month) %>% 
  dplyr::summarize(TAoce_mean= mean(TA_umol_kg, na.rm=T),
                   DICoce_mean= mean(DIC_umol_kg, na.rm=T),
                   ARAGoce_mean= mean(Aragonite_Sat_W, na.rm=T),
                   Toce_mean= mean(Temperature_C, na.rm=T),
                   Soce_mean= mean(Salinity_CTD, na.rm=T))
CCf.ref = dplyr::left_join(CCf,CCoceanRef,by=c("Year","Month"))
# names(CCf.ref)
# Note: oce_mean values are mean of oceanic station values BELOW the Keys

CCf.ref$xc=cos(2*pi*CCf.ref$jday.utc/366)
CCf.ref$xs=sin(2*pi*CCf.ref$jday.utc/366)


#Look at fractions of time spent north and south of the keys: 
CCf.ref %>% dplyr::filter(ndays==14) %>%
  ggplot()+
  geom_histogram(aes(x=north_fraction,stat(density)))+
  facet_wrap('Sub_region')+
  theme_bw()

#we don't want to ever use north_fraction for BB
CCf.ref %>% dplyr::filter(ndays==14) %>% dplyr::group_by(Sub_region) %>%
  dplyr::summarize(mean_nf=mean(north_fraction,na.rm=T))


##____________Predict DIC and TA endmember chemistry___________ 

#Use mod_3 models to predict northern reference (above Keys) from 5m filtered data
CCf.ref$DICrefN = predict(DICmod_3_noY_5m,newdata=CCf.ref)
CCf.ref$TArefN = predict(TAmod_3_noY_5m,newdata=CCf.ref)

#Recalculate Florida Bay "FB" fraction instead of northern_fraction (make sure BB never gets north fraction from FB)
CCf.ref$FBfraction = CCf.ref$north_fraction 
CCf.ref$FBfraction[CCf.ref$Sub_region=="BB"]=0

#Calculate south of Keys (Soce) fraction (acounting for the BB fractions always being entirely "south" water)
CCf.ref$Soce_fraction = 1-CCf.ref$FBfraction
# BBay = CCf.ref %>% filter(Sub_region=="BB")
# table(BBay$Soce_fraction) #all 1 (good)

#calculate admixture endmembers for DIC and TA
CCf.ref$DICrefNS = CCf.ref$FBfraction*CCf.ref$DICrefN + CCf.ref$Soce_fraction*CCf.ref$DICoce_mean
CCf.ref$TArefNS = CCf.ref$FBfraction*CCf.ref$TArefN + CCf.ref$Soce_fraction*CCf.ref$TAoce_mean

#Compare endmember chemistry!
JJ=8
end =CCf.ref%>% dplyr::filter(ndays==14) %>% 
# CCf.ref %>% filter(Year==2019) %>% filter(ndays==14) %>% 
ggplot()+
  geom_jitter(width=JJ,height=JJ,aes(x=DICrefNS,y=TArefNS,color=factor(MoY)),shape=17,alpha=.75,size=2)+
  geom_point(aes(x=DICrefN,y=TArefN,fill=factor(MoY)),shape=21,size=3,color='black',alpha=.1)+
  geom_point(aes(x=DICoce_mean,y=TAoce_mean,fill=factor(MoY)),shape=24,size=3,color='black')+
  # add the special 6 points!
  facet_wrap('Sub_region')+
  theme_bw()
# ggsave(end,file="/Users/heidi.k.hirsh/Desktop/endmemberFigs/endmember.png")
end



# Is low salinity a problem
# no, we are already filtering out most with the 5m filter
PTSsub %>% 
  ggplot()+
  geom_point(aes(x=bathymetry,y=CTDSAL_PSS78))+
  geom_vline(xintercept=5,color='red')+
  geom_vline(xintercept=6,color='gray')+geom_vline(xintercept=4,color='gray')+geom_vline(xintercept=3,color='gray')+
  theme_bw()


# Do DIC and TA differ meaningfully across regions at the Oceanic stations? --> NO
CC=CCc
CC=CCf
ggplot(PTSsub5)+
  geom_point(aes(x=DIC_umol.kg,y=TA_umol.kg))+ #,color=factor(Month_UTC)))+ #black points = measured DIC and TA (Gulf side)
  geom_point(aes(x=DICpred_3_noY_5m,y=TApred_3_noY_5m),shape=17,size=4)+ #triangles = predicted DIC and TA
  geom_point(data=subset(CC,Zone=="Oceanic"),aes(x=DIC_umol_kg,y=TA_umol_kg,color=Sub_region)) #oceanic data on Atlantic side

 
ggplot(PTSsub5)+geom_point(data=subset(CC,Zone=="Oceanic"),aes(x=DIC_umol_kg,y=TA_umol_kg,color=Sub_region))+
  facet_wrap("Season")+theme_bw()

a=ggplot()+geom_boxplot(data=subset(CC,Zone=="Oceanic"),aes(x=Sub_region,y=DIC_umol_kg))
b=ggplot()+geom_boxplot(data=subset(CC,Zone=="Oceanic"),aes(x=Sub_region,y=TA_umol_kg))
a/b




#output dataframe including relevant endmembers
class(CCf)
# write.csv(CCf,'/Users/heidi.k.hirsh/Desktop/CCf_end.csv',row.names=F)




#__________________________________________________________________________
# Play dataset
#
# Nsamples=500
# Err_Gain=50
# YEAR_Gain=25
# YDAY_Gain=150
#
# FD=data.frame(YEAR=sample(seq(2000,2023,by=4),Nsamples,replace = T),MONTH=sample(seq(1,12,2),Nsamples,replace=T))
# FD$YDAY=FD$MONTH*30-15+sample(-3:3,Nsamples,replace=T)
# FD$rDIC=1700 + YDAY_Gain*sin(2*pi*(FD$YDAY/366-.75)) + YEAR_Gain*(FD$YEAR-2010)+Err_Gain*rnorm(Nsamples) 
# FD$xc<-cos(2*pi*FD$YDAY/366)
# FD$xs<-sin(2*pi*FD$YDAY/366)

# FD %>% ggplot()+geom_point(aes(x=YDAY,y=rDIC,color=factor(YEAR)))+theme_bw()

# DICmod_0=lm(rDIC~YDAY+YEAR,data=FD)
# DICmod_1=lm(rDIC~poly(YDAY,2)+YEAR,data=FD)
# DICmod_2=lm(rDIC~poly(YDAY,3)+YEAR,data=FD)

# DICmod_3=lm(rDIC~xc+xs+YEAR,data=FD)
# DICmod_4=lm(rDIC~xs+YEAR,data=FD)
# DICmod_5=lm(rDIC~xc+YEAR,data=FD)

# FD$Pred_0=predict(DICmod_0,newdata=FD)
# FD$Pred_1=predict(DICmod_1,newdata=FD)
# FD$Pred_2=predict(DICmod_2,newdata=FD)
# FD$Pred_3=predict(DICmod_3,newdata=FD)
# FD$Pred_4=predict(DICmod_4,newdata=FD)
# FD$Pred_5=predict(DICmod_5,newdata=FD)

# FD=FD %>% dplyr::arrange(YEAR,YDAY) 

# ggplot(FD)+
#   geom_line(aes(x=YDAY,y=Pred_5,color=factor(YEAR)))+
#   geom_point(aes(x=YDAY,y=rDIC,color=factor(YEAR)))+
#   theme_bw()

# anova(DICmod_3,DICmod_5)
# anova(DICmod_1,DICmod_3)



# #understand sin and cospar(mfrow=c(2,2))
# a=1;b=0
# plot(FD$YDAY,aFD$xc+bFD$xs,type="p",main=paste("A = ",a," B = ",b))
# a=0;b=1
# plot(FD$YDAY,aFD$xc+bFD$xs,type="p",main=paste("A = ",a," B = ",b))
# a=.25;b=.75
# plot(FD$YDAY,aFD$xc+bFD$xs,type="p",main=paste("A = ",a," B = ",b))
# a=.5;b=.5
# plot(FD$YDAY,aFD$xc+bFD$xs,type="p",main=paste("A = ",a," B = ",b))