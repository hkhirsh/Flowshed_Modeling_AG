## Compare model complexity and skill
## Recreate figures 6 and 7

##Heidi K. Hirsh
##Last edit: Feb 19, 2025

## Clear Workspace ---------------------------------------------------------
rm(list=ls())

## Load Libraries ----------------------------------------------------------
packageload <- c('tidyverse','patchwork','MuMIn','PNWColors','ghibli','ggtext','ggfx','ggmagnify')
lapply(packageload, library, character.only = TRUE)


## Load model output dataframe: 
ModCol=read.csv('Flowshed_Modeling_InputData/ModCol_TrainTest_notOceanic_v2_1.csv') 


## Colors and shapes

## colors for ndays (1-14)
myColors=pnw_palette(name="Bay",n=14,type="continuous")

## shape for each model
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
Fdic.xmax = 1.5 #for 5/6 noOCE
Fdic.xmin = -7.5 #for 5/6 noOCE
Fdic.ymin = .584  #for 4/5/6/7 days noOCE
Fdic.ymax = .609   #for 4/5/6/7 days noOCE

# from.ta 
Fta.xmin = -15
Fta.xmax = 2
Fta.ymin = .48 #for 4/5/6/7 days noOCE
Fta.ymax = .52 #for 4/5/6/7 days noOCE

# to.dic 
Tdic.xmin = -500
Tdic.xmax = -250
Tdic.ymin = .46
Tdic.ymax = .62

# to.ta 
Tta.xmin = -500
Tta.xmax = -250
Tta.ymin = .46
Tta.ymax = .62

# #Add from column and to column to dataframe then match by response (for magnification on Figure 6)

ModCol$From.xmin=NULL
ModCol$From.xmax=NULL
ModCol$From.ymin=NULL
ModCol$From.ymax=NULL

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
fig6=ggplot(ModCol)+
  #Use 5-days backward
  geom_point(data=subset(ModCol,ndays==5),aes(x=dAIC,y=Adj.Rsq.TrainTest,shape=ModName),size=9,stroke=1.3)+
  geom_errorbar(data=subset(ModCol,ndays==5),aes(x=dAIC,y=Adj.Rsq.TrainTest,ymin=Adj.Rsq.TrainTest-seTT,ymax=Adj.Rsq.TrainTest+seTT),
                color=myColors[5],width = 1,size=.6)+
  ylab("Train-Test Adjusted R-squared")+
  facet_wrap(~Response2, labeller=label_parsed)+
  model_SHAPE_scale+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  xlab(expression(paste(Delta, "AIC within model cohort")))+
  scale_x_continuous(minor_breaks=seq(-800,100,10))+
  scale_y_continuous(minor_breaks=seq(.1,.63,.01))+
  theme(legend.spacing.y = unit(3, 'mm')) +
  guides(shape = guide_legend(override.aes = list(size = 5),byrow = TRUE))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),strip.text= element_text(size=20),
        legend.title=element_text(size=20),legend.text=element_text(size=19))+
  theme(legend.position = c(0.86, 0.22),
        legend.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.box.background = element_rect(colour = "black"))+
  theme(legend.key.size = unit(2, 'lines'))+
  geom_magnify(aes(from = From_list, to = To_list), colour='grey',
               shadow = TRUE,axes='xy')
fig6
# ggsave(filename="Flowshed_Modeling_Figures/Figure6.png",plot=fig6,width=20, height=12, dpi = 300)

## Limit models to most complex (to assess differences at high end of model skill):
ModColsub=NULL
ModColsub=dplyr::filter(ModCol, ModName %in%  c("environmental","modMetabolism","year","SRnoYear", "subregion"))
# unique(ModCol$ModName)
# unique(ModColsub$ModName)

fig7=ggplot()+
  geom_point(data=subset(ModColsub,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,color=ndays,shape=ModName),size=6,stroke=1.3)+
  scale_colour_gradientn(colours = myColors[1:14])+
  model_SHAPE_scale+
  facet_wrap(~Response2, labeller=label_parsed,scales="free")+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=20),strip.text= element_text(size=20))+
  scale_x_continuous(breaks=1:1:14)+
  ylab("Train-Test Adjusted R-squared")+
  xlab("Number of Days")+
  theme(legend.position = "none")
fig7
# ggsave(filename="Flowshed_Modeling_Figures/Figure7.png",plot=fig7,width=20, height=12, dpi = 300)


## Figure 7 but showing all models (not only most complex)
fig7all=ggplot(ModCol)+
  geom_point(data=subset(ModCol,ndays<=14),aes(x=ndays,y=Adj.Rsq.TrainTest,color=ndays,shape=ModName),size=6,stroke=1.3)+
  ylab("Train-Test Adjusted R-squared")+
  scale_colour_gradientn(colours = myColors[1:14])+
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
  xlab("Number of Days")
fig7all
