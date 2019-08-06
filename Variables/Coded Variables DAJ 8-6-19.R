library(ggplot2)
library(tidyverse)
library(skimr)
library(tangram)
install.packages("binr")
library(dummies)
library(corrgram)
library(binr)
library(plyr)


train2<- read.csv("c:/Users/daj0079/Desktop/SMU/train.csv")
str(train2)
ncol(train2)
#trainRM<- c(1:21)
trainDJ<-train [,c(22:43)]
str(trainDJ)
#trainAR <-(44:66)
#trainJN <-(67:81)
str(trainDJ)
dj_dum<-train2[,trainDJ]
str(dj_dum)
SalePrice<-train2[,81]
dj_dum<-cbind(SalePrice,dj_dum)
str(dj_dum)


#Need to add transformations where needed### 
##MasVnrArea, BsmtFinSf1, BsmtFunSf2, BsmtUnfSF,  TotalBsmtSF

#set quality measures increasing  1,2,3,4, etc
##here is my 21 variables each set to normalized levels for each variable denoted by normlevel= .n or bin= .b
###########################################################################################################
dj_dum.n<-cbind(RoofStyle.n,RoofMatl.n, Exterior1st.n,Exterior2nd.n, MasVnrType.n, MasVnrArea.b, ExterQual.n, ExterCond.n, Foundation.n, BsmtQual.n,
                BsmtCond.n, BsmtExposure.n, BsmtFinType1.n, BsmtFinSF1.b, BsmtFinType2.n, BsmtFinSF2.b, BsmtUnfSF.b, TotalBsmtSF.b,
                Heating.n,CentralAir.n, Electrical.n)
####################################################################################################
#below is all the code to set levels and bin the variables

#1=Flat
#2=Gable
#3=Gambrel
#4=Hip
#RoofStyleFactor%>% skim
RoofStyle.n<-as.numeric(dj_dum$RoofStyle)
ggplot(data = dj_dum, aes(x=dj_dum$RoofStyle, y=SalePrice, color= RoofStyle))+geom_point(stat ="identity")
ggplot(data = dj_dum, aes(x=RoofStyle.n, y=SalePrice, color= RoofStyle))+geom_point(stat ="identity")


#1=ClyTile
#2=CompShg
#3=Membran
#4=Metal
#5=Roll
#6=Tar&Grv
#7=Wdshake
#8=WdShngl
RoofMatl.n<-as.numeric(dj_dum$RoofMatl)           
ggplot(data = dj_dum, aes(x=dj_dum$RoofMatl, y=SalePrice, color= RoofMatl))+geom_point(stat ="identity")


#1=AsbShng  
#2=AsphShn
#3=BrkComm
#4=BrkFace
#5=CBlock
#6=CemntBd
#7=HdBoard
#8=ImStucc
#9=MetalSd
#10=Plywood
#11=Stone
#12=stucco
#13=VinylSf
#14=WD Sdng
#15=WDShing
Exterior1st.n<-as.numeric(dj_dum$Exterior1st)
ggplot(data = dj_dum, aes(x=dj_dum$Exterior1st, y=SalePrice, color= Exterior1st))+geom_point(stat ="identity")+coord_flip()


#1=AsbShng  
#2=AsphShn
#3=Brk Cmn
#4=BrkFace
#5=CBlock
#6=CemntBd
#7=HdBoard
#8=ImStucc
#9=MetalSd
#10=Other
#11=Plywood
#12=Stone
#13=Stucco
#14=VinylSd
#15=Wd Sdng
#16=Wd Shng
Exterior2nd.n<-as.numeric(dj_dum$Exterior2nd)
ggplot(data = dj_dum, aes(x=Exterior2nd.n, y=SalePrice, color= Exterior2nd))+geom_point(stat ="identity")+coord_flip()


#0=N/A
#1=BrkCmn  
#2=BrkFace
#3=None
#4=Stone
MasVnrType.n<-as.numeric(dj_dum$MasVnrType)
MasVnrType.n[is.na(MasVnrType.n)] <- 0
sum(is.na(MasVnrType.n))
#Masonry veneer type
ggplot(data = dj_dum, aes(x=MasVnrType.n, y=SalePrice, color= MasVnrType))+geom_point(stat ="identity")+coord_flip()


##Binned variable
#MasVnrArea.b 
#0=0 or Na
#1=1-300  
#2=301-600
#3=601-900
#4=901+
MasVnrArea.n<-as.numeric(dj_dum$MasVnrArea)
MasVnrArea.n [is.na(MasVnrArea.n)] <- 0
sum(is.na(MasVnrArea.n))
mvabin<-c(0,300,600,900,1800)
MasVnrArea.b<-.bincode(MasVnrArea.n,mvabin, right = TRUE,include.lowest = FALSE)
MasVnrArea.b [is.na(MasVnrArea.b)] <- 0
ggplot(data = dj_dum, aes(x=dj_dum$MasVnrArea, y=SalePrice, color= MasVnrArea))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(x=MasVnrArea.b, y=SalePrice, color= MasVnrType))+geom_point(stat ="identity")+coord_flip()


#Exter Qual (Ordinal): Evaluates the quality of the material on the exterior 
#4=Ex	Excellent
#3=Gd	Good
#2=TA	Average/Typical
#1=Fa	Fair
EXTQ2<-ifelse(dj_dum$ExterQual=="Fa",1, ifelse(dj_dum$ExterQual=="TA", 2,ifelse(dj_dum$ExterQual=="Gd", 3, ifelse(dj_dum$ExterQual=="Ex",4,0))))
ExterQual.n<-EXTQ2
ggplot(data = dj_dum, aes(x=EXTQ2, y=SalePrice, color= ExterQual))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(x=ExterQual.n, y=SalePrice, color= ExterQual))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(x=dj_dum$ExterQual, y=SalePrice, color= ExterQual))+geom_point(stat ="identity")+coord_flip()


#Exter Cond (Ordinal): Evaluates the present condition of the material on the exterior
#5=Ex	Excellent
#4=Gd	Good
#3=TA	Average/Typical
#2=Fa	Fair
#1=Po	Poor
ExterCond.n<-ifelse(dj_dum$ExterCond=="Po",1, ifelse(dj_dum$ExterCond=="Fa", 2,ifelse(dj_dum$ExterCond=="TA", 3, 
                    ifelse(dj_dum$ExterCond=="Gd",4,ifelse(dj_dum$ExterCond=="Ex",5,0)))))
ggplot(data = dj_dum, aes(x=ExterCond.n, y=SalePrice, color= ExterCond))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(x=dj_dum$ExterCond, y=SalePrice, color= ExterCond))+geom_point(stat ="identity")+coord_flip()


#Foundation.n
#1=BrkTil 
#2=CBlock
#3=PConc
#4=Slab
#5=Stone
#6=Wood
Foundation.n<-as.numeric(dj_dum$Foundation)
ggplot(data = dj_dum, aes(x=Foundation.n, y=SalePrice, color= Foundation))+geom_point(stat ="identity")+coord_flip()


#Bsmt Qual (Ordinal): Evaluates the height of the basement
#5=Ex	Excellent (100+ inches)	
#4=Gd	Good (90-99 inches)
#3=TA	Typical (80-89 inches)
#2=Fa	Fair (70-79 inches)
#1=Po	Poor (<70 inches
#0=NA	No Basement
BsmtQual.n<-ifelse(dj_dum$BsmtQual=="Po",1, ifelse(dj_dum$BsmtQual=="Fa", 2,ifelse(dj_dum$BsmtQual=="TA", 3, 
                                            ifelse(dj_dum$BsmtQual=="Gd",4,ifelse(dj_dum$BsmtQual=="Ex",5,ifelse(dj_dum$BsmtQual=="NA",0,0))))))
BsmtQual.n [is.na(BsmtQual.n)] <- 0
ggplot(data = dj_dum, aes(x=dj_dum$BsmtQual, y=SalePrice, color= BsmtQual))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(x=BsmtQual.n, y=SalePrice, color= BsmtQual))+geom_point(stat ="identity")+coord_flip()


#Bsmt Cond (Ordinal): Evaluates the general condition of the basement
#5=Ex	Excellent
#4=Gd	Good
#3=TA	Typical - slight dampness allowed
#2=Fa	Fair - dampness or some cracking or settling
#1=Po	Poor - Severe cracking, settling, or wetness
#0=NA	No Basement
BsmtCond.n<-ifelse(dj_dum$BsmtCond=="Po",1, ifelse(dj_dum$BsmtCond=="Fa", 2,ifelse(dj_dum$BsmtCond=="TA", 3, 
                                  ifelse(dj_dum$BsmtCond=="Gd",4,ifelse(dj_dum$BsmtCond=="Ex",5,ifelse(dj_dum$BsmtCond=="NA",0,0))))))
BsmtCond.n [is.na(BsmtCond.n)] <- 0
ggplot(data = dj_dum, aes(x=dj_dum$BsmtCond, y=SalePrice, color= BsmtCond))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(x=BsmtCond.n, y=SalePrice, color= BsmtCond))+geom_point(stat ="identity")+coord_flip()


#Bsmt Exposure	(Ordinal): Refers to walkout or garden level walls
#4=Gd	Good Exposure
#3=Av	Average Exposure (split levels or foyers typically score average or above)	
#2=Mn	Mimimum Exposure
#1=No	No Exposure
#0=NA	No Basement
BsmtExposure.n<-ifelse(dj_dum$BsmtExposure=="No",1, ifelse(dj_dum$BsmtExposure=="Mn", 2,ifelse(dj_dum$BsmtExposure=="Av", 3, 
                ifelse(dj_dum$BsmtExposure=="Gd",4,ifelse(dj_dum$BsmtCond=="NA",0,0)))))
BsmtExposure.n [is.na(BsmtExposure.n)] <- 0
ggplot(data = dj_dum, aes(x=dj_dum$BsmtExposure, y=SalePrice, color= BsmtExposure))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(x=BsmtExposure.n, y=SalePrice, color= BsmtExposure))+geom_point(stat ="identity")+coord_flip()



#BsmtFin Type 1	(Ordinal): Rating of basement finished area
#6=GLQ	Good Living Quarters
#5=ALQ	Average Living Quarters
#4=BLQ	Below Average Living Quarters	
#3=Rec	Average Rec Room
#2=LwQ	Low Quality
#1=Unf	Unfinshed
#0=NA	No Basement
BsmtFinType1.n<-ifelse(dj_dum$BsmtFinType1=="Unf",1, ifelse(dj_dum$BsmtFinType1=="LwQ", 2,ifelse(dj_dum$BsmtFinType1=="Rec", 3, 
                    ifelse(dj_dum$BsmtFinType1=="BLQ",4,ifelse(dj_dum$BsmtFinType1=="ALQ",5,ifelse(dj_dum$BsmtFinType1=="GLQ",6,0))))))
BsmtFinType1.n  [is.na(BsmtFinType1.n)] <- 0
ggplot(data = dj_dum, aes(x=dj_dum$BsmtFinType1, y=SalePrice, color= BsmtFinType1))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(x=BsmtFinType1.n, y=SalePrice, color= BsmtFinType1))+geom_point(stat ="identity")+coord_flip()


#Binned
#BsmtFin SF 1 (Continuous): Type 1 finished square feet
#0=0 or Na
#1=1-250 
#2=251-500
#3=501-750
#4=751-1000
#5=1001-1250
#6=1251-1500
#7=1501-1750
#8=1751-2000
#9=+2001
BsmtFinSF1.n<-as.numeric(dj_dum$BsmtFinSF1)
BsmtFinSF1.n[is.na(BsmtFinSF1.n)] <- 0
sum(is.na(BsmtFinSF1.n))
BSMT1<-c(0,250,500,750,1000,1250, 1500, 1750,2000,6500)
BsmtFinSF1.b<-.bincode(BsmtFinSF1.n,BSMT1, right = TRUE,include.lowest = FALSE)
BsmtFinSF1.b[is.na(BsmtFinSF1.b)] <- 0
ggplot(data = dj_dum, aes(x=dj_dum$BsmtFinSF1, y=SalePrice, color= BsmtFinType1))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(x=BsmtFinSF1.b, y=SalePrice, color= dj_dum$BsmtFinSF1))+geom_point(stat ="identity")+coord_flip()


#interesting fact to look at total living area vs basement 
#BsmtFin Type 2	(Ordinal): Rating of basement finished area(if multiple types)
#6=GLQ	Good Living Quarters
#5=ALQ	Average Living Quarters
#4=BLQ	Below Average Living Quarters	
#3=Rec	Average Rec Room
#2=LwQ	Low Quality
#1=Unf	Unfinshed
#0=NA	No Basement
BsmtFinType2.n<-ifelse(dj_dum$BsmtFinType2=="Unf",1, ifelse(dj_dum$BsmtFinType2=="LwQ", 2,ifelse(dj_dum$BsmtFinType2=="Rec", 3, 
                      ifelse(dj_dum$BsmtFinType2=="BLQ",4,ifelse(dj_dum$BsmtFinType2=="ALQ",5,ifelse(dj_dum$BsmtFinType2=="GLQ",6,0))))))
BsmtFinType2.n  [is.na(BsmtFinType2.n)] <- 0
ggplot(data = dj_dum, aes(x=dj_dum$BsmtFinType2, y=SalePrice, color= BsmtFinType2))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(BsmtFinType2.n, y=SalePrice, color= BsmtFinType2))+geom_point(stat ="identity")+coord_flip()

#BINNED
#BsmtFinType 2	(Ordinal): Rating of basement finished area (if multiple types)
#0=0 or Na
#1=1-250 
#2=251-500
#3=501-750
#4=751-1000
#5=1001-1250
#6=1251-1500
BsmtFinSF2.n<-as.numeric(dj_dum$BsmtFinSF2)
BsmtFinSF2.n[is.na(BsmtFinSF2.n)] <- 0
sum(is.na(BsmtFinSF2.n))
BSMT2<-c(0,250,500,750,1000,1250, 1500)
BsmtFinSF2.b<-.bincode(BsmtFinSF2.n,BSMT2, right = TRUE,include.lowest = FALSE)
BsmtFinSF2.b[is.na(BsmtFinSF2.b)] <- 0
ggplot(data = dj_dum, aes(x=dj_dum$BsmtFinSF2, y=SalePrice, color= BsmtFinType2))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(BsmtFinSF2.b, y=SalePrice, color= BsmtFinType2))+geom_point(stat ="identity")+coord_flip()


#BINNED
#Bsmt Unf SF (Continuous): Unfinished square feet of basement area
#0=0 or Na
#1=1-250 
#2=251-500
#3=501-750
#4=751-1000
#5=1001-1250
#6=1251-1500
#7=1501-1750
#8=1751-2000
#9=+2001
BsmtUnfSF.n<-as.numeric(dj_dum$BsmtUnfSF)
BsmtUnfSF.n[is.na(BsmtUnfSF.n)] <- 0
sum(is.na(BsmtUnfSF.n))
UNFBSMT<-c(0,250,500,750,1000,1250, 1500, 1750,2000,6500)
BsmtUnfSF.b<-.bincode(BsmtUnfSF.n,UNFBSMT, right = TRUE,include.lowest = FALSE)
BsmtUnfSF.b[is.na(BsmtUnfSF.b)] <- 0
ggplot(data = dj_dum, aes(x=dj_dum$BsmtUnfSF, y=SalePrice, color= BsmtUnfSF))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(x=BsmtUnfSF.b, y=SalePrice, color= BsmtUnfSF))+geom_point(stat ="identity")+coord_flip()


######IMPORTANT
#Total Bsmt SF (Continuous): Total square feet of basement area
#0=0 or Na
#1=1-250 
#2=251-500
#3=501-750
#4=751-1000
#5=1001-1250
#6=1251-1500
#7=1501-1750
#8=1751-2000
#9=2001-2250
#10=2501-2750
#11=2751-3000
#12=+3000
TotalBsmtSF.n<-as.numeric(dj_dum$TotalBsmtSF)
TotalBsmtSF.n[is.na(TotalBsmtSF.n)] <- 0
sum(is.na(TotalBsmtSF.n))
TOTBSMT<-c(0,250,500,750,1000,1250, 1500, 1750,2000,2250,2500,2750,3000,7000)
TotalBsmtSF.b<-.bincode(TotalBsmtSF.n,TOTBSMT, right = TRUE,include.lowest = FALSE)
TotalBsmtSF.b[is.na(TotalBsmtSF.b)] <- 0
ggplot(data = dj_dum, aes(x=dj_dum$TotalBsmtSF, y=SalePrice, color= TotalBsmtSF))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(TotalBsmtSF.b, y=SalePrice, color= TotalBsmtSF))+geom_point(stat ="identity")+coord_flip()


#Heating	(Nominal): Type of heating
#1=Floor	Floor Furnace
#2=GasA	Gas forced warm air furnace
#3=GasW	Gas hot water or steam heat
#4=Grav	Gravity furnace	
#5=OthW	Hot water or steam heat other than gas
#6=Wall	Wall furnace
Heating.n<-as.numeric(dj_dum$Heating)
ggplot(data = dj_dum, aes(x=dj_dum$Heating, y=SalePrice, color= Heating))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(Heating.n, y=SalePrice, color= Heating))+geom_point(stat ="identity")+coord_flip()


#HeatingQC (Ordinal): Heating quality and condition
#5=Ex	Excellent
#4=Gd	Good
#3=TA	Average/Typical
#2=Fa	Fair
#1=Po	Poor
HeatingQC.n<-ifelse(dj_dum$HeatingQC=="Po",1, ifelse(dj_dum$HeatingQC=="Fa", 2,ifelse(dj_dum$HeatingQC=="TA", 3, 
                                  ifelse(dj_dum$HeatingQC=="Gd",4,ifelse(dj_dum$HeatingQC=="Ex",5,0)))))
HeatingQC.n [is.na(HeatingQC.n)] <- 0
HeatingQC.n
ggplot(data = dj_dum, aes(x=dj_dum$HeatingQC, y=SalePrice, color= HeatingQC))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(x=HeatingQC.n, y=SalePrice, color= HeatingQC))+geom_point(stat ="identity")+coord_flip()


#Central Air (Nominal): Central air conditioning
#0=N	No
#1=Y	Yes
CentralAir.n<-ifelse(dj_dum$CentralAir=="Y",1, ifelse(dj_dum$CentralAir=="N", 0,0))
CentralAir.n [is.na(CentralAir.n)] <- 0
ggplot(data = dj_dum, aes(x=dj_dum$CentralAir, y=SalePrice, color= CentralAir))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(x=CentralAir.n, y=SalePrice, color= CentralAir))+geom_point(stat ="identity")+coord_flip()



#Electrical (Ordinal): Electrical system
#5=SBrkr	Standard Circuit Breakers & Romex
#4=FuseA	Fuse Box over 60 AMP and all Romex wiring (Average)	
#3=FuseF	60 AMP Fuse Box and mostly Romex wiring (Fair)
#2=FuseP	60 AMP Fuse Box and mostly knob & tube wiring (poor)
#1=Mix	Mixed
#0=NA
Electrical.n<-ifelse(dj_dum$Electrical=="Mix",1, ifelse(dj_dum$Electrical=="FuseP", 2,ifelse(dj_dum$Electrical=="FuseF", 3, 
                                ifelse(dj_dum$Electrical=="FuseA",4,ifelse(dj_dum$Electrical=="SBrkr",5,ifelse(dj_dum$Electrical=="NA",0,0))))))
Electrical.n  [is.na(Electrical.n)] <- 0
ggplot(data = dj_dum, aes(x=dj_dum$Electrical, y=SalePrice, color= Electrical))+geom_point(stat ="identity")+coord_flip()
ggplot(data = dj_dum, aes(x=Electrical.n, y=SalePrice, color= Electrical))+geom_point(stat ="identity")+coord_flip()

#lets talk about this 
corrgram(dj_dum.n, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

