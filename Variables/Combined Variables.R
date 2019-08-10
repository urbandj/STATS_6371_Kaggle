install.packages(c("skimr","tangram","fastDummies",'olsrr','rlang','roperators'))
library(tidyverse) #general data wrangling tools
library(skimr) #summary stats
library(tangram) #has is.categorical() function, useful for creating tables
library(car) # Regression tools
library(fastDummies) # creates dummy variables
library(rlang)
library(olsrr)
library(roperators) #used to convert NAs to to different values
library(DAAG)
library(ggplot2)
#create training data object, please refer to this variable when making modificaitons to the dataset----
#'if reading from local source
#'library(readr)
training_data <- train
  #read.csv("//DS1513/AllData/Adam/SMU Data Science Courses/DS 6371 Stats/Project/train.csv")
#'View(train)

#Jeff's Data----

training_data$Fence %na<-% "None" #roperators package

training_data = training_data %>% 
  mutate(logSalePrice = log(SalePrice)) %>%
  mutate(pool_y = if_else(PoolArea > 0,1,0)) %>% #not good predictor, 7 cases with data
  mutate(porchArea = `3SsnPorch` + EnclosedPorch + OpenPorchSF + ScreenPorch) %>%
  mutate(`3SsnPorch_y` = if_else(`3SsnPorch` > 0,1,0)) %>%
  mutate(EnclosedPorch_y = if_else(EnclosedPorch > 0,1,0)) %>%
  mutate(OpenPorchSF_y = if_else(EnclosedPorch > 0,1,0)) %>%
  mutate(ScreenPorch_y = if_else(ScreenPorch > 0,1,0))
 

training_data$porchArea %na<-% 0

training_data$Fence_f = factor(training_data$Fence, levels=c("None", "GdPrv", "GdWo","MnPrv","MnWw"), ordered = FALSE)

#WoodDeckSF Categorical Buckets from Quantitiative Data
training_data$WoodDeckSF %na<-% 0
training_data$WoodDeckSF_group<-cut(training_data$WoodDeckSF, c(0,100,200,300,400,900), ordered_result = FALSE, include.lowest = TRUE)
ggplot(data = training_data, aes(x=WoodDeckSF_group, y=logSalePrice))+geom_point(stat ="identity")

#OpenPorchSF Categorical Buckets from Quantitiative Data
training_data$OpenPorchSF %na<-% 0
training_data$OpenPorchSF_group<-cut(training_data$OpenPorchSF, c(0,50,100,150,200,550), ordered_result = FALSE, include.lowest = TRUE)
ggplot(data = training_data, aes(x=OpenPorchSF_group, y=logSalePrice))+geom_point(stat ="identity")

#EnclosedPorch Categorical Buckets from Quantitiative Data
training_data$EnclosedPorch_group<-cut(training_data$EnclosedPorch, c(0,50,100,150,200,600), ordered_result = FALSE, include.lowest = TRUE)
ggplot(data = training_data, aes(x=EnclosedPorch_group, y=logSalePrice))+geom_point(stat ="identity")

#24 Observations of 3SsnPorch, not good predictor
#'pool not good predictor, 7 cases with data
#'MiscFeature not good predictor


#Adam's Data----

# USE THIS ONE !! Bath FullBath + (0.5 * HalfBath) RECODE to THREE PLUS
training_data$TotalBath <- training_data$FullBath + (0.5 * training_data$HalfBath)

# BedroomAbvGr 4+ bedrooms
training_data$BedroomAbvGr4plus <- ifelse(training_data$BedroomAbvGr >= 4, 4, training_data$BedroomAbvGr)  

Qual4 <- c("Fa", "TA", "Gd", "Ex") # no "Po" data
training_data$KitchenQual.Fac <- factor(x=training_data$KitchenQual, levels=Qual4, ordered = FALSE)
training_data$KitchenQual.FacNum <- as.numeric(training_data$KitchenQual.Fac)

#POSSIBLE TotRmsAbvGrd MAY NEED TO RECODE FOR 10+
training_data$TotRmsAbvGrd10plus <- ifelse(training_data$TotRmsAbvGrd >= 10, 10, training_data$TotRmsAbvGrd)  

# Fireplaces
training_data$Fireplaces = factor(training_data$Fireplaces, levels = c(0,1,3), ordered = FALSE)
#training_data$FireplaceY <- ifelse(training_data$Fireplaces == 0, 0, 1)  

# GarageType different values "2Types Attchd Basment BuiltIn CarPort Detchd"
training_data$GarageType %na<-% "Detchd"

training_data$GarageTypeY <- ifelse(training_data$GarageType == 'Attchd' | training_data$GarageType == 'BuiltIn', 1, 0)  

table(training_data$BsmtExposure)
#Garage Year Built
training_data$GarageYrBlt %na<-% 0

# GarageCars recode Significant 0, 1, 2, 3+ 
GarCar <- c(0, 1, 2, 3, 4) 
training_data$GarageCars <- factor(x=training_data$GarageCars, levels=GarCar, ordered = FALSE)
levels(training_data$GarageCars)[levels(training_data$GarageCars) =="4"] ="3"
GarCar1 <- c(0, 1, 2, 3) 
training_data$GarageCars_f <- factor(x=training_data$GarageCars, levels=GarCar1, ordered = FALSE)
levels(training_data$GarageCars_f)
# GarageArea not as useful
training_data$GarageCond %na<-% "TA"

Qual5 <- c("Po", "Fa", "TA", "Gd", "Ex") 
training_data$GarageCond.Fac <- factor(x=training_data$GarageCond, levels=Qual5, ordered = FALSE)
training_data$GarageCond.N <- as.numeric(training_data$GarageCond.Fac)

# PavedDrive
Pave <- c("N", "P", "Y") 
training_data$PavedDrive.Fac <- factor(x=training_data$PavedDrive, levels=Pave, ordered = FALSE)

# PavedDriveY POSSIBLE
training_data$PavedDriveY <- ifelse(training_data$PavedDrive == "Y", 1, 0)  

#Reannan's Data----

#MSSubClass Categorical Buckets from Quantitiative Data
training_data$MSSubClass %na<-% 0
training_data$MSSubClass_group<-cut(training_data$MSSubClass, c(0,30,60,90,110,200), ordered_result = FALSE, include.lowest = TRUE)
ggplot(data = training_data, aes(x=MSSubClass_group, y=logSalePrice))+geom_point(stat ="identity")

#MSZoning
#1=C (all)
#2=FV
#3=RH
#4=RL
#5=RM
#MSZoningFactor%>% skim
MSZoning.n<-as.numeric(training_data$MSZoning)

table(training_data$Electrical)

training_data$LotFrontage %na<-% 0
training_data$LotFrontage_group = cut(training_data$LotFrontage, c(0,50,80,110,140,320), ordered_result = FALSE, include.lowest = TRUE)
ggplot(data = training_data, aes(x=training_data$LotFrontage_group, y=logSalePrice))+geom_point(stat ="identity")
levels(training_data$LotFrontage_group)

#LotArea Categorical Buckets from Quantitiative Data
group_LotArea <- function(LotArea){
  if (LotArea >= 0 & LotArea <= 3000){
    return('0-3000')
  }else if(LotArea > 3000 & LotArea <= 6000){
    return('3000-6000')
  }else if (LotArea > 6000 & LotArea <= 9000){
    return('6000-9000')
  }else if (LotArea > 9000){
    return('>9000')
  }
}
training_data$LotArea_group <- sapply(training_data$LotArea,group_LotArea)
training_data$LotArea_group <- factor(training_data$LotArea_group, levels = c('0-3000','3000-6000','6000-9000','>9000'), ordered = FALSE)
ggplot(data = training_data, aes(x=training_data$LotArea_group, y=SalePrice, color= LotArea_group))+geom_point(stat ="identity")

##Binned variable
#YearBuilt.b 
#1=1870-1900 
#2=1901-1930
#3=1931-1960
#4=1961-1990
#5=1991-2011
YearBuilt.n<-as.numeric(training_data$YearBuilt)
YearBuilt.n [is.na(YearBuilt.n)] <- 0
sum(is.na(YearBuilt.n))
ybbin<-c(0,1900,1930,1960,1990,2011)
#YearBuilt.b <-.bincode(YearBuilt.n,ybbin, right = TRUE,include.lowest = FALSE)
#YearBuilt.b  [is.na(YearBuilt.b )] <- 0
#training_data$YearBuilt.b <- YearBuilt.b


training_data$YearBuilt.b<-cut(training_data$YearBuilt, ybbin, ordered_result = FALSE, include.lowest = TRUE)

##Binned variable
#YearRemodAdd.b 
#1=1950-1960
#2=1961-1970
#3=1971-1980
#4=1981-1990
#5=1991-2000
#6=2001+
training_data$YearRemodAdd[is.na(training_data$YearRemodAdd)] <- 0
yrabin<-c(0,1960,1970,1980,1990,2000,2012)
training_data$YearRemodAdd.b<-cut(training_data$YearRemodAdd, yrabin, ordered = FALSE, include.lowest = TRUE)

#David's Data----

#below is all the code to set levels and bin the variables

#1=Flat
#2=Gable
#3=Gambrel
#4=Hip
#RoofStyleFactor%>% skim
RoofStyle.n<-as.numeric(training_data$RoofStyle)
training_data$RoofStyleY <- ifelse(training_data$RoofStyle == 'Gable' | training_data$RoofStyle == 'Hip', 1, 0)  


#0=None
#1=BrkCmn  
#2=BrkFace
#3=None
#4=Stone
training_data$MasVnrType %na<-% "None"


##Binned variable
#MasVnrArea.b 
#0=0 or Na
#1=1-300  
#2=301-600
#3=601-900
#4=901+
MasVnrArea.n<-as.numeric(training_data$MasVnrArea)
MasVnrArea.n [is.na(MasVnrArea.n)] <- 0
sum(is.na(MasVnrArea.n))
mvabin<-c(0,300,600,900,1800)
MasVnrArea.b<-.bincode(MasVnrArea.n,mvabin, right = TRUE,include.lowest = FALSE)
MasVnrArea.b [is.na(MasVnrArea.b)] <- 0
training_data$MasVnrArea.b <- MasVnrArea.b

#USE THIS ONE
#Exter Qual (Ordinal): Evaluates the quality of the material on the exterior 
#4=Ex	Excellent
#3=Gd	Good
#2=TA	Average/Typical
#1=Fa	Fair
EXTQ2<-ifelse(training_data$ExterQual=="Fa",1, ifelse(training_data$ExterQual=="TA", 2,ifelse(training_data$ExterQual=="Gd", 3, ifelse(training_data$ExterQual=="Ex",4,0))))
ExterQual.n<-EXTQ2
training_data$ExterQual.n <- ExterQual.n

#Exter Cond (Ordinal): Evaluates the present condition of the material on the exterior
#5=Ex	Excellent
#4=Gd	Good
#3=TA	Average/Typical
#2=Fa	Fair
#1=Po	Poor
ExterCond.n<-ifelse(training_data$ExterCond=="Po",1, ifelse(training_data$ExterCond=="Fa", 2,ifelse(training_data$ExterCond=="TA", 3, 
                                                                                                    ifelse(training_data$ExterCond=="Gd",4,ifelse(training_data$ExterCond=="Ex",5,0)))))
training_data$ExterCond.n <- ExterCond.n

#Foundation.n
#1=BrkTil 
#2=CBlock
#3=PConc
#4=Slab
#5=Stone
#6=Wood
training_data$Foundation.n<-as.numeric(training_data$Foundation)

#USE THIS ONE
#Bsmt Qual (Ordinal): Evaluates the height of the basement
#5=Ex	Excellent (100+ inches)	
#4=Gd	Good (90-99 inches)
#3=TA	Typical (80-89 inches)
#2=Fa	Fair (70-79 inches)
#1=Po	Poor (<70 inches
#0=NA	No Basement
BsmtQual.n<-ifelse(training_data$BsmtQual=="Po",1, ifelse(training_data$BsmtQual=="Fa", 2,ifelse(training_data$BsmtQual=="TA", 3, 
                                                                                                 ifelse(training_data$BsmtQual=="Gd",4,ifelse(training_data$BsmtQual=="Ex",5,ifelse(training_data$BsmtQual=="NA",0,0))))))
BsmtQual.n [is.na(BsmtQual.n)] <- 0
training_data$BsmtQual.n <- BsmtQual.n

#Bsmt Cond (Ordinal): Evaluates the general condition of the basement
#5=Ex	Excellent
#4=Gd	Good
#3=TA	Typical - slight dampness allowed
#2=Fa	Fair - dampness or some cracking or settling
#1=Po	Poor - Severe cracking, settling, or wetness
#0=NA	No Basement
BsmtCond.n<-ifelse(training_data$BsmtCond=="Po",1, ifelse(training_data$BsmtCond=="Fa", 2,ifelse(training_data$BsmtCond=="TA", 3, 
                                                                                                 ifelse(training_data$BsmtCond=="Gd",4,ifelse(training_data$BsmtCond=="Ex",5,ifelse(training_data$BsmtCond=="NA",0,0))))))
BsmtCond.n [is.na(BsmtCond.n)] <- 0
training_data$BsmtCond.n <- BsmtCond.n

#Bsmt Exposure	(Ordinal): Refers to walkout or garden level walls
#4=Gd	Good Exposure
#3=Av	Average Exposure (split levels or foyers typically score average or above)	
#2=Mn	Mimimum Exposure
#1=No	No Exposure
#0=NA	No Basement
BsmtExposure.n<-ifelse(training_data$BsmtExposure=="No",1, ifelse(training_data$BsmtExposure=="Mn", 2,ifelse(training_data$BsmtExposure=="Av", 3, 
                                                                                                             ifelse(training_data$BsmtExposure=="Gd",4,ifelse(training_data$BsmtCond=="NA",0,0)))))
BsmtExposure.n [is.na(BsmtExposure.n)] <- 0
training_data$BsmtExposure.n <- BsmtExposure.n

#BsmtFin Type 1	(Ordinal): Rating of basement finished area
#6=GLQ	Good Living Quarters
#5=ALQ	Average Living Quarters
#4=BLQ	Below Average Living Quarters	
#3=Rec	Average Rec Room
#2=LwQ	Low Quality
#1=Unf	Unfinshed
#0=NA	No Basement
BsmtFinType1.n<-ifelse(training_data$BsmtFinType1=="Unf",1, ifelse(training_data$BsmtFinType1=="LwQ", 2,ifelse(training_data$BsmtFinType1=="Rec", 3, 
                                                                                                               ifelse(training_data$BsmtFinType1=="BLQ",4,ifelse(training_data$BsmtFinType1=="ALQ",5,ifelse(training_data$BsmtFinType1=="GLQ",6,0))))))
BsmtFinType1.n  [is.na(BsmtFinType1.n)] <- 0
training_data$BsmtFinType1.n <- BsmtFinType1.n


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
training_data$BsmtFinSF1[is.na(training_data$BsmtFinSF1)] <- 0
BSMT1<-c(0,250,500,750,1000,1250, 1500, 1750,2000,6500)
training_data$BsmtFinSF1 = cut(training_data$BsmtFinSF1, BSMT1, ordered = FALSE, include.lowest = TRUE)
training_data %>% skim
plot(training_data$BsmtFinSF1)

#interesting fact to look at total living area vs basement 
#BsmtFin Type 2	(Ordinal): Rating of basement finished area(if multiple types)
#6=GLQ	Good Living Quarters
#5=ALQ	Average Living Quarters
#4=BLQ	Below Average Living Quarters	
#3=Rec	Average Rec Room
#2=LwQ	Low Quality
#1=Unf	Unfinshed
#0=NA	No Basement
BsmtFinType2.n<-ifelse(training_data$BsmtFinType2=="Unf",1, ifelse(training_data$BsmtFinType2=="LwQ", 2,ifelse(training_data$BsmtFinType2=="Rec", 3, 
                                                                                                               ifelse(training_data$BsmtFinType2=="BLQ",4,ifelse(training_data$BsmtFinType2=="ALQ",5,ifelse(training_data$BsmtFinType2=="GLQ",6,0))))))
BsmtFinType2.n  [is.na(BsmtFinType2.n)] <- 0
training_data$BsmtFinType2.n <- BsmtFinType2.n

#BINNED
#BsmtFinType 2	(Ordinal): Rating of basement finished area (if multiple types)
#0=0 or Na
#1=1-250 
#2=251-500
#3=501-750
#4=751-1000
#5=1001-1250
#6=1251-1500
training_data$BsmtFinSF2[is.na(training_data$BsmtFinSF2)] <- 0
BSMT2<-c(0,250,500,750,1000,1250, 1500)
training_data$BsmtFinSF2 = cut(training_data$BsmtFinSF2, BSMT2, ordered = FALSE,include.lowest = TRUE)
hist(training_model$BsmtFinSF2)

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

training_data$BsmtUnfSF[is.na(training_data$BsmtUnfSF)] <- 0

UNFBSMT<-c(0,250,500,750,1000,1250, 1500, 1750,2000,6500)
training_data$BsmtUnfSF = cut(training_data$BsmtFinSF1, UNFBSMT, ordered = FALSE,include.lowest = TRUE)

######IMPORTANT USE THIS ONE
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

TotalBsmtSF.n[is.na(TotalBsmtSF.n)] <- 0
TOTBSMT<-c(0,250,500,750,1000,1250, 1500, 1750,2000,2250,2500,2750,3000,7000)
training_model$TotalBsmtSF.b %na<-% 0
training_data$TotalBsmtSF.b = factor(training_data$TotalBsmtSF.b, levels = TOTBSMT, ordered = FALSE)

#Heating	(Nominal): Type of heating
#1=Floor	Floor Furnace
#2=GasA	Gas forced warm air furnace
#3=GasW	Gas hot water or steam heat
#4=Grav	Gravity furnace	
#5=OthW	Hot water or steam heat other than gas
#6=Wall	Wall furnace
training_data$Heating.n<-as.numeric(training_data$Heating)

# Possible
#HeatingQC (Ordinal): Heating quality and condition
#5=Ex	Excellent
#4=Gd	Good
#3=TA	Average/Typical
#2=Fa	Fair
#1=Po	Poor
HeatingQC.n<-ifelse(training_data$HeatingQC=="Po",1, ifelse(training_data$HeatingQC=="Fa", 2,ifelse(training_data$HeatingQC=="TA", 3, 
                                                                                                    ifelse(training_data$HeatingQC=="Gd",4,ifelse(training_data$HeatingQC=="Ex",5,0)))))
HeatingQC.n [is.na(HeatingQC.n)] <- 0
training_data$HeatingQC.n <- HeatingQC.n

#Central Air (Nominal): Central air conditioning
#0=N	No
#1=Y	Yes
CentralAir.n<-ifelse(training_data$CentralAir=="Y",1, ifelse(training_data$CentralAir=="N", 0,0))
CentralAir.n [is.na(CentralAir.n)] <- 0
training_data$CentralAir.n <- CentralAir.n

#Electrical (Ordinal): Electrical system
#5=SBrkr	Standard Circuit Breakers & Romex
#4=FuseA	Fuse Box over 60 AMP and all Romex wiring (Average)	
#3=FuseF	60 AMP Fuse Box and mostly Romex wiring (Fair)
#2=FuseP	60 AMP Fuse Box and mostly knob & tube wiring (poor)
#1=Mix	Mixed
#0=NA
Electrical.n<-ifelse(training_data$Electrical=="Mix",1, ifelse(training_data$Electrical=="FuseP", 2,ifelse(training_data$Electrical=="FuseF", 3, 
                                                                                                           ifelse(training_data$Electrical=="FuseA",4,ifelse(training_data$Electrical=="SBrkr",5,ifelse(training_data$Electrical=="NA",0,0))))))
Electrical.n  [is.na(Electrical.n)] <- 0
training_data$Electrical.n <- Electrical.n

#garage finish
training_data$GarageYrBlt %na<-% 0 
training_data$GarageFinish %na<-% "None"
training_data$GarageQual %na<-% "None"
training_data$FireplaceQu %na<-% "None"
training_data$Electrical %na<-% "None"
training_data$MasVnrType %na<-% "None"
training_data$MasVnrArea %na<-% 0
training_data$BsmtExposure%na<-% "None"
training_data$BsmtCond%na<-% "None"
training_data$BsmtFinType1%na<-% "None"     
training_data$BsmtFinType2%na<-% "None"    
training_data$BsmtQual%na<-% "None"
training_data %<>% mutate(logGrLivArea = log(GrLivArea))
training_data %<>% mutate(totalSF = training_data$`1stFlrSF` + training_data$`2ndFlrSF` + TotalBsmtSF)
