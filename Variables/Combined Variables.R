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
library(dplyr)
install.packages("DAAG")
library(plyr)


#create training data object, please refer to this variable when making modificaitons to the dataset----
#'if reading from local source
library(readr)
train<- read.csv("c:/Users/daj0079/Desktop/SMU/train.csv")
train%>%skim

training_data <- train


#Jeff's Data----

training_data <-training_data[-c(7)]
drop<- c("PoolQC","MiscFeature")
training_data = training_data[,!(names(training_data) %in% drop)]



training_data = training_data %>% 
  mutate(logSalePrice = log(SalePrice)) %>%
  mutate(pool_yn = if_else(PoolArea == 0,0,1)) %>% #not good predictor, 7 cases with data
  mutate(porch_yn = if_else(("3SsnPorch" == 0 || EnclosedPorch == 0 || OpenPorchSF == 0 || ScreenPorch == 0),0,1)) %>%
  mutate(porchArea = training_data$X3SsnPorch + EnclosedPorch + OpenPorchSF + ScreenPorch) %>%
  mutate(X3SsnPorch_y = if_else(X3SsnPorch > 0,1,0)) %>%
  mutate(EnclosedPorch_y = if_else(EnclosedPorch > 0,1,0)) %>%
  mutate(OpenPorchSF_y = if_else(EnclosedPorch > 0,1,0)) %>%
  mutate(ScreenPorch_y = if_else(ScreenPorch > 0,1,0))#if a home has any features related to a porch then porch_yn = 1

training_data$Fence = factor(training_data$Fence, levels=c("None", "GdPrv", "GdWo","MnPrv","MnWw"), ordered = FALSE)
training_data$Fence %na<-% "None" #roperators package
sum(is.na(training_data$Fence))

#training_data%>%skim

#WoodDeckSF Categorical Buckets from Quantitiative Data
training_data$WoodDeckSF %na<-% 0
training_data$WoodDeckSF_group<-cut(training_data$WoodDeckSF, c(0,100,200,300,400,900), ordered_result = FALSE, include.lowest = TRUE)
ggplot(data = training_data, aes(x=WoodDeckSF_group, y=logSalePrice))+geom_point(stat ="identity")

#OpenPorchSF Categorical Buckets from Quantitiative Data
training_data$OpenPorchSF %na<-% 0
training_data$OpenPorchSF_group<-cut(training_data$OpenPorchSF, c(0,50,100,150,200,550), ordered_result = FALSE, include.lowest = TRUE)
ggplot(data = training_data, aes(x=OpenPorchSF_group, y=logSalePrice))+geom_point(stat ="identity")

#EnclosedPorch Categorical Buckets from Quantitiative Data
training_data$EnclosedPorch_group %na<-% 0
training_data$EnclosedPorch_group<-cut(training_data$EnclosedPorch, c(0,50,100,150,200,600), ordered_result = FALSE, include.lowest = TRUE)
ggplot(data = training_data, aes(x=EnclosedPorch_group, y=logSalePrice))+geom_point(stat ="identity")

#24 Observations of 3SsnPorch, not good predictor


#'pool not good predictor, 7 cases with data
#'MiscFeature not good predictor
#'Interaction variables for porch
#' porch_yn*3SsnPorch
#' porch_yn*EnclosedPorch
#' porch_yn*OpenPorchSF
#' porch_yn*ScreenPorch



#Adam's Data----



# USE THIS ONE !! Bath FullBath + (0.5 * HalfBath) RECODE to THREE PLUS
training_data$TotalBath <- training_data$FullBath + (0.5 * training_data$HalfBath)
ggplot(data = training_data, aes(x=TotalBath, y=logSalePrice))+geom_point(stat ="identity")
sum(is.na(training_data$TotalBath))

# BedroomAbvGr 4+ bedrooms
training_data$BedroomAbvGr4plus <- ifelse(training_data$BedroomAbvGr >= 4, 4, training_data$BedroomAbvGr)  
ggplot(data = training_data, aes(x=BedroomAbvGr4plus, y=logSalePrice))+geom_point(stat ="identity")
sum(is.na(training_data$BedroomAbvGr4plus))

drop<- c("BedroomAbvGr")
training_data = training_data[,!(names(training_data) %in% drop)]



Qual4 <- c("Fa", "TA", "Gd", "Ex") # no "Po" data
training_data$KitchenQual <- factor(x=training_data$KitchenQual, levels=Qual4, ordered = FALSE)

ggplot(data = training_data, aes(x=KitchenQual, y=logSalePrice))+geom_point(stat ="identity")
sum(is.na(training_data$KitchenQual))

#POSSIBLE TotRmsAbvGrd MAY NEED TO RECODE FOR 10+
training_data$TotRmsAbvGrd10plus <- ifelse(training_data$TotRmsAbvGrd >= 10, 10, training_data$TotRmsAbvGrd) 
ggplot(data = training_data, aes(x=TotRmsAbvGrd10plus, y=logSalePrice))+geom_point(stat ="identity")

drop<- c("TotRmsAbvGr")
training_data = training_data[,!(names(training_data) %in% drop)]

# Fireplaces
training_data$FireplaceY <- ifelse(training_data$Fireplaces == 0, 0, 1)  
ggplot(data = training_data, aes(x=FireplaceY, y=logSalePrice))+geom_point(stat ="identity")

drop<- c("Fireplace")
training_data = training_data[,!(names(training_data) %in% drop)]


# GarageType different values "2Types Attchd Basment BuiltIn CarPort Detchd"
training_data$GarageType %na<-% "Detchd"
ggplot(data = training_data, aes(x=GarageType, y=logSalePrice))+geom_point(stat ="identity")
training_data$GarageTypeY <- ifelse(training_data$GarageType == 'Attchd' | training_data$GarageType == 'BuiltIn', 1, 0)  
ggplot(data = training_data, aes(x=GarageTypeY, y=logSalePrice))+geom_point(stat ="identity")

##### strange may want check buckets
#Garage Year Built
training_data$GarageYrBlt %na<-% 0
ggplot(data = training_data, aes(x=GarageYrBlt, y=logSalePrice))+geom_point(stat ="identity")

# GarageCars recode Significant 0, 1, 2, 3+ 
GarCar <- c(0, 1, 2, 3, 4) 
training_data$GarageCars<- factor(x=training_data$GarageCars, levels=GarCar, ordered = FALSE)
#training_data$GarageCars3plus <- ifelse(training_data$GarageCars == 4, 3, training_data$GarageCars) 
#ggplot(data = training_data, aes(x=GarageCars3plus, y=logSalePrice))+geom_point(stat ="identity")
ggplot(data = training_data, aes(x=GarageCars, y=logSalePrice))+geom_point(stat ="identity")

sum(is.na(training_data$GarageCars))

# GarageArea not as useful

training_data$GarageCond %na<-% 0

Qual5 <- c("Po", "Fa", "TA", "Gd", "Ex") 
training_data$GarageCond <- factor(x=training_data$GarageCond, levels=Qual5, ordered = FALSE)
training_data$GarageCond <- as.numeric(training_data$GarageCond)
training_data$GarageCond %na<-% 0
ggplot(data = training_data, aes(x=GarageCond, y=logSalePrice))+geom_point(stat ="identity")


# PavedDrive
Pave <- c("N", "P", "Y") 
training_data$PavedDrive <- factor(x=training_data$PavedDrive, levels=Pave, ordered = FALSE)
ggplot(data = training_data, aes(x=PavedDrive, y=logSalePrice))+geom_point(stat ="identity")

# PavedDriveY POSSIBLE
#training_data$PavedDriveY <- ifelse(training_data$PavedDrive == "Y", 1, 0)  

#ggplot(data = training_data, aes(x=PavedDriveY, y=logSalePrice))+geom_point(stat ="identity")


#Reannan's Data----

#MSSubClass Categorical Buckets from Quantitiative Data
group_MSSubClass <- function(MSSubClass){
  if (MSSubClass >= 0 & MSSubClass <= 30){
    return('0-30')
  }else if(MSSubClass > 30 & MSSubClass <= 60){
    return('30-60')
  }else if (MSSubClass > 60 & MSSubClass <= 90){
    return('60-90')
  }else if (MSSubClass > 90 & MSSubClass <=110){
    return('90-110')
  }else if (MSSubClass > 110){
    return('>110')
  }
}
training_data$MSSubClass <- sapply(training_data$MSSubClass,group_MSSubClass)
training_data$MSSubClass<- factor(training_data$MSSubClass, levels = c('0-30','30-60','60-90','90-110','>110'), ordered = FALSE)
ggplot(data = training_data, aes(x=MSSubClass, y=logSalePrice))+geom_point(stat ="identity")


#MSZoning
#1=C (all)
#2=FV
#3=RH
#4=RL
#5=RM
#MSZoningFactor%>% skim

ggplot(data = training_data, aes(x=MSZoning, y=logSalePrice))+geom_point(stat ="identity")
sum(is.na(training_data$MSZoning))

# training_data$LotFrontage %na<-% 0

as.numeric(training_data$LotFrontage)
training_data$LotFrontage %na<-% 0
group_LotFrontage <- function(LotFrontage){
  if (LotFrontage >=0 & LotFrontage <= 50){
    return('0-50')
  }else if(LotFrontage > 50 & LotFrontage <= 80){
    return('50-80')
  }else if (LotFrontage > 80 & LotFrontage <= 110){
    return('80-110')
  }else if (LotFrontage > 110 & LotFrontage <= 140){
    return('110-140')
  }else if (LotFrontage > 140){
    return('>140')
  }
}
training_data$LotFrontage <- sapply(training_data$LotFrontage,group_LotFrontage)
training_data$LotFrontage <- factor(training_data$LotFrontage, levels = c('0-50','50-80','80-110','110-140','>140'), ordered = FALSE)
#levels(training_data$LotFrontage_group)
ggplot(data = training_data, aes(x=LotFrontage, y=logSalePrice))+geom_point(stat ="identity")
sum(is.na(training_data$LotFrontage))


#LotArea Categorical Buckets from Quantitiative Data
as.numeric(training_data$LotArea)
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
training_data$LotArea <- sapply(training_data$LotArea,group_LotArea)
training_data$LotArea <- factor(training_data$LotArea, levels = c('0-3000','3000-6000','6000-9000','>9000'), ordered = FALSE)
ggplot(data = training_data, aes(x=training_data$LotArea, y=SalePrice))+geom_point(stat ="identity")
sum(is.na(training_data$LotArea))


#LandSlope
#1=Gtl
#2=Mod
#3=Sev
ordered(training_data$LandSlope)
ggplot(data = training_data, aes(x=training_data$LandSlope, y=SalePrice))+geom_point(stat ="identity")

#Neighborhood
#1=Blmngtn
#2=Blueste
#3=BrDale
#4=BrkSide
#5=ClearCr
#6=CollgCr
#7=CrawFor
#8=Edwards
#9=Gilbert
#10=IDOTRR
#11=MeadowV
#12=Mitchel
#13=NAmes
#14=NoRidge
#15=NPKVil
#16=NridgHt
#17=NWAmes
#18=OldTown
#19=Sawyer
#20=SawyerW
#21=Somerst
#22=StonBr
#23=SWISU
#24=Timber
#25=Veenker
ordered(training_data$Neighborhood)
ggplot(data = training_data, aes(x=training_data$Neighborhood, y=SalePrice))+geom_point(stat ="identity")




#OverallQual
#1=1
#2=2
#3=3
#4=4
#5=5
#6=6
#7=7
#8=8
#9=9
#10=10
ordered(training_data$OverallQual)
ggplot(data = training_data, aes(x=training_data$OverallQual, y=SalePrice))+geom_point(stat ="identity")



#OverallCond
#1=1
#2=2
#3=3
#4=4
#5=5
#6=6
#7=7
#8=8
#9=9
ordered(training_data$OverallCond)
ggplot(data = training_data, aes(x=training_data$OverallCond, y=SalePrice))+geom_point(stat ="identity")





#YearBuilt 
#1=1870-1900 
#2=1901-1930
#3=1931-1960
#4=1961-1990
#5=1991-2011
training_data = training_data %>% 
  mutate(logYearBuilt = log(YearBuilt))
hist(training_data$logYearBuilt)
sum(is.na(training_data$YearBuilt))


training_data%>%skim
##Binned variable
#YearRemodAdd.b 
#1=1950-1960
#2=1961-1970
#3=1971-1980
#4=1981-1990
#5=1991-2000
#6=2001+

training_data$YearRemodAdd[is.na(training_data$YearRemodAdd)] <- "NONE"
training_data$YearRemodAdd

ggplot(data = training_data, aes(x=YearRemodAdd, y=logSalePrice))+geom_point(stat ="identity")


#David's Data----




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
training_data$MasVnrArea<- MasVnrArea.b
fct_explicit_na(training_data$MasVnrArea, na_level = "None")->training_data$MasVnrArea
sum(is.na(training_data$MasVnrArea))
ggplot(data = training_data, aes(x=training_data$MasVnrArea, y=SalePrice))+geom_point(stat ="identity")


#USE THIS ONE
#Exter Qual (Ordinal): Evaluates the quality of the material on the exterior 
#4=Ex	Excellent
#3=Gd	Good
#2=TA	Average/Typical
#1=Fa	Fair
#0=NONE
EXTQ2<-ifelse(training_data$ExterQual=="Fa",1, ifelse(training_data$ExterQual=="TA", 2,ifelse(training_data$ExterQual=="Gd", 3, ifelse(training_data$ExterQual=="Ex",4,0))))
ExterQual<-EXTQ2
training_data$ExterQual<- ExterQual
ggplot(data = training_data, aes(x=training_data$ExterQual, y=SalePrice))+geom_point(stat ="identity")


#Exter Cond (Ordinal): Evaluates the present condition of the material on the exterior
#5=Ex	Excellent
#4=Gd	Good
#3=TA	Average/Typical
#2=Fa	Fair
#1=Po	Poor
ExterCond<-ifelse(training_data$ExterCond=="Po",1, ifelse(training_data$ExterCond=="Fa", 2,ifelse(training_data$ExterCond=="TA", 3, 
                                                                                                  ifelse(training_data$ExterCond=="Gd",4,ifelse(training_data$ExterCond=="Ex",5,0)))))

ggplot(data = training_data, aes(x=training_data$ExterCond, y=SalePrice))+geom_point(stat ="identity")


#USE THIS ONE
#Bsmt Qual (Ordinal): Evaluates the height of the basement
#5=Ex	Excellent (100+ inches)	
#4=Gd	Good (90-99 inches)
#3=TA	Typical (80-89 inches)
#2=Fa	Fair (70-79 inches)
#1=Po	Poor (<70 inches
#0=NA	No Basement
BsmtQual<-ifelse(training_data$BsmtQual=="Po",1, ifelse(training_data$BsmtQual=="Fa", 2,ifelse(training_data$BsmtQual=="TA", 3, 
                                                                                               ifelse(training_data$BsmtQual=="Gd",4,ifelse(training_data$BsmtQual=="Ex",5,ifelse(training_data$BsmtQual=="NA",0,0))))))
BsmtQual [is.na(BsmtQual)] <- "NONE"
fct_explicit_na(training_data$BsmtQual, na_level = "None")->training_data$BsmtQual

ggplot(data = training_data, aes(x=training_data$BsmtQual, y=SalePrice))+geom_point(stat ="identity")
sum(is.na(training_data$BsmtQual))

#Bsmt Cond (Ordinal): Evaluates the general condition of the basement
#5=Ex	Excellent
#4=Gd	Good
#3=TA	Typical - slight dampness allowed
#2=Fa	Fair - dampness or some cracking or settling
#1=Po	Poor - Severe cracking, settling, or wetness
#0=NA	No Basement
BsmtCond<-ifelse(training_data$BsmtCond=="Po",1, ifelse(training_data$BsmtCond=="Fa", 2,ifelse(training_data$BsmtCond=="TA", 3, 
                                                                                               ifelse(training_data$BsmtCond=="Gd",4,ifelse(training_data$BsmtCond=="Ex",5,ifelse(training_data$BsmtCond=="NA",0,0))))))
fct_explicit_na(training_data$BsmtCond, na_level = "TA")->training_data$BsmtCond
ggplot(data = training_data, aes(x=training_data$BsmtCond, y=SalePrice))+geom_point(stat ="identity")
sum(is.na(training_data$BsmtCond))

#Bsmt Exposure	(Ordinal): Refers to walkout or garden level walls
#4=Gd	Good Exposure
#3=Av	Average Exposure (split levels or foyers typically score average or above)	
#2=Mn	Mimimum Exposure
#1=No	No Exposure
#0=NA	No Basement
BsmtExposure<-ifelse(training_data$BsmtExposure=="No",1, ifelse(training_data$BsmtExposure=="Mn", 2,ifelse(training_data$BsmtExposure=="Av", 3, 
                                                                                                           ifelse(training_data$BsmtExposure=="Gd",4,ifelse(training_data$BsmtCond=="NA",0,0)))))
fct_explicit_na(training_data$BsmtExposure, na_level = "None")->training_data$BsmtExposure
sum(is.na(training_data$BsmtExposure))
ggplot(data = training_data, aes(x=training_data$BsmtExposure, y=SalePrice))+geom_point(stat ="identity")

#BsmtFin Type 1	(Ordinal): Rating of basement finished area
#6=GLQ	Good Living Quarters
#5=ALQ	Average Living Quarters
#4=BLQ	Below Average Living Quarters	
#3=Rec	Average Rec Room
#2=LwQ	Low Quality
#1=Unf	Unfinshed
#0=NA	No Basement
BsmtFinType1<-ifelse(training_data$BsmtFinType1=="Unf",1, ifelse(training_data$BsmtFinType1=="LwQ", 2,ifelse(training_data$BsmtFinType1=="Rec", 3, 
                                                                                                             ifelse(training_data$BsmtFinType1=="BLQ",4,ifelse(training_data$BsmtFinType1=="ALQ",5,ifelse(training_data$BsmtFinType1=="GLQ",6,0))))))

fct_explicit_na(training_data$BsmtFinType1, na_level = "None")->training_data$BsmtFinType1
sum(is.na(training_data$BsmtFinType1))
ggplot(data = training_data, aes(x=training_data$BsmtFinType1, y=SalePrice))+geom_point(stat ="identity")




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
BsmtFinSF1.n<-as.numeric(training_data$BsmtFinSF1)
BsmtFinSF1.n[is.na(BsmtFinSF1.n)] <- 0
sum(is.na(BsmtFinSF1.n))
BSMT1<-c(0,250,500,750,1000,1250, 1500, 1750,2000,6500)
BsmtFinSF1.b<-.bincode(BsmtFinSF1.n,BSMT1, right = TRUE,include.lowest = FALSE)
BsmtFinSF1.b[is.na(BsmtFinSF1.b)] <- 0
training_data$BsmtFinSF1<- BsmtFinSF1.b
ggplot(data = training_data, aes(x=BsmtFinSF1, y=SalePrice))+geom_point(stat ="identity")

sum(is.na(training_data$BsmtFinSF1))


#interesting fact to look at total living area vs basement 
#BsmtFin Type 2	(Ordinal): Rating of basement finished area(if multiple types)
#6=GLQ	Good Living Quarters
#5=ALQ	Average Living Quarters
#4=BLQ	Below Average Living Quarters	
#3=Rec	Average Rec Room
#2=LwQ	Low Quality
#1=Unf	Unfinshed
#0=NA	No Basement
BsmtFinType2<-ifelse(training_data$BsmtFinType2=="Unf",1, ifelse(training_data$BsmtFinType2=="LwQ", 2,ifelse(training_data$BsmtFinType2=="Rec", 3, 
                                                                                                             ifelse(training_data$BsmtFinType2=="BLQ",4,ifelse(training_data$BsmtFinType2=="ALQ",5,ifelse(training_data$BsmtFinType2=="GLQ",6,0))))))
fct_explicit_na(training_data$BsmtFinType2, na_level = "None")->training_data$BsmtFinType2
sum(is.na(training_data$BsmtFinType2))
ggplot(data = training_data, aes(x=BsmtFinType2, y=SalePrice))+geom_point(stat ="identity")

#BINNED
#BsmtFinType 2	(Ordinal): Rating of basement finished area (if multiple types)
#0=0 or Na
#1=1-250 
#2=251-500
#3=501-750
#4=751-1000
#5=1001-1250
#6=1251-1500
BsmtFinSF2.n<-as.numeric(training_data$BsmtFinSF2)
BsmtFinSF2.n[is.na(BsmtFinSF2.n)] <- 0
sum(is.na(BsmtFinSF2.n))
BSMT2<-c(0,250,500,750,1000,1250, 1500)
BsmtFinSF2.b<-.bincode(BsmtFinSF2.n,BSMT2, right = TRUE,include.lowest = FALSE)
BsmtFinSF2.b[is.na(BsmtFinSF2.b)] <- 0
training_data$BsmtFinSF2 <- BsmtFinSF2.b
ggplot(data = training_data, aes(x=BsmtFinSF2, y=SalePrice))+geom_point(stat ="identity")

sum(is.na(training_data$BsmtFinSF2))

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
BsmtUnfSF.n<-as.numeric(training_data$BsmtUnfSF)
BsmtUnfSF.n[is.na(BsmtUnfSF.n)] <- 0
sum(is.na(BsmtUnfSF.n))
UNFBSMT<-c(0,250,500,750,1000,1250, 1500, 1750,2000,6500)
BsmtUnfSF.b<-.bincode(BsmtUnfSF.n,UNFBSMT, right = TRUE,include.lowest = FALSE)
BsmtUnfSF.b[is.na(BsmtUnfSF.b)] <- 0
training_data$BsmtUnfSF <- BsmtUnfSF.b
ggplot(data = training_data, aes(x=BsmtUnfSF, y=SalePrice))+geom_point(stat ="identity")
sum(is.na(training_data$BsmtUnfSF))


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
TotalBsmtSF.n<-as.numeric(training_data$TotalBsmtSF)
TotalBsmtSF.n[is.na(TotalBsmtSF.n)] <- 0
sum(is.na(TotalBsmtSF.n))
TOTBSMT<-c(0,250,500,750,1000,1250, 1500, 1750,2000,2250,2500,2750,3000,7000)
TotalBsmtSF.b<-.bincode(TotalBsmtSF.n,TOTBSMT, right = TRUE,include.lowest = FALSE)
TotalBsmtSF.b[is.na(TotalBsmtSF.b)] <- 0
training_data$TotalBsmtSF <- TotalBsmtSF.b
ggplot(data = training_data, aes(x=TotalBsmtSF, y=SalePrice))+geom_point(stat ="identity")
sum(is.na(training_data$BsmtSF))



# Possible
#HeatingQC (Ordinal): Heating quality and condition
#5=Ex	Excellent
#4=Gd	Good
#3=TA	Average/Typical
#2=Fa	Fair
#1=Po	Poor
HeatingQC<-ifelse(training_data$HeatingQC=="Po",1, ifelse(training_data$HeatingQC=="Fa", 2,ifelse(training_data$HeatingQC=="TA", 3, 
                                                                                                  ifelse(training_data$HeatingQC=="Gd",4,ifelse(training_data$HeatingQC=="Ex",5,0)))))

ggplot(data = training_data, aes(x=TotalBsmtSF, y=SalePrice))+geom_point(stat ="identity")
sum(is.na(training_data$HeatingQC))

#Central Air (Nominal): Central air conditioning
#0=N	No
#1=Y	Yes
CentralAir<-ifelse(training_data$CentralAir=="Y",1, ifelse(training_data$CentralAir=="N", 0,0))
CentralAir [is.na(CentralAir)] <- 0
ggplot(data = training_data, aes(x=TotalBsmtSF, y=SalePrice))+geom_point(stat ="identity")
sum(is.na(training_data$CentralAir))

#Electrical (Ordinal): Electrical system
#5=SBrkr	Standard Circuit Breakers & Romex
#4=FuseA	Fuse Box over 60 AMP and all Romex wiring (Average)	
#3=FuseF	60 AMP Fuse Box and mostly Romex wiring (Fair)
#2=FuseP	60 AMP Fuse Box and mostly knob & tube wiring (poor)
#1=Mix	Mixed
#0=NA
Electrical<-ifelse(training_data$Electrical=="Mix",1, ifelse(training_data$Electrical=="FuseP", 2,ifelse(training_data$Electrical=="FuseF", 3, 
                                                                                                         ifelse(training_data$Electrical=="FuseA",4,ifelse(training_data$Electrical=="SBrkr",5,ifelse(training_data$Electrical=="NA's",0,0))))))


fct_explicit_na(training_data$Electrical, na_level = "NONE")->training_data$Electrical
fct_explicit_na(training_data$Exterior2nd, na_level = "NONE")->training_data$Exterior2nd
sum(is.na(training_data$Electrical))


ggplot(data = training_data, aes(x=training_data$Electrical, y=SalePrice, color= Electrical))+geom_point(stat ="identity")+coord_flip()


training_data%>%skim
#garage finish

fct_explicit_na(training_data$FireplaceQu, na_level = "NONE")->training_data$FireplaceQu
fct_explicit_na(training_data$GarageFinish, na_level = "NONE")->training_data$GarageFinish
fct_explicit_na(training_data$GarageQual, na_level = "NONE")->training_data$GarageQual


training_data %<>% mutate(logGrLivArea = log(GrLivArea))
training_data%>%skim
training_data$MasVnrType %na<-% "None"

training_data %<>% mutate(totalSF = training_data$`1stFlrSF` + training_data$`2ndFlrSF` + TotalBsmtSF)
training_data %<>% mutate(MultipleFloor = if_else(training_data$`2ndFlrSF` >0,1,0))

full_training <-training_data

full_training%>%skim
table(full_training$EnclosedPorch_group)
