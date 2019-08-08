
install.packages(c("skimr","tangram","fastDummies",'olsrr','rlang','roperators'))
library(tidyverse) #general data wrangling tools
library(skimr) #summary stats
library(tangram) #has is.categorical() function, useful for creating tables
library(car) # Regression tools
library(fastDummies) # creates dummy variables
library(rlang)
library(olsrr)
library(roperators) #used to convert NAs to to different values

#create training data object, please refer to this variable when making modificaitons to the dataset----
#'if reading from local source
#'library(readr)
train <- read.csv("//DS1513/AllData/Adam/SMU Data Science Courses/DS 6371 Stats/Project/train.csv")
#'View(train)

training_data = train


#Jeff's Data----

training_data$Fence %na<-% "None" #roperators package

training_data = training_data %>% 
  mutate(logSalePrice = log(SalePrice)) %>%
  mutate(GarageCars_f = as.factor(training_data$GarageCars)) %>%
  mutate(pool_yn = if_else(PoolArea == 0,0,1)) %>% #not good predictor, 7 cases with data
  mutate(porch_yn = if_else(("3SsnPorch" == 0 || EnclosedPorch == 0 || OpenPorchSF == 0 || ScreenPorch == 0),0,1)) %>% #if a home has any features related to a porch then porch_yn = 1
  mutate(Fence_f = factor(Fence, levels=c("None", "GdPrv", "GdWo","MnPrv","MnWw")))

#WoodDeckSF Categorical Buckets from Quantitiative Data
group_WoodDeckSF <- function(WoodDeckSF){
  if (WoodDeckSF >= 0 & WoodDeckSF <= 100){
    return('0-100SF')
  }else if(WoodDeckSF > 100 & WoodDeckSF <= 200){
    return('100-200SF')
  }else if (WoodDeckSF > 200 & WoodDeckSF <= 300){
    return('200-300SF')
  }else if (WoodDeckSF > 300 & WoodDeckSF <=400){
    return('300-400SF')
  }else if (WoodDeckSF > 400){
    return('> 400SF')
  }
}
training_data$WoodDeckSF_group <- sapply(training_data$WoodDeckSF,group_WoodDeckSF)
training_data$WoodDeckSF_group <- factor(training_data$WoodDeckSF_group, levels = c("0-100SF","100-200SF","200-300SF","300-400SF","> 400SF"), ordered = TRUE)

ggplot(data = training_data, aes(x=WoodDeckSF_group, y=logSalePrice))+geom_point(stat ="identity")


#OpenPorchSF Categorical Buckets from Quantitiative Data
group_OpenPorchSF <- function(OpenPorchSF){
  if (OpenPorchSF >= 0 & OpenPorchSF <= 50){
    return('0-50SF')
  }else if(OpenPorchSF > 50 & OpenPorchSF <= 100){
    return('50-100SF')
  }else if (OpenPorchSF > 100 & OpenPorchSF <= 150){
    return('100-150SF')
  }else if (OpenPorchSF > 150 & OpenPorchSF <=200){
    return('150-200SF')
  }else if (OpenPorchSF > 200){
    return('> 200SF')
  }
}
training_data$OpenPorchSF_group <- sapply(training_data$OpenPorchSF,group_OpenPorchSF)
training_data$OpenPorchSF_group <- factor(training_data$OpenPorchSF_group,levels = c("0-50SF","50-100SF","100-150SF","150-200SF","> 200SF" ), ordered = TRUE)

ggplot(data = training_data, aes(x=OpenPorchSF_group, y=logSalePrice))+geom_point(stat ="identity")


#EnclosedPorch Categorical Buckets from Quantitiative Data
group_EnclosedPorch<- function(EnclosedPorch){
  if (EnclosedPorch >= 0 & EnclosedPorch <= 50){
    return('0-50SF')
  }else if(EnclosedPorch > 50 & EnclosedPorch <= 100){
    return('50-100SF')
  }else if (EnclosedPorch > 100 & EnclosedPorch <= 150){
    return('100-150SF')
  }else if (EnclosedPorch > 150 & EnclosedPorch <=200){
    return('150-200SF')
  }else if (EnclosedPorch > 200){
    return('> 200SF')
  }
}
training_data$EnclosedPorch_group <- sapply(training_data$EnclosedPorch,group_EnclosedPorch)
training_data$EnclosedPorch_group <- factor(training_data$EnclosedPorch_group, levels = c("0-50SF","50-100SF","100-150SF","150-200SF","> 200SF"), ordered = TRUE)
levels(training_data$EnclosedPorch_group)
ggplot(data = training_data, aes(x=EnclosedPorch_group, y=logSalePrice))+geom_point(stat ="identity")

#24 Observations of 3SsnPorch, not good predictor

#MiscVal Categorical Buckets from Quantitiative Data
group_MiscVal <- function(MiscVal){
  if (MiscVal >= 0 & MiscVal <= 200){
    return('$0-$200')
  }else if(MiscVal > 200 & MiscVal <= 400){
    return('$200-$400')
  }else if (MiscVal > 400 & MiscVal <= 600){
    return('$400-$600')
  }else if (MiscVal > 600 & MiscVal <=800){
    return('$600-$800')
  }else if (MiscVal > 800){
    return('> $800')
  }
}
training_data$MiscVal_group <- sapply(training_data$MiscVal,group_MiscVal)
training_data$MiscVal_group <- factor(training_data$MiscVal_group, levels = c("$0-$200","$200-$400","$400-$600","$600-$800","> $800"), ordered = TRUE)
levels(training_data$MiscVal_group)
ggplot(data = training_data, aes(x=MiscVal_group, y=logSalePrice))+geom_point(stat ="identity")

#Convert NA Fence to "None"
#convert MiscVale to categorical

#'pool not good predictor, 7 cases with data
#'MiscFeature not good predictor
#'Interaction variables for porch
#' porch_yn*3SsnPorch
#' porch_yn*EnclosedPorch
#' porch_yn*OpenPorchSF
#' porch_yn*ScreenPorch



#Adam's Data----
# X1stFlrSF NO
ggplot(data = training_data, aes(x=X1stFlrSF, y=SalePrice))+geom_point(stat ="identity")

# X2stFlrSF NO
ggplot(data = training_data, aes(x=X2ndFlrSF, y=SalePrice))+geom_point(stat ="identity")

# X2ndFlrSF/X1stFlrSF NO
ggplot(data = training_data, aes(x=X2ndFlrSF/X1stFlrSF, y=SalePrice))+geom_point(stat ="identity")

# NO EFFECT Basement Bath BsmtFullBath + (0.5*BsmtHalfBath)
ggplot(data = training_data, aes(x=BsmtFullBath + (0.5*BsmtHalfBath), y=SalePrice))+geom_point(stat ="identity")

# USE THIS ONE !! Bath FullBath + (0.5 * HalfBath) RECODE to THREE PLUS
training_data$TotalBath <- training_data$FullBath + (0.5 * training_data$HalfBath)
ggplot(data = training_data, aes(x=TotalBath, y=SalePrice))+geom_point(stat ="identity")
ggplot(data = training_data, aes(x=TotalBath, y=logSalePrice))+geom_point(stat ="identity")


# BedroomAbvGr 4+ bedrooms
ggplot(data = training_data, aes(x=BedroomAbvGr, y=SalePrice))+geom_point(stat ="identity")
training_data$BedroomAbvGr4plus <- ifelse(training_data$BedroomAbvGr >= 4, 4, training_data$BedroomAbvGr)  
ggplot(data = training_data, aes(x=BedroomAbvGr4plus, y=SalePrice))+geom_point(stat ="identity")
ggplot(data = training_data, aes(x=BedroomAbvGr4plus, y=logSalePrice))+geom_point(stat ="identity")


#NO EFFECT KitchenAbvGr
ggplot(data = training_data, aes(x=KitchenAbvGr, y=SalePrice))+geom_point(stat ="identity")

training_data$KitchenQual

Qual4 <- c("Fa", "TA", "Gd", "Ex") # no "Po" data
training_data$KitchenQual.Fac <- factor(x=training_data$KitchenQual, levels=Qual4, ordered = TRUE)
#levels(Qual.Fac)
training_data$KitchenQual.FacNum <- as.numeric(training_data$KitchenQual.Fac)

# POSSIBLE LOG KitchenQual
ggplot(data = training_data, aes(x=training_data$KitchenQual.FacNum, y=SalePrice))+geom_point(stat ="identity")
ggplot(data = training_data, aes(x=training_data$KitchenQual.FacNum, y=logSalePrice))+geom_point(stat ="identity")


#POSSIBLE TotRmsAbvGrd MAY NEED TO RECODE FOR 10+
ggplot(data = training_data, aes(x=TotRmsAbvGrd, y=SalePrice))+geom_point(stat ="identity")
training_data$TotRmsAbvGrd10plus <- ifelse(training_data$TotRmsAbvGrd >= 10, 10, training_data$TotRmsAbvGrd)  
ggplot(data = training_data, aes(x=TotRmsAbvGrd10plus, y=logSalePrice))+geom_point(stat ="identity")

# DO NOT USE!! Functional  Levels: Maj1 Maj2 Min1 Min2 Mod Sev Typ
training_data$Functional
ggplot(data = training_data, aes(x=Functional, y=SalePrice))+geom_point(stat ="identity")


# Fireplaces
ggplot(data = training_data, aes(x=Fireplaces, y=SalePrice))+geom_point(stat ="identity")

training_data$FireplaceY <- ifelse(training_data$Fireplaces == 0, 0, 1)  
ggplot(data = training_data, aes(x=FireplaceY, y=SalePrice))+geom_point(stat ="identity")


# GarageType different values "2Types Attchd Basment BuiltIn CarPort Detchd"
training_data$GarageType
ggplot(data = training_data, aes(x=GarageType, y=SalePrice))+geom_point(stat ="identity")
training_data$GarageTypeY <- ifelse(training_data$GarageType == 'Attchd' | training_data$GarageType == 'BuiltIn', 1, 0)  
ggplot(data = training_data, aes(x=GarageTypeY, y=logSalePrice))+geom_point(stat ="identity")

# GarageCars 
ggplot(data = training_data, aes(x=GarageCars, y=SalePrice))+geom_point(stat ="identity")

# GarageCars recode Significant 0, 1, 2, 3+ 
GarCar <- c(0, 1, 2, 3, 4) 
training_data$GarageCars.Fac <- factor(x=training_data$GarageCars, levels=GarCar, ordered = TRUE)

training_data$GarageCars3plus <- ifelse(training_data$GarageCars.Fac == 4, 3, training_data$GarageCars.Fac)  

ggplot(data = training_data, aes(x=as.numeric(training_data$GarageCars3plus), y=SalePrice))+geom_point(stat ="identity")
ggplot(data = training_data, aes(x=as.numeric(training_data$GarageCars3plus), y=logSalePrice))+geom_point(stat ="identity")


# GarageArea not as useful
ggplot(data = training_data, aes(x=GarageArea, y=SalePrice))+geom_point(stat ="identity")

training_data$GarageCond

Qual5 <- c("Po", "Fa", "TA", "Gd", "Ex") 
training_data$GarageCond.Fac <- factor(x=training_data$GarageCond, levels=Qual5, ordered = TRUE)
#levels(Qual.Fac)
as.numeric(training_data$GarageCond.Fac)

# Not useable GarageCond.Fac
ggplot(data = training_data, aes(x=as.numeric(training_data$GarageCond.Fac), y=SalePrice))+geom_point(stat ="identity")


# PavedDrive
training_data$PavedDrive
Pave <- c("N", "P", "Y") 
training_data$PavedDrive.Fac <- factor(x=training_data$PavedDrive, levels=Pave, ordered = TRUE)
ggplot(data = training_data, aes(x=as.numeric(training_data$PavedDrive.Fac), y=SalePrice))+geom_point(stat ="identity")

# PavedDriveY POSSIBLE
training_data$PavedDriveY <- ifelse(training_data$PavedDrive == "Y", 1, 0)  
ggplot(data = training_data, aes(x=PavedDriveY, y=SalePrice))+geom_point(stat ="identity")
ggplot(data = training_data, aes(x=PavedDriveY, y=logSalePrice))+geom_point(stat ="identity")


# Fireplaces FireplaceQu GarageType GarageYrBlt GarageFinish
# GarageCars GarageArea GarageQual GarageCond PavedDrive KitchenQual.Fac



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
training_data$MSSubClass_group <- sapply(training_data$MSSubClass,group_MSSubClass)
training_data$MSSubClass_group <- factor(training_data$MSSubClass_group, levels = c('0-30','30-60','60-90','90-110','>110'), ordered = TRUE)
levels(training_data$MSSubClass_group)

#MSZoning
#1=C (all)
#2=FV
#3=RH
#4=RL
#5=RM
#MSZoningFactor%>% skim
MSZoning.n<-as.numeric(training_data$MSZoning)
ggplot(data =training_data, aes(x=training_data$MSZoning, y=SalePrice, color= MSZoning))+geom_point(stat ="identity")
ggplot(data = training_data, aes(x=MSZoning.n, y=SalePrice, color= MSZoning))+geom_point(stat ="identity")


#LotFrontage Categorical Buckets from Quantitiative Data
training_data$LotFrontage %na<-% 0
group_LotFrontage <- function(LotFrontage){
  if (LotFrontage >= 0 & LotFrontage <= 50){
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
training_data$LotFrontage_group <- sapply(training_data$LotFrontage,group_LotFrontage)
training_data$LotFrontage_group <- factor(training_data$LotFrontage_group, levels = c('0-50','50-80','80-110','110-140','>140'), ordered = TRUE)
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
training_data$LotArea_group <- factor(training_data$LotArea_group, levels = c('0-3000','3000-6000','6000-9000','>9000'), ordered = TRUE)
ggplot(data = training_data, aes(x=training_data$LotArea_group, y=SalePrice, color= LotArea_group))+geom_point(stat ="identity")


#LotShape
#1=Reg
#2=IR1
#3=IR2
#4=IR3
LotShape.n<-as.numeric(training_data$LotShape)           
ggplot(data = training_data, aes(x=training_data$LotShap, y=SalePrice, color= LotShape))+geom_point(stat ="identity")
ggplot(data = training_data, aes(x=LotShape.n, y=SalePrice, color= LotShape))+geom_point(stat ="identity")


#LandContour
#1=Bnk
#2=HLS
#3=Low
#4=Lvl
LandContour.n<-as.numeric(training_data$LandContour)
ggplot(data = training_data, aes(x=training_data$LandContour, y=SalePrice, color= LandContour))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=LandContour.n, y=SalePrice, color= LandContour))+geom_point(stat ="identity")


#LotConfig
#1=Corner
#2=CulDSac
#3=FR2
#4=FR3
#5=Inside
LotConfig.n<-as.numeric(training_data$LotConfig)
ggplot(data =training_data, aes(x=training_data$LotConfig, y=SalePrice, color= LotConfig))+geom_point(stat ="identity")
ggplot(data = training_data, aes(x=LotConfig.n, y=SalePrice, color= LotConfig))+geom_point(stat ="identity")


#LandSlope
#1=Gtl
#2=Mod
#3=Sev
LandSlope.n<-as.numeric(training_data$LandSlope)
ggplot(data =training_data, aes(x=training_data$LandSlope, y=SalePrice, color= LandSlope))+geom_point(stat ="identity")
ggplot(data = training_data, aes(x=LandSlope.n, y=SalePrice, color= LandSlope))+geom_point(stat ="identity")


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
Neighborhood.n<-as.numeric(training_data$Neighborhood)
ggplot(data = training_data, aes(x=Neighborhood.n, y=SalePrice, color= Neighborhood))+geom_point(stat ="identity")+coord_flip()


#Condition1
#1=Artery
#2=Feedr
#3=Norm
#4=PosA
#5=PosN
#6=RRAe
#7=RRAn
#8=RRNe
#9=RRNn
Condition1.n<-as.numeric(training_data$Condition1)
ggplot(data = training_data, aes(x=Condition1.n, y=SalePrice, color= Condition1))+geom_point(stat ="identity")+coord_flip()



#BldgType
#1=1Fam
#2=2fmCon
#3=Duplex
#4=Twhs
#5=TwnhE
BldgType.n<-as.numeric(training_data$BldgType)
ggplot(data =training_data, aes(x=training_data$BldgType, y=SalePrice, color= BldgType))+geom_point(stat ="identity")
ggplot(data = training_data, aes(x=BldgType.n, y=SalePrice, color= BldgType))+geom_point(stat ="identity")


#HouseStyle
#1=1.5Fin
#2=1.5Unf
#3=1Story
#4=2.5Fin
#5=2.5Unf
#6=2Story
#7=SFoyer
#8=SLvl
HouseStyle.n<-as.numeric(training_data$HouseStyle)
ggplot(data =training_data, aes(x=training_data$HouseStyle, y=SalePrice, color= HouseStyle))+geom_point(stat ="identity")
ggplot(data = training_data, aes(x=HouseStyle.n, y=SalePrice, color= HouseStyle))+geom_point(stat ="identity")


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
OverallQual.n<-as.numeric(training_data$OverallQual)
ggplot(data = training_data, aes(x=OverallQual.n, y=SalePrice, color= OverallQual))+geom_point(stat ="identity")


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
OverallCond.n<-as.numeric(training_data$OverallCond)
ggplot(data = training_data, aes(x=OverallCond.n, y=SalePrice, color= OverallCond))+geom_point(stat ="identity")


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
ybbin<-c(1900,1930,1960,1990,2011)
YearBuilt.b <-.bincode(YearBuilt.n,ybbin, right = TRUE,include.lowest = FALSE)
YearBuilt.b  [is.na(YearBuilt.b )] <- 0
ggplot(data = training_data, aes(x=training_data$YearBuilt, y=SalePrice, color= YearBuilt))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=YearBuilt.b , y=SalePrice, color= YearBuilt))+geom_point(stat ="identity")


##Binned variable
#YearRemodAdd.b 
#1=1950-1960
#2=1961-1970
#3=1971-1980
#4=1981-1990
#5=1991-2000
#6=2001+
YearRemodAdd.n<-as.numeric(training_data$YearRemodAdd)
YearRemodAdd.n [is.na(YearRemodAdd.n)] <- 0
sum(is.na(YearRemodAdd.n))
yrabin<-c(1960,1970,1980,1990,2000,2012)
YearRemodAdd.b <-.bincode(YearRemodAdd.n,yrabin, right = TRUE,include.lowest = FALSE)
YearRemodAdd.b  [is.na(YearRemodAdd.b )] <- 0
ggplot(data = training_data, aes(x=training_data$YearRemodAdd, y=SalePrice, color= YearRemodAdd))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=YearRemodAdd.b , y=SalePrice, color= YearRemodAdd))+geom_point(stat ="identity")




#David's Data----

#below is all the code to set levels and bin the variables

#1=Flat
#2=Gable
#3=Gambrel
#4=Hip
#RoofStyleFactor%>% skim
RoofStyle.n<-as.numeric(training_data$RoofStyle)
ggplot(data = training_data, aes(x=training_data$RoofStyle, y=SalePrice, color= RoofStyle))+geom_point(stat ="identity")
ggplot(data = training_data, aes(x=RoofStyle.n, y=logSalePrice, color= RoofStyle))+geom_point(stat ="identity")

training_data$RoofStyleY <- ifelse(training_data$RoofStyle == 'Gable' | training_data$RoofStyle == 'Hip', 1, 0)  
ggplot(data = training_data, aes(x=RoofStyleY, y=logSalePrice, color= RoofStyle))+geom_point(stat ="identity")

#1=ClyTile
#2=CompShg
#3=Membran
#4=Metal
#5=Roll
#6=Tar&Grv
#7=Wdshake
#8=WdShngl
RoofMatl.n<-as.numeric(training_data$RoofMatl)           
ggplot(data = training_data, aes(x=training_data$RoofMatl, y=SalePrice, color= RoofMatl))+geom_point(stat ="identity")


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
Exterior1st.n<-as.numeric(training_data$Exterior1st)
ggplot(data = training_data, aes(x=training_data$Exterior1st, y=SalePrice, color= Exterior1st))+geom_point(stat ="identity")+coord_flip()


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
Exterior2nd.n<-as.numeric(training_data$Exterior2nd)
ggplot(data = training_data, aes(x=Exterior2nd.n, y=SalePrice, color= Exterior2nd))+geom_point(stat ="identity")+coord_flip()


#0=N/A
#1=BrkCmn  
#2=BrkFace
#3=None
#4=Stone
MasVnrType.n<-as.numeric(training_data$MasVnrType)
MasVnrType.n[is.na(MasVnrType.n)] <- 0
sum(is.na(MasVnrType.n))
#Masonry veneer type
ggplot(data = training_data, aes(x=MasVnrType.n, y=SalePrice, color= MasVnrType))+geom_point(stat ="identity")+coord_flip()


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
ggplot(data = training_data, aes(x=training_data$MasVnrArea, y=SalePrice, color= MasVnrArea))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=MasVnrArea.b, y=SalePrice, color= MasVnrType))+geom_point(stat ="identity")+coord_flip()

#USE THIS ONE
#Exter Qual (Ordinal): Evaluates the quality of the material on the exterior 
#4=Ex	Excellent
#3=Gd	Good
#2=TA	Average/Typical
#1=Fa	Fair
EXTQ2<-ifelse(training_data$ExterQual=="Fa",1, ifelse(training_data$ExterQual=="TA", 2,ifelse(training_data$ExterQual=="Gd", 3, ifelse(training_data$ExterQual=="Ex",4,0))))
ExterQual.n<-EXTQ2
ggplot(data = training_data, aes(x=EXTQ2, y=SalePrice, color= ExterQual))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=ExterQual.n, y=SalePrice, color= ExterQual))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=training_data$ExterQual, y=SalePrice, color= ExterQual))+geom_point(stat ="identity")+coord_flip()


#Exter Cond (Ordinal): Evaluates the present condition of the material on the exterior
#5=Ex	Excellent
#4=Gd	Good
#3=TA	Average/Typical
#2=Fa	Fair
#1=Po	Poor
ExterCond.n<-ifelse(training_data$ExterCond=="Po",1, ifelse(training_data$ExterCond=="Fa", 2,ifelse(training_data$ExterCond=="TA", 3, 
                                                                                      ifelse(training_data$ExterCond=="Gd",4,ifelse(training_data$ExterCond=="Ex",5,0)))))
ggplot(data = training_data, aes(x=ExterCond.n, y=SalePrice, color= ExterCond))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=training_data$ExterCond, y=SalePrice, color= ExterCond))+geom_point(stat ="identity")+coord_flip()


#Foundation.n
#1=BrkTil 
#2=CBlock
#3=PConc
#4=Slab
#5=Stone
#6=Wood
Foundation.n<-as.numeric(training_data$Foundation)
ggplot(data = training_data, aes(x=Foundation.n, y=SalePrice, color= Foundation))+geom_point(stat ="identity")+coord_flip()

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
ggplot(data = training_data, aes(x=training_data$BsmtQual, y=SalePrice, color= BsmtQual))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=BsmtQual.n, y=SalePrice, color= BsmtQual))+geom_point(stat ="identity")+coord_flip()


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
ggplot(data = training_data, aes(x=training_data$BsmtCond, y=SalePrice, color= BsmtCond))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=BsmtCond.n, y=SalePrice, color= BsmtCond))+geom_point(stat ="identity")+coord_flip()


#Bsmt Exposure	(Ordinal): Refers to walkout or garden level walls
#4=Gd	Good Exposure
#3=Av	Average Exposure (split levels or foyers typically score average or above)	
#2=Mn	Mimimum Exposure
#1=No	No Exposure
#0=NA	No Basement
BsmtExposure.n<-ifelse(training_data$BsmtExposure=="No",1, ifelse(training_data$BsmtExposure=="Mn", 2,ifelse(training_data$BsmtExposure=="Av", 3, 
                                                                                               ifelse(training_data$BsmtExposure=="Gd",4,ifelse(training_data$BsmtCond=="NA",0,0)))))
BsmtExposure.n [is.na(BsmtExposure.n)] <- 0
ggplot(data = training_data, aes(x=training_data$BsmtExposure, y=SalePrice, color= BsmtExposure))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=BsmtExposure.n, y=SalePrice, color= BsmtExposure))+geom_point(stat ="identity")+coord_flip()



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
ggplot(data = training_data, aes(x=training_data$BsmtFinType1, y=SalePrice, color= BsmtFinType1))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=BsmtFinType1.n, y=SalePrice, color= BsmtFinType1))+geom_point(stat ="identity")+coord_flip()


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
ggplot(data = training_data, aes(x=training_data$BsmtFinSF1, y=SalePrice, color= BsmtFinType1))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=BsmtFinSF1.b, y=SalePrice, color= training_data$BsmtFinSF1))+geom_point(stat ="identity")+coord_flip()


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
ggplot(data = training_data, aes(x=training_data$BsmtFinType2, y=SalePrice, color= BsmtFinType2))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(BsmtFinType2.n, y=SalePrice, color= BsmtFinType2))+geom_point(stat ="identity")+coord_flip()

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
ggplot(data = training_data, aes(x=training_data$BsmtFinSF2, y=SalePrice, color= BsmtFinType2))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(BsmtFinSF2.b, y=SalePrice, color= BsmtFinType2))+geom_point(stat ="identity")+coord_flip()


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
ggplot(data = training_data, aes(x=training_data$BsmtUnfSF, y=SalePrice, color= BsmtUnfSF))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=BsmtUnfSF.b, y=SalePrice, color= BsmtUnfSF))+geom_point(stat ="identity")+coord_flip()


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
ggplot(data = training_data, aes(x=training_data$TotalBsmtSF, y=SalePrice, color= TotalBsmtSF))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(TotalBsmtSF.b, y=SalePrice, color= TotalBsmtSF))+geom_point(stat ="identity")+coord_flip()


#Heating	(Nominal): Type of heating
#1=Floor	Floor Furnace
#2=GasA	Gas forced warm air furnace
#3=GasW	Gas hot water or steam heat
#4=Grav	Gravity furnace	
#5=OthW	Hot water or steam heat other than gas
#6=Wall	Wall furnace
Heating.n<-as.numeric(training_data$Heating)
ggplot(data = training_data, aes(x=training_data$Heating, y=SalePrice, color= Heating))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(Heating.n, y=SalePrice, color= Heating))+geom_point(stat ="identity")+coord_flip()


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
HeatingQC.n
ggplot(data = training_data, aes(x=training_data$HeatingQC, y=SalePrice, color= HeatingQC))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=HeatingQC.n, y=SalePrice, color= HeatingQC))+geom_point(stat ="identity")+coord_flip()


#Central Air (Nominal): Central air conditioning
#0=N	No
#1=Y	Yes
CentralAir.n<-ifelse(training_data$CentralAir=="Y",1, ifelse(training_data$CentralAir=="N", 0,0))
CentralAir.n [is.na(CentralAir.n)] <- 0
ggplot(data = training_data, aes(x=training_data$CentralAir, y=SalePrice, color= CentralAir))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=CentralAir.n, y=SalePrice, color= CentralAir))+geom_point(stat ="identity")+coord_flip()



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
ggplot(data = training_data, aes(x=training_data$Electrical, y=SalePrice, color= Electrical))+geom_point(stat ="identity")+coord_flip()
ggplot(data = training_data, aes(x=Electrical.n, y=SalePrice, color= Electrical))+geom_point(stat ="identity")+coord_flip()

#lets talk about this 
corrgram(training_data.n, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

training_data$logGrLivArea <- log(training_data$GrLivArea)

# 0.8376
# test_sale_price<-lm( data = training_data, logSalePrice~logGrLivArea + TotalBath + BedroomAbvGr4plus + KitchenQual.FacNum + TotRmsAbvGrd10plus +
#                             FireplaceY + GarageTypeY + GarageCars3plus + PavedDriveY + RoofStyleY + MasVnrArea.b + ExterQual +
#                             ExterCond + BsmtQual.n + BsmtCond.n + BsmtExposure.n + BsmtFinType1.n + BsmtFinSF1.b + BsmtUnfSF.b +
#                             TotalBsmtSF.b + HeatingQC.n + CentralAir.n + Electrical.n)

# 0.8401
# test_sale_price<-lm( data = training_data, logSalePrice~logGrLivArea + TotalBath + KitchenQual.FacNum + TotRmsAbvGrd10plus +
#                        FireplaceY + GarageTypeY + GarageCars3plus + PavedDriveY + ExterQual +
#                        ExterCond + BsmtQual.n + BsmtExposure.n + BsmtFinType1.n + BsmtFinSF1.b +
#                        TotalBsmtSF.b + HeatingQC.n + CentralAir.n + WoodDeckSF_group + OpenPorchSF_group + EnclosedPorch_group)

#0.8744
test_sale_price<-lm( data = training_data, logSalePrice~logGrLivArea + TotalBath + KitchenQual.FacNum + TotRmsAbvGrd10plus +
                       FireplaceY + GarageTypeY + GarageCars3plus + PavedDriveY + ExterQual +
                       ExterCond + BsmtQual.n + BsmtExposure.n + BsmtFinType1.n + BsmtFinSF1.b +
                       TotalBsmtSF.b + HeatingQC.n + CentralAir.n + WoodDeckSF_group + OpenPorchSF_group + EnclosedPorch_group +
                       LandSlope.n + OverallQual.n + OverallCond.n + YearBuilt.b +logGrLivArea + LotArea + LotFrontage)
#+    ) # Skipped not working LotFrontage_group + LotArea_group

summary(test_sale_price)

