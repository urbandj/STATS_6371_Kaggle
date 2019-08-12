train<- train
test<- test

test[["SalePrice"]]<-0
training_data<-rbind(train,test)
skim(test)
#Jeff's Data----
drop<- c("PoolQC","MiscFeature", "LotShape")
training_data = training_data[,!(names(training_data) %in% drop)]
names(training_data)

training_data = training_data %>% 
  mutate(pool_yn = if_else(PoolArea == 0,0,1)) %>% #not good predictor, 7 cases with data
  mutate(porch_yn = if_else((X3SsnPorch == 0 || EnclosedPorch == 0 || OpenPorchSF == 0 || ScreenPorch == 0),0,1)) %>%
  mutate(porchArea = X3SsnPorch + EnclosedPorch + OpenPorchSF + ScreenPorch) %>%
  mutate(X3SsnPorch_y = if_else(X3SsnPorch > 0,1,0)) %>%
  mutate(EnclosedPorch_y = if_else(EnclosedPorch > 0,1,0)) %>%
  mutate(OpenPorchSF_y = if_else(EnclosedPorch > 0,1,0)) %>%
  mutate(ScreenPorch_y = if_else(ScreenPorch > 0,1,0))#if a home has any features related to a porch then porch_yn = 1

training_data$Fence = factor(training_data$Fence, levels=c("None", "GdPrv", "GdWo","MnPrv","MnWw"), ordered = FALSE)
training_data$Fence %na<-% "None" #roperators package
sum(is.na(training_data$Fence))

#WoodDeckSF Categorical Buckets from Quantitiative Data
training_data$WoodDeckSF %na<-% 0
training_data$WoodDeckSF_group<-cut(training_data$WoodDeckSF, c(0,100,200,300,400,900), ordered_result = FALSE, include.lowest = TRUE)


#OpenPorchSF Categorical Buckets from Quantitiative Data
training_data$OpenPorchSF %na<-% 0
training_data$OpenPorchSF_group<-cut(training_data$OpenPorchSF, c(0,50,100,150,200,550), ordered_result = FALSE, include.lowest = TRUE)

#EnclosedPorch Categorical Buckets from Quantitiative Data
training_data$EnclosedPorch %na<-% 0
training_data$EnclosedPorch_group<-cut(training_data$EnclosedPorch, c(0,50,100,150,200,600), ordered_result = FALSE, include.lowest = TRUE)

#Adam's Data----

# USE THIS ONE !! Bath FullBath + (0.5 * HalfBath) RECODE to THREE PLUS
training_data$TotalBath <- training_data$FullBath + (0.5 * training_data$HalfBath)

sum(is.na(training_data$TotalBath))

# BedroomAbvGr 4+ bedrooms
training_data$BedroomAbvGr4plus <- ifelse(training_data$BedroomAbvGr >= 4, 4, training_data$BedroomAbvGr)  
sum(is.na(training_data$BedroomAbvGr4plus))

drop<- c("BedroomAbvGr")
training_data = training_data[,!(names(training_data) %in% drop)]

Qual4 <- c("Fa", "TA", "Gd", "Ex") # no "Po" data
training_data$KitchenQual <- factor(x=training_data$KitchenQual, levels=Qual4, ordered = FALSE)
sum(is.na(training_data$KitchenQual))

#POSSIBLE TotRmsAbvGrd MAY NEED TO RECODE FOR 10+
training_data$TotRmsAbvGrd10plus <- ifelse(training_data$TotRmsAbvGrd >= 10, 10, training_data$TotRmsAbvGrd) 

drop<- c("TotRmsAbvGr")
training_data = training_data[,!(names(training_data) %in% drop)]

# Fireplaces
training_data$FireplaceY <- ifelse(training_data$Fireplaces == 0, 0, 1)  
#ggplot(data = training_data, aes(x=FireplaceY, y=logSalePrice))+geom_point(stat ="identity")

drop<- c("Fireplace")
training_data = training_data[,!(names(training_data) %in% drop)]


# GarageType different values "2Types Attchd Basment BuiltIn CarPort Detchd"
training_data$GarageType %na<-% "Detchd"

training_data$GarageTypeY <- ifelse(training_data$GarageType == 'Attchd' | training_data$GarageType == 'BuiltIn', 1, 0)  

##### strange may want check buckets
#Garage Year Built
training_data$GarageYrBlt %na<-% 0

# GarageCars recode Significant 0, 1, 2, 3+ 
GarCar <- c(0, 1, 2, 3, 4) 
training_data$GarageCars<- factor(x=training_data$GarageCars, levels=GarCar, ordered = FALSE)
#training_data$GarageCars3plus <- ifelse(training_data$GarageCars == 4, 3, training_data$GarageCars) 

sum(is.na(training_data$GarageCars))

# GarageArea not as useful
training_data$GarageCond %na<-% 0

Qual5 <- c("Po", "Fa", "TA", "Gd", "Ex") 
training_data$GarageCond <- factor(x=training_data$GarageCond, levels=Qual5, ordered = FALSE)
training_data$GarageCond <- as.numeric(training_data$GarageCond)
training_data$GarageCond %na<-% 0

# PavedDrive
Pave <- c("N", "P", "Y") 
training_data$PavedDrive <- factor(x=training_data$PavedDrive, levels=Pave, ordered = FALSE)

# PavedDriveY POSSIBLE
#training_data$PavedDriveY <- ifelse(training_data$PavedDrive == "Y", 1, 0)  


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

#MSZoning
#1=C (all)
#2=FV
#3=RH
#4=RL
#5=RM
#MSZoningFactor%>% skim
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
sum(is.na(training_data$LotArea))


#LandSlope
#1=Gtl
#2=Mod
#3=Sev
ordered(training_data$LandSlope)
#ggplot(data = training_data, aes(x=training_data$LandSlope, y=SalePrice))+geom_point(stat ="identity")

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

##Binned variable
#YearRemodAdd.b 
#1=1950-1960
#2=1961-1970
#3=1971-1980
#4=1981-1990
#5=1991-2000
#6=2001+
training_data$YearRemodAdd[is.na(training_data$YearRemodAdd)] <- "NONE"


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


#Exter Cond (Ordinal): Evaluates the present condition of the material on the exterior
#5=Ex	Excellent
#4=Gd	Good
#3=TA	Average/Typical
#2=Fa	Fair
#1=Po	Poor
ExterCond<-ifelse(training_data$ExterCond=="Po",1, ifelse(training_data$ExterCond=="Fa", 2,ifelse(training_data$ExterCond=="TA", 3, 
                                                                                                  ifelse(training_data$ExterCond=="Gd",4,ifelse(training_data$ExterCond=="Ex",5,0)))))

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
sum(is.na(training_data$TotalBsmtSF))

# Possible
#HeatingQC (Ordinal): Heating quality and condition
#5=Ex	Excellent
#4=Gd	Good
#3=TA	Average/Typical
#2=Fa	Fair
#1=Po	Poor
HeatingQC<-ifelse(training_data$HeatingQC=="Po",1, ifelse(training_data$HeatingQC=="Fa", 2,ifelse(training_data$HeatingQC=="TA", 3, 
                                                                                                  ifelse(training_data$HeatingQC=="Gd",4,ifelse(training_data$HeatingQC=="Ex",5,0)))))

sum(is.na(training_data$HeatingQC))

#Central Air (Nominal): Central air conditioning
#0=N	No
#1=Y	Yes
CentralAir<-ifelse(training_data$CentralAir=="Y",1, ifelse(training_data$CentralAir=="N", 0,0))
CentralAir [is.na(CentralAir)] <- 0
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

#garage finish

fct_explicit_na(training_data$FireplaceQu, na_level = "NONE")->training_data$FireplaceQu
fct_explicit_na(training_data$GarageFinish, na_level = "NONE")->training_data$GarageFinish
fct_explicit_na(training_data$GarageQual, na_level = "NONE")->training_data$GarageQual
fct_explicit_na(training_data$EnclosedPorch_group, na_level = "0")->training_data$EnclosedPorch_group
fct_explicit_na(training_data$Exterior1st, na_level = "NONE")->training_data$Exterior1st
fct_explicit_na(training_data$KitchenQual, na_level = "NONE")->training_data$KitchenQual
fct_explicit_na(training_data$Functional, na_level = "NONE")->training_data$Functional
fct_explicit_na(training_data$MSZoning, na_level = "NONE")->training_data$MSZoning
fct_explicit_na(training_data$OpenPorchSF_group, na_level = "NONE")->training_data$OpenPorchSF_group
fct_explicit_na(training_data$SaleType, na_level = "NONE")->training_data$SaleType
fct_explicit_na(training_data$Functional, na_level = "NONE")->training_data$Functional
fct_explicit_na(training_data$Utilities, na_level = "NONE")->training_data$Utilities
fct_explicit_na(training_data$WoodDeckSF_group, na_level = "NONE")->training_data$WoodDeckSF_group
training_data$WoodDeckSF_group[is.na(training_data$WoodDeckSF_group)] <- "NONE"
training_data$GarageArea [is.na(training_data$GarageArea)] <- 0
training_data$GarageCars [is.na(training_data$GarageCars)] <- 0
training_data$MasVnrType %na<-% "None"
training_data %<>% mutate(totalSF = training_data$`1stFlrSF` + training_data$`2ndFlrSF` + TotalBsmtSF)
training_data %<>% mutate(MultipleFloor = if_else(training_data$`2ndFlrSF` >0,1,0))
training_data %<>% mutate(logGrLivArea = log(GrLivArea))

training_data = training_data %>% 
  mutate(RoofMatl = replace(RoofMatl, RoofMatl == "WdShake", "WdShngl")) %>%
  mutate(Exterior1st = replace(Exterior1st, Exterior1st == "AsphShn","AsbShng")) %>%
  mutate(Exterior1st = replace(Exterior1st, Exterior1st == "CBlock","CemntBd")) %>%
  mutate(Exterior1st = replace(Exterior1st, Exterior1st == "NONE","HdBoard")) %>%
  mutate(Heating = replace(Heating, Heating == "Wall","GasW")) %>%
  mutate(KitchenQual = replace(KitchenQual, KitchenQual == "NONE","TA")) %>%
  mutate(Functional = replace(Functional, Functional == "NONE","Typ")) %>%
  mutate(SaleType = replace(SaleType, SaleType == "Con","ConLw")) %>%
  mutate(SaleType = replace(SaleType, SaleType == "Oth","WD")) %>%
  mutate(SaleType = replace(SaleType, SaleType == "NONE","New")) %>%
  mutate(WoodDeckSF_group = replace(WoodDeckSF_group, WoodDeckSF_group == "NONE","[0,100]")) %>%
  mutate(OpenPorchSF_group = replace(OpenPorchSF_group, OpenPorchSF_group == "NONE","[0,50]")) %>%
  mutate(EnclosedPorch_group = replace(EnclosedPorch_group, EnclosedPorch_group == 0,"[0,50]")) %>%
  mutate(SalePrice = replace(SalePrice, SalePrice == "none",0)) %>%
  mutate(SalePrice = as.numeric(SalePrice)) %>%
  mutate(MSZoning = replace(MSZoning, MSZoning == "NONE","RL")) %>%
  mutate(Condition2 = replace(MSZoning, MSZoning == "Feedr","Norm")) %>%
  mutate(Condition2 = replace(MSZoning, MSZoning == "PosA","RRAn")) %>%
  mutate(BsmtFullBath = if(BsmtFullBath==3) {BsmtFullBath == 2}) %>%
  mutate(BsmtHalfBath = if(BsmtHalfBath==2){BsmtHalfBath == 1}) %>%
  mutate(RoofStyle = replace(RoofStyle, RoofStyle == "Shed","Mansard")) %>%
  mutate(SaleCondition = replace(SaleCondition, SaleCondition == "AdjLand","Normal")) %>%
  mutate(ExterCond = replace(ExterCond, ExterCond == "Po","Fa"))
  


table(ames_test$SaleCondition)
table(model_Test_Set$SaleCondition)
training_data %<>% 
  mutate(logGrLivArea = log(GrLivArea)) %>%
  mutate(basement_y = if_else(TotalBsmtSF > 0,1,0))
training_data %<>% mutate(Heating_y = if_else(!is.na(Heating),1,0))
training_data %<>% mutate(HVAC = if_else((CentralAir == 1 || Heating == 1),1,0))
training_data %<>% mutate(logTotalSF = log(totalSF))

drop<- c("PoolQC","MiscFeature", "Fence","FireplaceQu","Alley","Street" )
training_data = training_data[,!(names(training_data) %in% drop)]

training_data %<>% dplyr::select(1:6,8:10,13:15,17:19,21,25:27,29,47,48,55,67:79,83,84)
names(training_data)
SP<-0                
model_Test_Set = training_data%>% filter(SalePrice %in% SP)
training_data= training_data%>% filter(SalePrice %notin% SP)
full_training <-training_data
full_training$basement_y = as.factor(full_training$basement_y)
full_training$FireplaceY = as.factor(full_training$FireplaceY)
full_training$GarageTypeY = as.factor(full_training$GarageTypeY)
full_training$GarageCars = as.numeric(full_training$GarageCars)

testList = c(9,14,15,16,17,27,29,30,32,36,44,50,51,52,53,60,61,70,72,74,76,80,85,88,92,93,94,98,101,
             102,104,107,113,114,125,134,135,137,146,160,164,165,167,169,171,173,175,177,183,186,187,188,203,207,208,209,220,222,
             225,235,238,239,240,244,247,253,254,267,270,271,274,275,278,280,281,282,283,286,290,295,297,309,310,312,319,323,331,
             340,342,344,346,349,351,352,354,363,365,366,377,379,383,385,396,397,398,403,404,406,407,409,412,417,418,423,425,433,
             439,442,443,448,455,469,471,492,499,501,502,506,508,509,511,514,516,518,522,525,531,537,538,546,550,551,560,562,579,
             583,586,589,594,597,602,604,608,612,614,616,623,625,628,633,634,636,637,638,645,646,648,658,660,663,665,667,670,672,
             673,677,680,687,693,694,702,711,715,723,732,733,735,745,747,757,759,762,766,767,770,774,777,781,787,789,797,800,801,
             812,816,817,818,821,825,827,829,832,839,842,852,855,861,872,875,878,884,885,889,890,903,905,906,915,925,930,942,944,
             946,947,954,957,959,973,976,984,986,987,993,996,1001,1004,1006,1007,1015,1016,1017,1018,1020,1022,1026,1027,1029,1036,1046,1048,1050,
             1058,1059,1061,1063,1065,1067,1072,1074,1076,1082,1085,1090,1094,1095,1098,1108,1112,1116,1119,1125,1130,1135,1138,1139,1144,1149,1154,1158,1160,
             1161,1164,1165,1168,1170,1172,1173,1174,1176,1182,1184,1187,1189,1207,1209,1212,1216,1217,1219,1233,1234,1235,1239,1240,1244,1252,1257,1262,1266,
             1269,1280,1283,1290,1294,1298,1301,1307,1311,1312,1313,1317,1320,1322,1323,1329,1334,1338,1342,1344,1347,1352,1355,1361,1363,1367,1370,1373,1376,
             1378,1381,1382,1384,1387,1403,1406,1412,1416,1424,1428,1435,1440,1441,1449,1452,1459) 

#select variables


ames_test = full_training %>% filter(Id %in% testList)
ames_train = full_training %>% filter(Id %notin% testList)
skim(ames_train)
