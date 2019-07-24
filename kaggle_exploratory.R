##Jeff Nguyen
#Kaggle Project
#Each section is condensed for readability

#import libraries----
install.packages(c("skimr","tangram"))
library(tidyverse)
library(skimr)
library(tangram)
library(car)

#filter out based on Analaysis 1 Requirements----
names(train)
century21 = train %>% 
            filter(Neighborhood %in% c('NAmes','Edwards','BrkSide')) %>%
            mutate(GrLivArea = GrLivArea/100) %>% #ask for clarification divide on frontend or backend - feel free to remove
            mutate(logLotArea = log(LotArea)) %>% 
            mutate(logGrLivArea = log(GrLivArea)) %>%
             as_tibble()

#Get tables for all categorical variables----
cat_DF <- century21[, sapply(century21, is.categorical)]
lapply(cat_DF, function(x) table(x, useNA="ifany"))

#Get summary statistics----
century21 %>% glimpse
century21 %>% skim


#model construction (current model is for exploratory purposes)----
homePrice = lm(SalePrice ~  Neighborhood + YearBuilt + logLotArea + logGrLivArea + CentralAir + YearRemodAdd + HouseStyle + PoolArea + KitchenAbvGr + Street +
                 MSZoning + LotShape  + BldgType + RoofStyle + Exterior1st + ExterCond + Foundation + Heating + Electrical, data = century21)


vif(homePrice)
summary(homePrice)

#Model Selection----
library(MASS)
stepAIC(homePrice, direction = "both", trace = FALSE)

houseprice2 = lm(formula = SalePrice ~ YearBuilt + logLotArea + logGrLivArea + 
                   CentralAir + YearRemodAdd + HouseStyle + KitchenAbvGr + LotShape + 
                   Exterior1st + ExterCond, data = century21)
summary(houseprice2)

#Other Variables typed out to save time----
#Alley - BsmtCond - BsmtExposure - BsmtFinType1 - BsmtFinType2 - BsmtQual - Fence - FireplaceQu - GarageCond - GarageFinish - GarageQual - GarageType - MiscFeature - PoolQC

#Residual Checks (To be completed once model is built)----
