#libraries----
install.packages(c("skimr","tangram","fastDummies",'olsrr','rlang','forcats','roperators'))
library(tidyverse) #general data wrangling tools
library(skimr) #summary stats
library(tangram) #has is.categorical() function, useful for creating tables
library(car) # Regression tools
library(fastDummies) # creates dummy variables
library(rlang)
library(olsrr)
library(forcats) 
library(roperators)
library(corrplot)
library(mefa4)
library(MASS)#studentized residuals
library(caret)


full_training =ames_train

#Linearity Check----

training_corr = full_training %>% 
  dplyr::select(`1stFlrSF`,       
                `2ndFlrSF`,      
                X3SsnPorch,     
                BedroomAbvGr4plus,       
                BsmtFinSF1,       
                BsmtFinSF2,      
                BsmtFullBath,     
                BsmtHalfBath,       
                BsmtUnfSF,      
                EnclosedPorch,             
                Fireplaces,      
                FireplaceY,      
                FullBath,     
                GarageArea,      
                GrLivArea,     
                logGrLivArea,      
                logSalePrice,   
                logYearBuilt,     
                LowQualFinSF,    
                MasVnrArea,         
                MoSold,      
                OpenPorchSF,        
                OverallQual,           
                PoolArea,         
                SalePrice,           
                TotalBsmtSF,   
                TotRmsAbvGrd,      
                TotRmsAbvGrd10plus,     
                WoodDeckSF,    
                YearBuilt,     
                YrSold  )#select all quantitiative variables here

training_corr%>% skim
sum(is.na(full_training))

corr_object = cor(training_corr)
corrplot(corr_object, method = "number")#use this in write-up


#Initial Model----
options(max.print=999999)
full.model <- lm(logSalePrice ~., data = full_training)
summary(full.model)
vif(full.model)

#'Remove Quant Variable due to high VIF
#'YearBuilt  97754.0000
#'`1stFlrSF` 42.8860
#'`2ndFlrSF` 41.1500
#'WoodDeckSF  21.9280    
#' OpenPorchSF 14.8880
#' EnclosedPorch  47.1830 
#'  PoolArea 111.7800 
#'  logYearBuilt 96023.0000
#'  logGrLivArea 54.2760
#'  porch_yn due to multicolinearity NA in output
#'  OpenPorchSF_y due to multicolinearity NA in output
#'  totalbath due to multicolinearity NA in output
#'  FireplaceY due to multicolinearity NA in output                           
#'  GarageTypeY due to multicolinearity NA in output                      
#'  totalSF due to multicolinearity NA in output  


#Remove High VIF Variables----
full_training_noVIF <- full_training[ -c(1,18,42,43,64:66,69,79,82,86,88,91:95)]

full.model2 <- lm(logSalePrice ~., data = full_training_noVIF)
summary(full.model2)
vif(full.model2)

#Outlier Check----
plot(full.model)
ols_plot_cooksd_bar(initial_model)

#Outlier Removal----
full_training_no <- full_training_noVIF[-c(74,266,416,547,548,651,892),] 

full_training_no$BsmtFinType1 <- relevel(full_training_no$BsmtFinType1, ref = "None")

#Model outliers addressed----
new_model = lm(logSalePrice~., data = full_training_no)#interaction terms will need to be added here
summary(new_model) #R^2 = 0.9726
vif(new_model)


#Selection + Custom

#Step
stepAIC(new_model, direction = "both")

stepmodel = lm(logSalePrice ~  MSSubClass + MSZoning + LotFrontage + 
                 LotArea + Utilities + Neighborhood + Condition1 + HouseStyle + 
                 OverallQual + OverallCond + Exterior1st + MasVnrType + MasVnrArea + 
                 ExterQual + ExterCond + Foundation + BsmtQual + TotalBsmtSF + 
                 Heating + HeatingQC + CentralAir + LowQualFinSF + GrLivArea + 
                 BsmtFullBath + BsmtHalfBath + HalfBath + KitchenQual + TotRmsAbvGrd + 
                 Functional + Fireplaces + FireplaceQu + GarageType + GarageFinish + 
                 GarageCars + GarageArea + GarageQual + PavedDrive + ScreenPorch + 
                 Fence + MoSold + SaleType + SaleCondition + SalePrice + pool_yn + 
                 OpenPorchSF_group + EnclosedPorch_group + BedroomAbvGr4plus + 
                 TotRmsAbvGrd10plus-ID, data = full_training_no)

summary(stepmodel)#r^2=0.9723

#Forwards
stepAIC(new_model, direction = "forward")
forwards = lm(logSalePrice ~  MSSubClass + MSZoning + LotFrontage + 
                LotArea + Street + LotShape + LandContour + Utilities + LotConfig + 
                LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + 
                HouseStyle + OverallQual + OverallCond + YearRemodAdd + RoofStyle + 
                RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + 
                ExterQual + ExterCond + Foundation + BsmtQual + BsmtCond + 
                BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + 
                BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + 
                CentralAir + Electrical + LowQualFinSF + GrLivArea + BsmtFullBath + 
                BsmtHalfBath + FullBath + HalfBath + KitchenAbvGr + KitchenQual + 
                TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType + 
                GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + 
                GarageCond + PavedDrive + `3SsnPorch` + ScreenPorch + Fence + 
                MiscVal + MoSold + YrSold + SaleType + SaleCondition + SalePrice + 
                pool_yn + porchArea + `3SsnPorch_y` + EnclosedPorch_y + ScreenPorch_y + 
                WoodDeckSF_group + OpenPorchSF_group + EnclosedPorch_group + 
                BedroomAbvGr4plus + TotRmsAbvGrd10plus + MultipleFloor, data = full_training_no)
summary(forwards)#r^2=0.9726

#Backwards
b.step.model <- stepAIC(new_model, direction = "backward", 
                        trace = FALSE)
summary(b.step.model)#r^2=0.9726


#Custom
custom = lm(logSalePrice ~ Id + MSSubClass + MSZoning + LotFrontage + 
                 LotArea + Utilities + Neighborhood + Condition1 + HouseStyle + 
                 OverallQual + OverallCond + Exterior1st + MasVnrType + MasVnrArea + 
                 ExterQual + ExterCond + Foundation + BsmtQual + TotalBsmtSF + 
                 Heating + HeatingQC + CentralAir + LowQualFinSF + GrLivArea + 
                 BsmtFullBath + BsmtHalfBath + HalfBath + KitchenQual + TotRmsAbvGrd + 
                 Functional + Fireplaces + FireplaceQu + GarageType + GarageFinish + 
                 GarageCars + GarageArea + GarageQual + PavedDrive + ScreenPorch + 
                 Fence + MoSold + SaleType + SaleCondition + SalePrice + pool_yn + 
                 OpenPorchSF_group + EnclosedPorch_group + BedroomAbvGr4plus + 
                 TotRmsAbvGrd10plus + , data = full_training_no)

summary(custom)#r^2=0.9724
vif(stepmodel)

#Assumption Check New Model----
#residual

plot(new_model)

res=resid(new_model)
plot(training_model_no$"INSERT EXPLAINATORY HERE", res,#insert explanatory here or predicted value???
     ylab="Residuals", xlab="prices_linlog_nol", 
     main="prices_linlog_nol Residual Plot")  #change title here
abline(0, 0, col = 'red')    


#studentized resid
sresid <- studres(new_model)
plot(training_model_no$logGrLivArea, sresid,#change explanatory
     ylab="Studentized Residuals", xlab="prices_linlog_nol", 
     main="prices_linlog_nol Studentized Residual Plot") 
abline(2, 0, col = 'red')    
abline(-2, 0, col = 'red')    


#resid hist
library(MASS)
resid <- studres(new_model)
hist(resid, freq=FALSE,
     main="Distribution of Residuals")
xfit<-seq(min(resid),max(resid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit) 

#qqplot
qqnorm(res)
qqline(res, col = "steelblue", lwd = 2)

#model selection----
#http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/


#Cross Validation----
#install.packages('forecast')



#Assumption Check on optimum model----
