#libraries----
install.packages(c("skimr","tangram","fastDummies",'olsrr','rlang','forcats','roperators','stats'))
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
library(stats)


full_training = ames_train

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
full_training_noVIF <- full_training[ -c(1,18,42,43,45,64:66,69,76,79,82,86,88,91:95, 23,27,32,59,61)]

full.model2 <- lm(logSalePrice ~., data = full_training_noVIF)
summary(full.model2) #R^2 = 0.9252
vif(full.model2)

#Outlier Check----
plot(full.model)
ols_plot_cooksd_bar(full.model2)

#Outlier Removal----
full_training_no <- full_training_noVIF[-c(74,266,416,547,548,651,892,922,314,427,1029,547),] 
#cooks plot outliers - 427,314,1029,127,547
full_training_no$BsmtFinType1 <- relevel(full_training_no$BsmtFinType1, ref = "None")

#Model outliers addressed----
new_model = lm(logSalePrice~., data = full_training_no)#interaction terms will need to be added here
summary(new_model) #R^2 = 0.9361
vif(new_model)


#Selection + Custom

#Step
stepAIC(new_model, direction = "both")

stepmodel = lm(logSalePrice ~ MSSubClass + MSZoning + LotFrontage + 
                 LotArea + Utilities + LotConfig + LandSlope + Neighborhood + 
                 Condition1 + OverallQual + OverallCond + RoofMatl + Exterior1st + 
                 Foundation + BsmtQual + BsmtExposure + BsmtFinSF1 + TotalBsmtSF + 
                 Heating + HeatingQC + CentralAir + BsmtFullBath + FullBath + 
                 HalfBath + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + 
                 Fireplaces + GarageType + GarageArea + PavedDrive + X3SsnPorch + 
                 ScreenPorch + MiscVal + SaleCondition + WoodDeckSF_group + 
                 pool_yn + MultipleFloor, data = full_training_no)

summary(stepmodel)#r^2=0.9365




#Forwards
stepAIC(new_model, direction = "forward")
forwards = lm(logSalePrice ~ MSSubClass + MSZoning + LotFrontage + 
                LotArea + Street + LandContour + Utilities + LotConfig + 
                LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + 
                HouseStyle + OverallQual + OverallCond + YearRemodAdd + RoofStyle + 
                RoofMatl + Exterior1st + MasVnrType + MasVnrArea + ExterQual + 
                Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinSF1 + 
                BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + 
                HeatingQC + CentralAir + Electrical + LowQualFinSF + BsmtFullBath + 
                BsmtHalfBath + FullBath + HalfBath + KitchenAbvGr + KitchenQual + 
                TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType + 
                GarageYrBlt + GarageFinish + GarageArea + GarageCond + PavedDrive + 
                X3SsnPorch + ScreenPorch + Fence + MiscVal + MoSold + YrSold + 
                SaleType + SaleCondition + WoodDeckSF_group + OpenPorchSF_group + 
                pool_yn + porchArea + X3SsnPorch_y + EnclosedPorch_y + ScreenPorch_y + 
                BedroomAbvGr4plus + TotRmsAbvGrd10plus + MultipleFloor, data = full_training_no)
summary(forwards)#r^2=0.9361

#Backwards
b.step.model <- stepAIC(new_model, direction = "backward", 
                        trace = FALSE)
summary(b.step.model)#r^2=0.9365


#Custom
custom = lm(logSalePrice ~ MSSubClass + MSZoning + LotFrontage + 
              LotArea + Utilities + LotConfig + LandSlope + Neighborhood + 
              Condition1 + OverallQual + OverallCond + RoofMatl + Exterior1st + 
              Foundation + BsmtQual + BsmtExposure + BsmtFinSF1 + TotalBsmtSF + 
              Heating + HeatingQC + CentralAir + BsmtFullBath + FullBath + 
              HalfBath + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + 
              Fireplaces + GarageType + GarageArea + PavedDrive + X3SsnPorch + 
              ScreenPorch + MiscVal + SaleCondition + WoodDeckSF_group + 
              pool_yn + MultipleFloor + Heating*CentralAir , data = full_training_no)

summary(custom)#r^2=0.9724
vif(stepmodel)

#Assumption Check New Model----

assumptionPlots<-function(model, data){
require(c('lindia','ggplot2','gridExtra'))
  
# Histogram overlaid with kernel density curve
hist = ggplot(data, aes(x=predict(model))) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Distribution of Residuals") +
  theme_bw()

#QQ-plot
qq = gg_qqplot(model, scale.factor = 1) +
  ggtitle("Residual QQ-Plot") +
  theme_bw()

#reisidual Plot
residp = ggplot(model, aes(.fitted, .resid)) + 
  geom_point() + 
  geom_hline(yintercept=0, col="red", linetype="dashed") + 
  xlab("Fitted values") + 
  ylab("Residuals") + 
  ggtitle("Residual vs Fitted Plot") +
  theme_bw()

#standardized residual plots
sresidp = ggplot(model, aes(.fitted, .stdresid)) + 
  geom_point() + 
  geom_hline(yintercept=0, col="red", linetype="dashed") + 
  xlab("Fitted values") + 
  ylab("Studentized Residuals") + 
  ggtitle("StdResid vs Fitted Plot") +
  theme_bw()+
  geom_hline(yintercept=c(-2,2),linetype="dashed")

grid.arrange(residp, sresidp, hist, qq, nrow = 1)
}

#Leverage and Outlier Check---- 
#Cooks
cooksp = gg_cooksd(full.model2,label = TRUE, show.threshold = TRUE,
                   threshold = "convention", scale.factor = 0.2)

#Leverage Plot
leverage = ggplot(full.model2, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE,show.legend = FALSE) +
  stat_smooth(method="loess", na.rm=TRUE) + 
  xlab("Leverage") + 
  ylab("Standardized Residuals") + 
  ggtitle("Residual vs Leverage Plot") + 
  theme_bw()
grid.arrange(cooksp, leverage, nrow = 1)

#Model selection assupmtion checks----
assumptionPlots(full.model2,full_training)
assumptionPlots(stepmodel,full_training_no)
assumptionPlots(forwards,full_training_no)
assumptionPlots(b.step.model,full_training_no)
assumptionPlots(custom,full_training_no)

#model selection----
#http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/


#Cross Validation----
#install.packages('forecast')



#Assumption Check on optimum model----
