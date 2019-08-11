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
                BsmtFullBath,     
                BsmtHalfBath,       
                BsmtUnfSF,      
                EnclosedPorch,             
                Fireplaces,      
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
                YrSold,
                SalePrice)#select all quantitiative variables here

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
#' GrLivArea
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
#'  exterior quality 27
#'  BsmtFinType1 33

names(full_training)
#Remove High VIF Variables----
drop<- c("Exterior2nd","BsmtFinSF2","ExterCond","GrLivArea", "logYearBuilt","GarageCars", "GarageCond","GarageQual", "porch_yn", "FireplaceY","GarageTypeY",
         "porchArea", "BsmtFinType1", "OpenPorchSF_y","TotalBath","logGrLivArea","ExterQual","TotalBsmtSF","totalSF","Id","pool_yn","BsmtFinSF1","logSalePrice")
full_training_noVIF = full_training[,!(names(full_training) %in% drop)]



full.model2 <- lm(SalePrice ~., data = full_training_noVIF)
summary(full.model2) #R^2 = 0.9252
vif(full.model2)

#Outlier Check----
plot(full.model2)

ols_plot_cooksd_bar(full.model2)

#Outlier Removal----
full_training_no <- full_training_noVIF[-c(514,675,883,617,9,256,65,146,389),] 
#cooks plot outliers - 427,314,1029,127,547


#Model outliers addressed----
new_model = lm(SalePrice~., data = full_training_no)#interaction terms will need to be added here
summary(new_model) #R^2 = 0.9361
vif(new_model)


#Selection + Custom

#Step
stepmodel = stepAIC(new_model, direction = "both",trace=FALSE)
summary(stepmodel)#r^2=0.9365

#Forwards

forwards = stepAIC(new_model, direction = "forward",trace=FALSE)
summary(forwards)#r^2=0.9361

#Backwards
b.step.model <- stepAIC(new_model, direction = "backward", trace = FALSE)
summary(b.step.model)#r^2=0.9365


#Custom
custom = lm(SalePrice ~ MSSubClass + MSZoning + LotFrontage + 
              LotArea + Street + LotShape + LandContour + Utilities + LotConfig + 
              LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + 
              HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + 
              RoofStyle + RoofMatl + Exterior1st + MasVnrType + MasVnrArea + 
              Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType2 + 
              BsmtUnfSF + Heating + HeatingQC + CentralAir + Electrical + 
              `1stFlrSF` + `2ndFlrSF` + LowQualFinSF + BsmtFullBath + BsmtHalfBath + 
              FullBath + HalfBath + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + 
              Functional + Fireplaces + FireplaceQu + GarageType + GarageYrBlt + 
              GarageFinish + GarageArea + PavedDrive + WoodDeckSF + OpenPorchSF + 
              EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + Fence + 
              MiscVal + MoSold + YrSold + SaleType + SaleCondition + logSalePrice + 
              X3SsnPorch_y + EnclosedPorch_y + ScreenPorch_y + WoodDeckSF_group + 
              OpenPorchSF_group + EnclosedPorch_group + BedroomAbvGr4plus + 
              TotRmsAbvGrd10plus + MultipleFloor+ Heating*CentralAir , data = full_training_no)

summary(custom)#r^2=0.9724
vif(stepmodel)

#Assumption Check New Model----
require(c('lindia','ggplot2','gridExtra'))
assumptionPlots<-function(model, data){
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
plot(stepmodel)

#Model selection assupmtion checks----
assumptionPlots(full.model2,full_training)
assumptionPlots(stepmodel,full_training_no)
assumptionPlots(forwards,full_training_no)
assumptionPlots(b.step.model,full_training_no)
assumptionPlots(custom,full_training_no)

#model selection----
#http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/


#Cross Validation----
#Generate prediction
cv <- function(model, data){
stepmodelpredict = predict(model,interval = "predict",newdata = data)
as.data.frame(stepmodelpredict)

#Find MSPE and check
MSPE = data.frame(Observed = data$SalePrice, Predicted = stepmodelpredict)
MSPE$Residual = MSPE$Observed -MSPE$Predicted.fit
MSPE$SqauaredResidual = MSPE$Residual^2
print("MSPE is: ")
mean(MSPE$SqauaredResidual)


}
cv(stepmodel,full_training_no)
cv(forwards,full_training_no)
cv(b.step.model,full_training_no)
cv(custom,full_training_no)


