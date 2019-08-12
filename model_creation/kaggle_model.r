install.packages(c("skimr","tangram",'olsrr','rlang','forcats','roperators','stats'))
library(tidyverse) #general data wrangling tools
library(skimr) #summary stats
library(corrplot)
library(MASS)#studentized residuals
library(lindia)
library(ggplot)
library(car) # Regression tools
library(rlang)
library(olsrr)
library(forcats) 
library(roperators)
library(gridExtra)
library(mefa4)
library(stats)


full_training = training_data

drop<- c("Id")
full_training = full_training[,!(names(full_training) %in% drop)]
skim(full_training)
#Linearity Check----

training_corr = full_training %>% 
  dplyr::select(YearBuilt,YrSold,SalePrice,logGrLivArea,logTotalSF)#select all quantitiative variables here

training_corr%>% skim
sum(is.na(full_training))

corr_object = cor(training_corr)
corrplot(corr_object, method = "number")#use this in write-up

#Initial Model----
options(max.print=999999)
full.model <- lm(SalePrice ~., data = full_training)
summary(full.model)#.8487
vif(full.model)

#Outlier Check----

plot(full.model)
ols_plot_cooksd_bar(full.model)


#Outlier Removal----
full_training_no <- full_training[-c(108,314,589,770,692,1183,1299,186,524,899,1183,1170,1212,1181,1169,804),] 


#Model outliers addressed----
new_model = lm(SalePrice~., data = full_training_no)#interaction terms will need to be added here
summary(new_model) #R^2 = 0.8896
vif(new_model)

#Model Selection----
#Step Selection----
stepAIC(new_model, direction = "both")
stepmodel = lm(SalePrice ~ MSZoning + LotFrontage + LotArea + LandContour + 
                 LotConfig + LandSlope + Neighborhood + BldgType + HouseStyle + 
                 OverallQual + YearBuilt + Exterior1st + ExterQual + ExterCond + 
                 KitchenAbvGr + KitchenQual + GarageCars + MoSold + SaleCondition + 
                 WoodDeckSF_group + OpenPorchSF_group + TotalBath + BedroomAbvGr4plus + 
                 TotRmsAbvGrd10plus + FireplaceY + logGrLivArea + basement_y, data = full_training_no)
summary(stepmodel) #.8871
stepmodel_cv = lm(SalePrice ~ MSZoning + LotFrontage + LotArea + LandContour + 
                    LotConfig + LandSlope + Neighborhood + BldgType + HouseStyle + 
                    OverallQual + YearBuilt + Exterior1st + ExterQual + ExterCond + 
                    KitchenAbvGr + KitchenQual + GarageCars + MoSold + SaleCondition + 
                    WoodDeckSF_group + OpenPorchSF_group + TotalBath + BedroomAbvGr4plus + 
                    TotRmsAbvGrd10plus + FireplaceY + logGrLivArea + basement_y, data = ames_test)

#Forward Selection----
stepAIC(new_model, direction = "forward")
forward = lm(SalePrice ~ MSSubClass + MSZoning + LotFrontage + 
               LotArea + LandContour + LotConfig + LandSlope + Neighborhood + 
               BldgType + HouseStyle + OverallQual + YearBuilt + YearRemodAdd + 
               RoofStyle + Exterior1st + ExterQual + ExterCond + Foundation + 
               KitchenAbvGr + KitchenQual + GarageCars + MoSold + YrSold + 
               SaleType + SaleCondition + WoodDeckSF_group + OpenPorchSF_group + 
               EnclosedPorch_group + TotalBath + BedroomAbvGr4plus + TotRmsAbvGrd10plus + 
               FireplaceY + GarageTypeY + logGrLivArea + basement_y, data = full_training_no)
summary(forward)#.8894
forward_cv = lm(SalePrice ~ MSSubClass + MSZoning + LotFrontage + 
                  LotArea + LandContour + LotConfig + LandSlope + Neighborhood + 
                  BldgType + HouseStyle + OverallQual + YearBuilt + YearRemodAdd + 
                  RoofStyle + Exterior1st + ExterQual + ExterCond + Foundation + 
                  KitchenAbvGr + KitchenQual + GarageCars + MoSold + YrSold + 
                  SaleType + SaleCondition + WoodDeckSF_group + OpenPorchSF_group + 
                  EnclosedPorch_group + TotalBath + BedroomAbvGr4plus + TotRmsAbvGrd10plus + 
                  FireplaceY + GarageTypeY + logGrLivArea + basement_y, data = ames_test)
#Backward Selection----
stepAIC(new_model, direction = "backward")
backward = lm(SalePrice ~ MSZoning + LotFrontage + LotArea + LandContour + 
                LotConfig + LandSlope + Neighborhood + BldgType + HouseStyle + 
                OverallQual + YearBuilt + Exterior1st + ExterQual + ExterCond + 
                KitchenAbvGr + KitchenQual + GarageCars + MoSold + SaleCondition + 
                WoodDeckSF_group + OpenPorchSF_group + TotalBath + BedroomAbvGr4plus + 
                TotRmsAbvGrd10plus + FireplaceY + logGrLivArea + basement_y, data = full_training_no)
summary(backward)#0.8871
backward_cv = lm(SalePrice ~ MSZoning + LotFrontage + LotArea + LandContour + 
                   LotConfig + LandSlope + Neighborhood + BldgType + HouseStyle + 
                   OverallQual + YearBuilt + Exterior1st + ExterQual + ExterCond + 
                   KitchenAbvGr + KitchenQual + GarageCars + MoSold + SaleCondition + 
                   WoodDeckSF_group + OpenPorchSF_group + TotalBath + BedroomAbvGr4plus + 
                   TotRmsAbvGrd10plus + FireplaceY + logGrLivArea + basement_y,data = ames_test)
#Custom Model----

custom = lm(SalePrice ~ MSSubClass + MSZoning + LotFrontage + 
              LotArea + LandContour + LotConfig + LandSlope + Neighborhood + 
              BldgType + HouseStyle + OverallQual + YearBuilt + YearRemodAdd + 
              RoofStyle + Exterior1st + ExterQual + ExterCond + Foundation + 
              KitchenAbvGr + KitchenQual + GarageCars + MoSold + YrSold + 
              SaleType + SaleCondition + WoodDeckSF_group + OpenPorchSF_group + 
              EnclosedPorch_group + TotalBath + BedroomAbvGr4plus + TotRmsAbvGrd10plus + 
              FireplaceY + GarageTypeY + logGrLivArea + basement_y + logGrLivArea*OverallQual, data = full_training_no)
summary(custom)#0.9043


custom_cv = lm(SalePrice ~ MSSubClass + MSZoning + LotFrontage + 
                 LotArea + LandContour + LotConfig + LandSlope + Neighborhood + 
                 BldgType + HouseStyle + OverallQual + YearBuilt + YearRemodAdd + 
                 RoofStyle + Exterior1st + ExterQual + ExterCond + Foundation + 
                 KitchenAbvGr + KitchenQual + GarageCars + MoSold + YrSold + 
                 SaleType + SaleCondition + WoodDeckSF_group + OpenPorchSF_group + 
                 EnclosedPorch_group + TotalBath + BedroomAbvGr4plus + TotRmsAbvGrd10plus + 
                 FireplaceY + GarageTypeY + logGrLivArea + basement_y+ logGrLivArea*OverallQual +,data = ames_test)
skim(full_training_no)
#AssumptionChecks----
#Assumption Check New Model----
predictmodel = predict(stepmodel)
# Histogram overlaid with kernel density curve
gg_reshist(custom)

ggplot(full_training_no, aes(x=predictmodel)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Distribution of Residuals") +
  theme_bw()

#QQ-plot
gg_qqplot(custom, scale.factor = 1) +
  ggtitle("Residual QQ-Plot") +
  theme_bw()

#reisidual Plot
ggplot(custom, aes(.fitted, .resid)) + 
  geom_point() + 
  geom_hline(yintercept=0, col="red", linetype="dashed") + 
  xlab("Fitted values") + 
  ylab("Residuals") + 
  ggtitle("Residual vs Fitted Plot") +
  theme_bw()

#standardized residual plots
ggplot(custom, aes(.fitted, .stdresid)) + 
  geom_point() + 
  geom_hline(yintercept=0, col="red", linetype="dashed") + 
  xlab("Fitted values") + 
  ylab("Studentized Residuals") + 
  ggtitle("StdResid vs Fitted Plot") +
  theme_bw()+
  geom_hline(yintercept=c(-2,2),linetype="dashed")
#Leverage and Outlier Check---- 
#Cooks
gg_cooksd(full.model,label = TRUE, show.threshold = TRUE,
          threshold = "convention", scale.factor = 0.2)

#Leverage Plot
ggplot(full.model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE,show.legend = FALSE) +
  stat_smooth(method="loess", na.rm=TRUE) + 
  xlab("Leverage") + 
  ylab("Standardized Residuals") + 
  ggtitle("Residual vs Leverage Plot") + 
  theme_bw()
grid.arrange(cooksp, leverage, nrow = 1)

#Cross Validation
crossVal <-function(model,modelData){
stepmodelpredict = predict(model,interval = "predict",newdata = modelData)
as.data.frame(stepmodelpredict)

#Find MSPE and check
MSPE = data.frame(Observed = modelData$SalePrice, Predicted = stepmodelpredict[,1])
MSPE$Observed=as.numeric(as.character(MSPE$Observed))
MSPE$Residual = MSPE$Observed - MSPE$Predicted
MSPE$SqauaredResidual = MSPE$Residual^2
print("MSPE is: ")
mean(MSPE$SqauaredResidual)
}

crossVal(stepmodel_cv,ames_test)
crossVal(forward_cv,ames_test)
crossVal(backward_cv,ames_test)
crossVal(custom_cv,ames_test)

#Predict----
#Stepwise Predict
SalePriceSW = predict(stepmodel,newdata = model_Test_Set )
sw_kaggle = cbind(model_Test_Set,SalePriceSW)
sw_kaggle = model_Test_Set %>% dplyr::select(Id, SalePrice) %>%
  mutate(SalePrice = SalePriceSW )

#Forwards Predict #ERROR
SalePricef = predict(forward,newdata = model_Test_Set )
f_kaggle = cbind(model_Test_Set,SalePricef)
forward_kaggle = model_Test_Set %>% dplyr::select(Id, SalePrice) %>%
  mutate(SalePrice = SalePricef)


#Backwards predict
SalePriceb= predict(backward,newdata = model_Test_Set)
back_kaggle = cbind(model_Test_Set,SalePriceb)
back_kaggle = model_Test_Set %>% dplyr::select(Id, SalePrice) %>%
  mutate(SalePrice = SalePriceb )

#Custom
SalePricec= predict(custom_cv,newdata = model_Test_Set)
custom_kaggle = cbind(model_Test_Set,SalePricec)
custom_kaggle = model_Test_Set %>% dplyr::select(Id, SalePrice) %>%
  mutate(SalePrice = SalePricec)

write.csv(sw_kaggle, file = "sw_kaggle.csv")
write.csv(f_kaggle, file = "f_kaggle.csv")
write.csv(back_kaggle, file = "back_kaggle.csv")
write.csv(custom_kaggle, file = "custom_kaggle.csv")
