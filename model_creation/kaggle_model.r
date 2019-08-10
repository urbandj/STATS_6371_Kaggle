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

#Create Dataset object----
names(training_data)
training_model = training_data %>%
  dplyr::select(3,8,9,11:19,23:26,28:43,46,60,61,63,64,76:92,94:96,98:106,119:126)

training_model %>% skim
#Linearity Check----

hist(training_data$MasVnrArea)
training_corr = training_model %>% 
  dplyr::select(BedroomAbvGr4plus,   
            GarageArea,  
            logGrLivArea,  
            logSalePrice,  
            MasVnrArea.b,  
            MoSold, 
            OverallCond,  
            OverallQual,  
            TotalBath,      
            TotRmsAbvGrd10plus,     
            YrSold,
            totalSF,
            porchArea)#select all quantitiative variables here

training_corr%>% skim

corr_object = cor(training_corr)
corrplot(corr_object, method = "number")#use this in write-up


#Initial Model----
options(max.print=999999)
initial_model = lm(logSalePrice~.-logGrLivArea,data = training_model)#once lines 22&23 are completed ~. inputs all variables into model,interaction terms can be added via "+ term1*term2"
summary(initial_model)
vif(initial_model)

table(training_data$GarageCars_f)
#Outlier Check----
plot(initial_model)
ols_plot_cooksd_bar(initial_model)


#Outlier Removal----
training_model_no <- training_model[-c(463,633,692,1170),] 

#Model outliers addressed----
new_model = lm(logSalePrice~.-logGrLivArea -porch_yn - PavedDriveY - pool_y -OpenPorchSF_y, data = training_model_no)#interaction terms will need to be added here
summary(new_model)
vif(new_model)


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

#Stepwise and AIC methods
stepAIC(new_model, direction = "both")

stepmodel = lm(logSalePrice ~ MSZoning + Neighborhood + Condition1 + 
                 Condition2 + HouseStyle + OverallQual + OverallCond + Exterior1st + 
                 Exterior2nd + ExterQual + ExterCond + Foundation + BsmtQual + 
                 BsmtCond + BsmtFinSF1 + BsmtUnfSF + Heating + HeatingQC + 
                 CentralAir + LowQualFinSF + GarageFinish + GarageArea + GarageQual + 
                 MoSold + SaleType + SaleCondition + SalePrice + GarageCars_f + 
                 Fence_f + BedroomAbvGr4plus + GarageTypeY + GarageCond.Fac + 
                 PavedDrive.Fac + MSSubClass_group + LotArea_group + YearBuilt.b + 
                 YearRemodAdd.b + porchArea + EnclosedPorch_y + ScreenPorch_y + 
                 totalSF, data = training_model_no)

summary(stepmodel)#r^2=0.9739
vif(stepmodel)

stepAIC(new_model, direction = "forward")
forwards = lm(logSalePrice ~ (MSZoning + LotShape + LandContour + 
                                LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + 
                                BldgType + HouseStyle + OverallQual + OverallCond + RoofMatl + 
                                Exterior1st + Exterior2nd + MasVnrType + ExterQual + ExterCond + 
                                Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + 
                                BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + 
                                Heating + HeatingQC + CentralAir + Electrical + LowQualFinSF + 
                                GarageYrBlt + GarageFinish + GarageArea + GarageQual + MiscVal + 
                                MoSold + YrSold + SaleType + SaleCondition + SalePrice + 
                                GarageCars_f + pool_yn + porch_yn + Fence_f + WoodDeckSF_group + 
                                OpenPorchSF_group + EnclosedPorch_group + TotalBath + BedroomAbvGr4plus + 
                                KitchenQual.Fac + TotRmsAbvGrd10plus + GarageTypeY + GarageCond.Fac + 
                                PavedDrive.Fac + PavedDriveY + MSSubClass_group + LotFrontage_group + 
                                LotArea_group + YearBuilt.b + YearRemodAdd.b + RoofStyleY + 
                                MasVnrArea.b + logGrLivArea + pool_y + porchArea + `3SsnPorch_y` + 
                                EnclosedPorch_y + OpenPorchSF_y + ScreenPorch_y + totalSF) - 
                logGrLivArea - porch_yn - PavedDriveY - pool_y - OpenPorchSF_y, data = training_model_no)

summary(forwards)#r^2=0.9734


stepAIC(new_model, direction = "backward")
backward = lm(logSalePrice ~ MSZoning + Neighborhood + Condition1 + 
                Condition2 + HouseStyle + OverallQual + OverallCond + Exterior1st + 
                Exterior2nd + ExterQual + ExterCond + Foundation + BsmtQual + 
                BsmtCond + BsmtFinSF1 + BsmtUnfSF + Heating + HeatingQC + 
                CentralAir + LowQualFinSF + GarageFinish + GarageArea + GarageQual + 
                MoSold + SaleType + SaleCondition + SalePrice + GarageCars_f + 
                Fence_f + BedroomAbvGr4plus + GarageTypeY + GarageCond.Fac + 
                PavedDrive.Fac + MSSubClass_group + LotArea_group + YearBuilt.b + 
                YearRemodAdd.b + porchArea + EnclosedPorch_y + ScreenPorch_y + 
                totalSF, data = training_model_no)

summary(backward)#r^2=0.9739


#Cross Validation----
#install.packages('forecast')


model_forwards <- train(logSalePrice ~ (ID+MSSubClass + MSZoning + LandContour + 
                                          Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + 
                                          OverallQual + OverallCond + YearRemodAdd + RoofMatl + Exterior1st + 
                                          Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + 
                                          Foundation + BsmtQual + BsmtCond + BsmtFinType1 + BsmtFinSF1 + 
                                          BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + 
                                          CentralAir + Electrical + FireplaceQu + GarageArea + GarageQual + 
                                          ScreenPorch + MoSold + YrSold + SaleType + SaleCondition + 
                                          GarageCars_f + pool_yn + porch_yn + Fence_f + WoodDeckSF_group + 
                                          OpenPorchSF_group + EnclosedPorch_group + TotalBath + BedroomAbvGr4plus + 
                                          KitchenQual.Fac + KitchenQual.FacNum + TotRmsAbvGrd10plus + 
                                          GarageTypeY + GarageCond.Fac + GarageCond.N + PavedDrive.Fac + 
                                          PavedDriveY + MSSubClass_group + LotFrontage_group + LotArea_group + 
                                          YearBuilt.b + YearRemodAdd.b + RoofStyleY + MasVnrArea.b + 
                                          ExterQual.n + logGrLivArea) - logGrLivArea - GarageCond.N - 
                          PavedDriveY - porch_yn - BsmtFinType1 - ExterQual.n,data=training_model_no, method = "lm",
               trControl = train.control)
# Summarize the results
print(model_forwards) #R^2 = 0.8438159


#Assumption Check on optimum model----
