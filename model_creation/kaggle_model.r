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
  dplyr::select(1:3,9,13:19,21,23:32,34:38,40:43,58,63,64,71,77:80,82:107,119)

training_model %>% skim
#Linearity Check----

hist(training_data$MasVnrArea)
training_corr = training_model %>% 
  dplyr::select(BedroomAbvGr4plus,   
            GarageArea,  
            Id,  
            logGrLivArea,  
            logSalePrice,  
            MasVnrArea,  
            MoSold, 
            MSSubClass,  
            OverallCond,  
            OverallQual,  
            ScreenPorch,     
            TotalBath,      
            TotRmsAbvGrd10plus,     
            YearRemodAdd,    
            YrSold)#select all quantitiative variables here

training_corr%>% skim

corr_object = cor(training_corr)
corrplot(corr_object, method = "number")#use this in write-up


#Initial Model----
options(max.print=999999)
initial_model = lm(logSalePrice~.-logGrLivArea-GarageCond.N -PavedDriveY-porch_yn -BsmtFinType1-ExterQual.n ,data = training_model)#once lines 22&23 are completed ~. inputs all variables into model,interaction terms can be added via "+ term1*term2"
summary(initial_model)
vif(initial_model)

table(training_data$GarageCars_f)
#Outlier Check----
plot(initial_model)
ols_plot_cooksd_bar(initial_model)


#Outlier Removal----
training_model_no <- training_model[-c(524,633,826,1325),] 

#Model outliers addressed----
new_model = lm(logSalePrice~.-logGrLivArea-GarageCond.N -PavedDriveY-porch_yn -BsmtFinType1-ExterQual.n , data = training_model_no)#interaction terms will need to be added here
summary(new_model)
vif(new_model)

stepAIC(new_model, direction = "both")

stepmodel = lm(logSalePrice ~ MSSubClass + MSZoning + Neighborhood + 
                 Condition1 + HouseStyle + OverallQual + OverallCond + YearRemodAdd + 
                 RoofMatl + Exterior1st + MasVnrArea + Foundation + BsmtQual + 
                 BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + 
                 CentralAir + FireplaceQu + GarageArea + GarageQual + ScreenPorch + 
                 SaleType + SaleCondition + GarageCars_f + pool_yn + WoodDeckSF_group + 
                 TotalBath + BedroomAbvGr4plus + KitchenQual.Fac + TotRmsAbvGrd10plus + 
                 GarageTypeY + GarageCond.Fac + PavedDrive.Fac + MSSubClass_group + 
                 LotFrontage_group + LotArea_group + YearBuilt.b, data = training_model_no)

summary(stepmodel)#r^2=0.923
vif(stepmodel)

stepAIC(new_model, direction = "forward")
forwards = lm(formula = logSalePrice ~ (Id + MSSubClass + MSZoning + LandContour + 
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
                 PavedDriveY - porch_yn - BsmtFinType1 - ExterQual.n, data = training_model_no)

summary(forwards)#r^2=0.9228


stepAIC(new_model, direction = "backward")
backward = lm(logSalePrice ~ MSSubClass + MSZoning + Neighborhood + 
                Condition1 + HouseStyle + OverallQual + OverallCond + YearRemodAdd + 
                RoofMatl + Exterior1st + MasVnrArea + Foundation + BsmtQual + 
                BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + 
                CentralAir + FireplaceQu + GarageArea + GarageQual + ScreenPorch + 
                SaleType + SaleCondition + GarageCars_f + pool_yn + WoodDeckSF_group + 
                TotalBath + BedroomAbvGr4plus + KitchenQual.Fac + TotRmsAbvGrd10plus + 
                GarageTypeY + GarageCond.Fac + PavedDrive.Fac + MSSubClass_group + 
                LotFrontage_group + LotArea_group + YearBuilt.b, data = training_model_no)

summary(backward)#r^2=0.923
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
step(new_model)
stepAIC(new_model, direction = "both")
stepAIC(new_model, direction = "forwards")
stepAIC(new_model, direction = "backwards")


#Cross Validation----
#install.packages('forecast')
library(DAAG)

library(forecast)

forecast::cv(newmodel)
cv.lm(df = training_model_no, form.lm = newmodel,m=3, seed=29)


summary(newmodel)
vif(newmodel)
  
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(logSalePrice ~ Id + MSSubClass + MSZoning + LandContour + 
                 Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + 
                 OverallQual + OverallCond + YearRemodAdd + RoofMatl + Exterior1st + 
                 Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + 
                 Foundation + BsmtQual + BsmtCond + BsmtFinType1 + BsmtFinSF1 + 
                 BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + 
                 CentralAir + Electrical + FireplaceQu + GarageArea + GarageQual + 
                 ScreenPorch + MoSold + YrSold + SaleType + SaleCondition + 
                 Fence_f + WoodDeckSF_group + OpenPorchSF_group + EnclosedPorch_group + 
                 GarageCars_f + pool_yn + porch_yn + TotalBath + BedroomAbvGr4plus + 
                 KitchenQual.Fac + KitchenQual.FacNum + TotRmsAbvGrd10plus + 
                 GarageTypeY + GarageCars3plus + GarageCond.Fac + GarageCond.N + 
                 PavedDrive.Fac + PavedDriveY + MSSubClass_group + LotFrontage_group + 
                 LotArea_group + YearBuilt.b + YearRemodAdd.b + RoofStyleY + 
                 MasVnrArea.b + logGrLivArea - logGrLivArea - GarageCars3plus - 
  GarageCond.N - PavedDriveY - porch_yn, data = training_model_no, method = "lm",
               trControl = train.control)


# Summarize the results
print(model)

cv.lm(training_model_no, form.lm = formula(logSalePrice ~ Id + MSSubClass + MSZoning + LandContour + 
                                                  Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + 
                                                  OverallQual + OverallCond + YearRemodAdd + RoofMatl + Exterior1st + 
                                                  Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + 
                                                  Foundation + BsmtQual + BsmtCond + BsmtFinType1 + BsmtFinSF1 + 
                                                  BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + 
                                                  CentralAir + Electrical + FireplaceQu + GarageArea + GarageQual + 
                                                  ScreenPorch + MoSold + YrSold + SaleType + SaleCondition + 
                                                  Fence_f + WoodDeckSF_group + OpenPorchSF_group + EnclosedPorch_group + 
                                                  GarageCars_f + pool_yn + porch_yn + TotalBath + BedroomAbvGr4plus + 
                                                  KitchenQual.Fac + KitchenQual.FacNum + TotRmsAbvGrd10plus + 
                                                  GarageTypeY + GarageCars3plus + GarageCond.Fac + GarageCond.N + 
                                                  PavedDrive.Fac + PavedDriveY + MSSubClass_group + LotFrontage_group + 
                                                  LotArea_group + YearBuilt.b + YearRemodAdd.b + RoofStyleY + 
                                                  MasVnrArea.b + logGrLivArea - logGrLivArea - GarageCars3plus - 
                                                  GarageCond.N - PavedDriveY - porch_yn), m=3, dots = 
        FALSE, seed=29, plotit=TRUE, printit=TRUE)

#-OR----
library(caret)

# Define train control for k fold cross validation, will take a long time to run
train_control <- trainControl(method="cv", number=5)
# Fit Naive Bayes Model
final_model <- train(logSalePrice~., data = training_model_no, trControl=train_control, method="nb")
# Summarise Results
print(final_model)


#Assumption Check on optimum model----
