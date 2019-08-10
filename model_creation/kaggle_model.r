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
  dplyr::select(1:3,9,13:19,21,23:32,34:38,40:43,58,63,64,71,77:80,82:99,101:109,122,124:126,131)

training_model$GarageCars3plus = factor(training_model$GarageCars3plus, levels = c("1","2","3","4"), ordered = TRUE)
training_model$FireplaceY = factor(training_model$FireplaceY, levels = c(0,1), ordered = TRUE)

training_data$TotalBsmtSF.b %na<-% 0
training_data$TotalBsmtSF.b<-cut(training_data$TotalBsmtSF.b, c(0,2,4,6,8,14), ordered_result = TRUE, include.lowest = TRUE)
max(training_data$TotalBsmtSF.b)

training_modeldv = dummy_cols(training_model,remove_original = TRUE)
# .n variables: 1:3,6,8:19,44:46,48:51,53,56,58,60,61,63:66,71:73,77:80,82:130
names(training_data)
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
initial_model = lm(logSalePrice~.- logGrLivArea- GarageArea-OverallQual-TotalBath-GarageCars.Fac-FireplaceY-PavedDriveY-porch_yn  ,data = training_model)#once lines 22&23 are completed ~. inputs all variables into model,interaction terms can be added via "+ term1*term2"
summary(initial_model)
vif(initial_model)

#Outlier Check----
plot(initial_model)
ols_plot_cooksd_bar(initial_model)


#Outlier Removal----
training_model_no <- training_model[-c(524,633,826,1325),] 

#Model outliers addressed----
new_model = lm(logSalePrice~., data = training_model_no)#interaction terms will need to be added here
summary(new_model)
vif(new_model)

step(new_model)

newmodel = glm(logSalePrice ~ Id + MSSubClass + MSZoning + LotShape + 
  Utilities + LotConfig + Neighborhood + Condition1 + BldgType + 
  HouseStyle + OverallCond + RoofMatl + Exterior1st + MasVnrType + 
  ExterCond + Foundation + BsmtQual + BsmtExposure + BsmtFinType1 + 
  BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + Heating + 
  HeatingQC + CentralAir + BsmtFullBath + FullBath + HalfBath + 
  KitchenAbvGr + KitchenQual + Functional + FireplaceQu + GarageFinish + 
  GarageArea + GarageQual + ScreenPorch + MoSold + SaleType + 
  SaleCondition + GarageCars_f + pool_yn + WoodDeckSF_group + 
  EnclosedPorch_group + TotRmsAbvGrd10plus + GarageTypeY + 
  GarageCond.Fac + PavedDrive.Fac + MasVnrArea.b, data = training_model_no)

summary(newmodel)
vif(newmodel)

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

cv.lm(df = training_model_no, form.lm = newmodel,m=3, seed=29)

newmodel = lm(logSalePrice ~ Id + MSSubClass + MSZoning + LotShape + 
                Utilities + LotConfig + Neighborhood + Condition1 + BldgType + 
                HouseStyle + OverallCond + RoofMatl + Exterior1st + MasVnrType + 
                ExterCond + Foundation + BsmtQual + BsmtExposure + BsmtFinType1 + 
                BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + Heating + 
                HeatingQC + CentralAir + BsmtFullBath + FullBath + HalfBath + 
                KitchenAbvGr + KitchenQual + Functional + FireplaceQu + GarageFinish + 
                GarageArea + GarageQual + ScreenPorch + MoSold + SaleType + 
                SaleCondition + GarageCars_f + pool_yn + WoodDeckSF_group + 
                EnclosedPorch_group + TotRmsAbvGrd10plus + GarageTypeY + 
                GarageCond.Fac + PavedDrive.Fac + MasVnrArea.b, data = training_model_no)

summary(newmodel)
vif(newmodel)
  
  #-OR----
library(caret)

# Define train control for k fold cross validation, will take a long time to run
train_control <- trainControl(method="cv", number=5)
# Fit Naive Bayes Model
final_model <- train(logSalePrice~., data = training_model_no, trControl=train_control, method="nb")
# Summarise Results
print(final_model)


#Assumption Check on optimum model----
