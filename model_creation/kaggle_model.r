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
install.packages('stats')
drop<- c("Exterior2nd","BsmtFinSF2","ExterCond","GrLivArea", "logYearBuilt","GarageCars", "GarageCond","GarageQual", "porch_yn", "FireplaceY","GarageTypeY",
         "porchArea", "BsmtFinType1", "OpenPorchSF_y","TotalBath","logGrLivArea", "logSalePrice","totalSF","TotalBsmtSF","ExterQual","pool_yn",
         "PoolArea","WoodDeckSF","LotArea","X2ndFlrSF","Utilities","CentralAir")

full_training = ames_train[,!(names(ames_train) %in% drop)]

CV_test = ames_test[,!(names(ames_test) %in% drop)]
ptest = Test_Set[,!(names(Test_Set) %in% drop)]
as_tibble(ptest)
#Linearity Check----

  training_corr = full_training %>% 
  dplyr::select(       
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
                 
                LowQualFinSF,    
                MasVnrArea,         
                MoSold,      
                OpenPorchSF,        
                OverallQual,          
                SalePrice,           
                   
                TotRmsAbvGrd,      
                TotRmsAbvGrd10plus,     
                  
                YearBuilt,     
                YrSold  )#select all quantitiative variables here

training_corr%>% skim
sum(is.na(full_training))

corr_object = cor(training_corr)
corrplot(corr_object, method = "number")#use this in write-up



#Initial Model----
options(max.print=999999)
full.model <- lm(SalePrice ~., data = full_training)
summary(full.model)#9102
vif(full.model)

summary(aov(full.model))


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

#full.model2 <- lm(logSalePrice ~., data = full_training_noVIF)
#summary(full.model2) #R^2 = 0.9252
#vif(full.model2)

#Outlier Check----
plot(full.model)

ols_plot_cooksd_bar(full.model)

#Outlier Removal----
full_training_no <- full_training[-c(617,883,389,514,256,1035),] 


#Model outliers addressed----
new_model = lm(SalePrice~., data = full_training_no)#interaction terms will need to be added here
summary(new_model) #R^2 = 0.925
vif(new_model)


#Selection + Custom

#Step


stepAIC(new_model, direction = "both")
stepmodel = lm(SalePrice ~ MSSubClass + LotFrontage + Street + 
                 LotConfig + Neighborhood + BldgType + 
                 OverallQual + OverallCond + YearBuilt + RoofMatl + Exterior1st + 
                 MasVnrType + MasVnrArea + BsmtQual + BsmtExposure + BsmtFinSF1 + 
                 BsmtFinType2 + BsmtUnfSF + Heating + `1stFlrSF` + `2ndFlrSF` + 
                 BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + KitchenAbvGr + 
                 KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + GarageType + 
                 GarageYrBlt + GarageArea + OpenPorchSF + ScreenPorch + MiscVal + 
                 SaleType + SaleCondition + WoodDeckSF_group + OpenPorchSF_group + 
                 BedroomAbvGr4plus + TotRmsAbvGrd10plus, data = ptest) 
summary(stepmodel)#r^2=0.9264
confint(stepmodel)

skim(ames_test)
#Forwards

stepAIC(new_model, direction = "forward")
forwards = lm(formula = SalePrice ~ Id + MSSubClass + MSZoning + LotFrontage + 
                Street + LotShape + LandContour + LotConfig + LandSlope + 
                Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + 
                OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + 
                RoofMatl + Exterior1st + MasVnrType + MasVnrArea + Foundation + 
                BsmtQual + BsmtCond + BsmtExposure + BsmtFinSF1 + BsmtFinType2 + 
                BsmtUnfSF + Heating + HeatingQC + Electrical + `1stFlrSF` + 
                `2ndFlrSF` + LowQualFinSF + BsmtFullBath + BsmtHalfBath + 
                FullBath + HalfBath + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + 
                Functional + Fireplaces + FireplaceQu + GarageType + GarageYrBlt + 
                GarageFinish + GarageArea + PavedDrive + OpenPorchSF + EnclosedPorch + 
                X3SsnPorch + ScreenPorch + Fence + MiscVal + MoSold + YrSold + 
                SaleType + SaleCondition + X3SsnPorch_y + EnclosedPorch_y + 
                ScreenPorch_y + WoodDeckSF_group + OpenPorchSF_group + EnclosedPorch_group + 
                BedroomAbvGr4plus + TotRmsAbvGrd10plus, data = ptest)
  
  
summary(forwards)#r^2=0.925
confint(forwards)

#Backwards
stepAIC(new_model, direction = "backward", 
                        trace = FALSE)
b.step.model <- lm(SalePrice ~ MSSubClass + LotFrontage + Street + 
  LotConfig + Neighborhood + Condition1 + Condition2 + BldgType + 
  OverallQual + OverallCond + YearBuilt + RoofMatl + Exterior1st + 
  MasVnrType + MasVnrArea + BsmtQual + BsmtExposure + BsmtFinSF1 + 
  BsmtFinType2 + BsmtUnfSF + Heating + `1stFlrSF` + `2ndFlrSF` + 
  BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + KitchenAbvGr + 
  KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + GarageType + 
  GarageYrBlt + GarageArea + OpenPorchSF + ScreenPorch + MiscVal + 
  SaleType + SaleCondition + WoodDeckSF_group + OpenPorchSF_group + 
  BedroomAbvGr4plus + TotRmsAbvGrd10plus, data = ptest)
summary(b.step.model)#r^2=0.9264
confint(b.step.model)

#Custom
custom = lm(SalePrice ~ MSSubClass + LotFrontage + Street + 
                LotConfig + Neighborhood +
               BldgType + OverallQual + OverallCond + YearBuilt + RoofMatl + 
               Exterior1st + MasVnrType + MasVnrArea + BsmtQual + BsmtExposure + 
               BsmtFinSF1 + BsmtFinType2 + BsmtUnfSF + Heating + `1stFlrSF` + 
               `2ndFlrSF` + BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + 
               KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + GarageType + 
               GarageYrBlt + GarageArea + OpenPorchSF + ScreenPorch + MiscVal + 
               SaleType + SaleCondition + WoodDeckSF_group + OpenPorchSF_group + 
               BedroomAbvGr4plus + TotRmsAbvGrd10plus -Exterior1st,  data = ptest)
summary(custom)#r^2=0.9267
vif(stepmodel)
confint(custom)

#Assumption Check New Model----
require(c('lindia','ggplot2','gridExtra'))

assumptionPlots(full.model,full_training)
assumptionPlots(stepmodel,full_training_no)
assumptionPlots(forwards,full_training_no)
assumptionPlots(b.step.model,full_training_no)
assumptionPlots(custom,full_training_no)

predictmodel = predict(stepmodel)
  # Histogram overlaid with kernel density curve
gg_reshist(custom)
                                     
                                      colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Distribution of Residuals") +
  theme_bw()

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

#Model selection assupmtion checks----


#model selection----
#http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/

levels(ames_test$Condition2)
skim(ames_test)
#Cross Validation----
#Generate prediction

  stepmodelpredict = predict(custom,interval = "predict",newdata = CV_test)
  as.data.frame(stepmodelpredict)
  
  #Find MSPE and check
  MSPE = data.frame(Observed = CV_test$SalePrice, Predicted = stepmodelpredict[,1])
  MSPE$Observed=as.numeric(as.character(MSPE$Observed))
  MSPE$Residual = MSPE$Observed - MSPE$Predicted
  MSPE$SqauaredResidual = MSPE$Residual^2
  print("MSPE is: ")
  mean(MSPE$SqauaredResidual)

cv(stepmodel,ames_test)
cv(forwards,ames_test)
cv(b.step.model,ames_test)
cv(custom,ames_test)

cv.lm(df = full_training_no,stepmodel, m =3)

#Assumption Check on optimum model----
ptest = as_tibble(ptest)
ptest=ptest%>% mutate(SalePrice = if_else(SalePrice =="none",NA,0))



SalePriceSW = predict(stepmodel,newdata = ptest )
sw_kaggle = cbind(ptest,SalePriceSW)
sw_kaggle = ptest %>% dplyr::select(Id, SalePrice) %>%
  mutate(SalePrice = SalePriceSW )

cbind(ptest,SalePricep)
custom_kaggle = ptest %>% dplyr::select(Id, SalePrice) %>%
  mutate(SalePrice = SalePricep )

SalePriceb= predict(b.step.model,newdata = ptest)
back_kaggle = cbind(ptest,SalePriceb)
back_kaggle = ptest %>% dplyr::select(Id, SalePrice) %>%
  mutate(SalePrice = SalePricef )

write.csv(custom_kaggle, file = "custom_kaggle.csv")
write.csv(sw, file = "sw_kaggle.csv")



ptest = ptest %>% 
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
  mutate(SalePrice = replace(SalePrice, SalePrice == "none",0)) %>%
  mutate(SalePrice = as.numeric(SalePrice)) %>%
  mutate(MSZoning = replace(MSZoning, MSZoning == "NONE","RL")) %>%
  mutate(Condition2 = replace(MSZoning, MSZoning == "Feedr","Norm")) %>%
  mutate(Condition2 = replace(MSZoning, MSZoning == "PosA","RRAn")) %>%
  mutate(BsmtFullBath = if(BsmtFullBath==3) {BsmtFullBath == 2}) %>%
  mutate(BsmtHalfBath = if(BsmtHalfBath==2){BsmtHalfBath == 1})
  


skim(ptest)

table(ptest$BsmtHalfBath)
table(ames_test$BsmtHalfBath)
