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
library(MASS)#studentized residuals
library(caret)

#Create Dataset object----
training_model = training_data %>%
  select()


#Linearity Check----

training_corr = training_model %>% 
  select()#select all quantitiative variables here

training_corr%>% skim

corr_object = cor(training_corr)
corrplot(corr_object, method = "number")#use this in write-up


#Initial Model----
initial_model = lm(logSalePrice~.,data = training_model)#once lines 22&23 are completed ~. inputs all variables into model,interaction terms can be added via "+ term1*term2"
summary(initial_model)
vif(initial_model)

#Assumption Check Initial Model (Use in Write up)----
#residual
res=resid(intial_model)
plot(training_model_no$"INSERT EXPLAINATORY HERE", res,
     ylab="Residuals", xlab="prices_linlog_nol", 
     main="prices_linlog_nol Residual Plot") 
abline(0, 0, col = 'red')    


#studentized resid
library(MASS)
sresid <- studres(intital_model)
plot(century21_loglog$logGrLivArea, sresid,
     ylab="Studentized Residuals", xlab="prices_linlog_nol", 
     main="prices_linlog_nol Studentized Residual Plot") 
abline(2, 0, col = 'red')    
abline(-2, 0, col = 'red')    


#resid hist
library(MASS)
resid <- studres(intial_model)
hist(resid, freq=FALSE,
     main="Distribution of Residuals")
xfit<-seq(min(resid),max(resid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit) 

#qqplot
qqnorm(res)
qqline(res, col = "steelblue", lwd = 2)


#Outlier Check----
plot(intial_model)
ols_plot_cooksd_bar(intial_model)


#Outlier Removal----
training_model_no = training_model#creates no-outlier dataset
training_model_no$index = seq.int(nrow(training_model_no)) #creates index to reference outliers
training_model_no = training_model %>%
  filter(index %notin% c())#removes outliers

#Model outliers addressed----

new_model = lm(logSalePrice~., data = training_model_no)#interaction terms will need to be added here
summary(new_model)
vif(mew_model)

#Assumption Check New Model----
#residual
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


#Cross Validation
#install.packages('forecast')
library(forecast)

cv(training_model_no)

final_model = 

#-OR----
library(caret)

# Define train control for k fold cross validation, will take a long time to run
train_control <- trainControl(method="cv", number=5)
# Fit Naive Bayes Model
final_model <- train(logSalePrice~., data = training_model_no, trControl=train_control, method="nb")
# Summarise Results
print(final_model)


#Assumption Check on optimum model----
