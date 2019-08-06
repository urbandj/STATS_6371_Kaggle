install.packages(c("skimr","tangram","fastDummies",'olsrr','rlang'))
library(tidyverse) #general data wrangling tools
library(skimr) #summary stats
library(tangram) #has is.categorical() function, useful for creating tables
library(car) # Regression tools
library(fastDummies) # creates dummy variables
library('rlang')
library('olsrr')

#create training data object, please refer to this variable when making modificaitons to the dataset----
#'if reading from local source
#'library(readr)
#'train <- read_csv("C:/.../train.csv")
#'View(train)

training_data = train


#Jeff's Data----

training_data = training_data %>% 
  mutate(logSalePrice = log(SalePrice)) %>%
  mutate(GarageCars_f = as.factor(training_data$GarageCars)) %>%
  mutate(pool_yn = if_else(PoolArea == 0,0,1)) %>% #not good predictor, 7 cases with data
  mutate(porch_yn = if_else((`3SsnPorch` == 0 || EnclosedPorch == 0 || OpenPorchSF == 0 || ScreenPorch == 0),0,1)) %>% #if a home has any features related to a porch then porch_yn = 1
  mutate(Fence_f = factor(Fence, levels=c("NA", "GdPrv", "GdWo","MnPrv","MnWw")))

#WoodDeckSF Categorical Buckets from Quantitiative Data
group_WoodDeckSF <- function(WoodDeckSF){
  if (WoodDeckSF >= 0 & WoodDeckSF <= 100){
    return('0-100SF')
  }else if(WoodDeckSF > 100 & WoodDeckSF <= 200){
    return('100-200SF')
  }else if (WoodDeckSF > 200 & WoodDeckSF <= 300){
    return('200-300SF')
  }else if (WoodDeckSF > 300 & WoodDeckSF <=400){
    return('300-400SF')
  }else if (WoodDeckSF > 400){
    return('> 400SF')
  }
}
training_data$WoodDeckSF_group <- sapply(training_data$WoodDeckSF,group_WoodDeckSF)
training_data$WoodDeckSF_group <- as.factor(training_data$WoodDeckSF_group)

#OpenPorchSF Categorical Buckets from Quantitiative Data
group_OpenPorchSF <- function(OpenPorchSF){
  if (OpenPorchSF >= 0 & OpenPorchSF <= 50){
    return('0-50SF')
  }else if(OpenPorchSF > 50 & OpenPorchSF <= 100){
    return('50-100SF')
  }else if (OpenPorchSF > 100 & OpenPorchSF <= 150){
    return('100-150SF')
  }else if (OpenPorchSF > 150 & OpenPorchSF <=200){
    return('150-200SF')
  }else if (OpenPorchSF > 200){
    return('> 200SF')
  }
}
training_data$OpenPorchSF_group <- sapply(training_data$OpenPorchSF,group_OpenPorchSF)
training_data$OpenPorchSF_group <- as.factor(training_data$OpenPorchSF_group)

#EnclosedPorch Categorical Buckets from Quantitiative Data
group_EnclosedPorch<- function(EnclosedPorch){
  if (EnclosedPorch >= 0 & EnclosedPorch <= 50){
    return('0-50SF')
  }else if(EnclosedPorch > 50 & EnclosedPorch <= 100){
    return('50-100SF')
  }else if (EnclosedPorch > 100 & EnclosedPorch <= 150){
    return('100-150SF')
  }else if (EnclosedPorch > 150 & EnclosedPorch <=200){
    return('150-200SF')
  }else if (EnclosedPorch > 200){
    return('> 200SF')
  }
}
training_data$EnclosedPorch_group <- sapply(training_data$EnclosedPorch,group_EnclosedPorch)
training_data$EnclosedPorch_group <- as.factor(training_data$EnclosedPorch_group)

#24 Observations of 3SsnPorch, not good predictor

#MiscVal Categorical Buckets from Quantitiative Data
group_MiscVal <- function(MiscVal){
  if (MiscVal >= 0 & MiscVal <= 200){
    return('$0-$200')
  }else if(MiscVal > 200 & MiscVal <= 400){
    return('$200-$400')
  }else if (MiscVal > 400 & MiscVal <= 600){
    return('$400-$600')
  }else if (MiscVal > 600 & MiscVal <=800){
    return('$600-$800')
  }else if (MiscVal > 800){
    return('> $800')
  }
}
training_data$MiscVal_group <- sapply(training_data$MiscVal,group_MiscVal)
training_data$MiscVal_group <- as.factor(training_data$MiscVal_group)

#Convert NA Fence to "None"
#convert MiscVale to categorical

#'pool not good predictor, 7 cases with data
#'MiscFeature not good predictor
#'Interaction variables for porch
#' porch_yn*3SsnPorch
#' porch_yn*EnclosedPorch
#' porch_yn*OpenPorchSF
#' porch_yn*ScreenPorch



#Adam's Data----

#Reannan's Data----
training_data_rea= training_data %>%select(1:21)
training_data_rea %>% skim

#training_data_rea$LotFrontage, create categorical categories: none, 50, 100, 150, 200+
#Ally is poor predictor

#David's Data----

