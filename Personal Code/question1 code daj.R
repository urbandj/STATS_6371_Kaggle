install.packages(c("skimr","tangram"))
library(tidyverse)
library(skimr)
library(tangram)

#direct the obeject named train to your taining data file 
train<- read.csv("c:/Users/daj0079/Desktop/SMU/train.csv")


## checking for NAs in each of the variables of interest
sum(is.na(train$Neighborhood))
sum(is.na(train$GrLivArea))
sum(is.na(train$SalePrice))

## estabish the house counts per neighborhood from orginal data
table(train$Neighborhood)


#Brkside 58 + Edwards 100 + NAmes 225
58+100+225

## select the columns required for Question 1 and confirm data
train%>%select(Id,GrLivArea, Neighborhood,SalePrice)->quest1
table(quest1$Neighborhood)
str(quest1)

## select and confirm only the neighborhoods of interest from the quest1 dataframe
quest1 %>% filter(Neighborhood  %in% c('NAmes','Edwards','BrkSide'))->NBH
table(NBH$logGrLivArea)
sum(complete.cases(NBH))


# generate the log for each of the contuious data columns SEE THE LOG per 100sq ft
NBH$GrLivArea
per100sqft <-(NBH$GrLivArea)/100
per100sqft
logper100sqft <- log(per100sqft)
logper100sqft
logGrLivArea <-log(NBH$GrLivArea)
logPrice <- log(NBH$SalePrice)
logPrice
# join the logged tranformed to the NBH dataframe and confirm df
LogNBH<-cbind(logGrLivArea,logPrice)
LogNBH
NBH <- cbind(NBH, LogNBH)
str(NBH)
NBH <-cbind(NBH, logper100sqft)

#Check normality asumptiona and variance 
NBH %>% skim
str(NBH)


ggplot(data = NBH, aes(x=Neighborhood, y=SalePrice, color=GrLivArea))+geom_point(stat ="identity")
ggplot(data = NBH, aes(x=Neighborhood, y=GrLivArea, color=SalePrice))+geom_point(stat ="identity")
ggplot(data = NBH, aes(x=GrLivArea, y=SalePrice, color=Neighborhood))+geom_point(stat ="identity")
ggplot(data = NBH, aes(x=Neighborhood, y=logGrLivArea, color=SalePrice))+geom_point(stat ="identity")
ggplot(data = NBH, aes(x=Neighborhood, y=logPrice, color=SalePrice))+geom_point(stat ="identity")

# planning to use the logGrlivArea and LogPrice  Need to confirm the data seen as outliers
ggplot(data = NBH, aes(x=logGrLivArea, y=logPrice, color=Neighborhood))+geom_point(stat ="identity")

# regession model with interaction variable
test_sale_price<-lm( data = NBH, logPrice~logper100sqft + Neighborhood + logGrLivArea*Neighborhood )
summary(test_sale_price)

# graphs to address assumptions

plot(test_sale_price, which=1)
plot(test_sale_price, which=2)
plot(test_sale_price, which=3)
plot(test_sale_price, which=4)



#tired to filter based on ranges but did not see any improvemenets  GO ahead an adjust the ranges   you can visualize 
#with plot and run the lm modle

FilterNBH <- NBH %>% filter(logGrLivArea >= 6.5 & logGrLivArea <= 8  & logPrice >11)


ggplot(data = FilterNBH, aes(x=logGrLivArea, y=logPrice, color=Neighborhood))+geom_point(stat ="identity")
str(FilterNBH)

filter_price<-lm( data = FilterNBH, logPrice~logGrLivArea + Neighborhood + logGrLivArea*Neighborhood )
summary(filter_price)

# Indentifed outliers ,190,104,339,131,



# remove outlines @ # squences   AND found our highest r^2 I used the residual plots above to determine
# the IDs of the outliers
testNBH<-NBH[-c(104,130, 186,190,339), ]
str(testNBH)
test_sale_price2<-lm( data = testNBH, logPrice~logper100sqft + Neighborhood + logper100sqft*Neighborhood )
summary(test_sale_price2)
confint(test_sale_price2, level=0.99)
qt(0.995, 387-2)


#plot with outliers removed.  I think if we cut off the point near past logper100st=3.5  
#i believe the r goes down

ggplot(data = testNBH, aes(x=logper100sqft, y=logPrice, color=Neighborhood))+geom_point(stat ="identity")


# Veiw residual plots 
#additional outliers can be seen but I think I tried to remove them and did not find improvement.  
#I was also concerned about over fitting
plot(test_sale_price2, which=1)
plot(test_sale_price2, which=2)
plot(test_sale_price2, which=3)
plot(test_sale_price2, which=4)

##Store studentized residuals
studresdisplay <- rstudent(test_sale_price2)

##Histogram
hist(studresdisplay, freq=FALSE, main="Distribution of Studentized Residuals", 
     xlab="Studentized Residuals", ylab="Density", ylim=c(0,0.5))

##Create range of x-values for normal curve
xfit <- seq(min(studresdisplay), max(studresdisplay), length=40)

##Generate values from the normal distribution at the specified values
yfit <- (dnorm(xfit))

##Add the normal curve
lines(xfit, yfit, ylim=c(0,0.5))
