#install and load in packages
install.packages("latticeExtra")
library(tidyverse)
library(rgdal)
library(dplyr)
library(sp)
library(latticeExtra)
library(spdep)
library(spatialreg)
library(car)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
setwd("D:/GGR376/A2") # sets workspace

#reads geojson file 

rgdal::readOGR("https://raw.githubusercontent.com/gisUTM/GGR376/master/Lab_1/houseValues.geojson") 

#set geojson file as a data frame so it can be joined with CSV 

house_price <- as.data.frame(rgdal::readOGR("https://raw.githubusercontent.com/gisUTM/GGR376/master/Lab_1/houseValues.geojson")) 

#read and assigns attribute values, guess_max = 9 ensures cols are double 

house_attribute <- read_csv("h_attribute.csv", guess_max = 9) 

#joining house price and attribute by CTUID, omit any NA rows  

value_attribute <- na.omit(inner_join(house_price, house_attribute, by = "CTUID")) 

#check data for any zero values, where they should have >0 

summary(value_attribute) 

#Graphical analysis Pre-check, in order of appearance, Scatter plot, boxplot, histogram
#Population density
ggplot(data = value_attribute, mapping = aes(PopDensity, houseValue))+ 
  geom_point()+ 
  geom_smooth(method = "lm", se = F) 

boxplot(value_attribute$PopDensity)

hist(value_attribute$PopDensity)

#mean age
ggplot(data = value_attribute, mapping = aes(meanAge, houseValue))+ 
  geom_point()+ 
  geom_smooth(method = "lm", se = F)

boxplot(value_attribute$meanAge)

hist(value_attribute$meanAge)

#mean household size
ggplot(data = value_attribute, mapping = aes(meanHouseholdSize, houseValue))+ 
  geom_point()+ 
  geom_smooth(method = "lm", se = F)

boxplot(value_attribute$meanHouseholdSize)

hist(value_attribute$meanHouseholdSize)

#mean income
ggplot(data = value_attribute, mapping = aes(meanIncome, houseValue))+ 
  geom_point()+ 
  geom_smooth(method = "lm", se = F)

boxplot(value_attribute$meanIncome)

hist(value_attribute$meanIncome)

#mean number of rooms
ggplot(data = value_attribute, mapping = aes(meanRooms, houseValue))+ 
  geom_point()+ 
  geom_smooth(method = "lm", se = F)

boxplot(value_attribute$meanRooms)

hist(value_attribute$meanRooms)

#housing suitability
ggplot(data = value_attribute, mapping = aes(pSuitability, houseValue))+ 
  geom_point()+ 
  geom_smooth(method = "lm", se = F)

boxplot(value_attribute$pSuitability)

hist(value_attribute$pSuitability)

#proportion of semi detached houses
ggplot(data = value_attribute, mapping = aes(pDetached, houseValue))+ 
  geom_point()+ 
  geom_smooth(method = "lm", se = F)

boxplot(value_attribute$pDetached)

hist(value_attribute$pDetached)

#proportion of houses that do not need major repairs
ggplot(data = value_attribute, mapping = aes(pGoodCondition, houseValue))+ 
  geom_point()+ 
  geom_smooth(method = "lm", se = F)

boxplot(value_attribute$pGoodCondition)

hist(value_attribute$pGoodCondition)

#proportion of employed people
ggplot(data = value_attribute, mapping = aes(pEmployment, houseValue))+ 
  geom_point()+ 
  geom_smooth(method = "lm", se = F)

boxplot(value_attribute$pEmployment)

hist(value_attribute$pEmployment)

#proportion of houses built between 1991 to 2016
ggplot(data = value_attribute, mapping = aes(pBuilt1991, houseValue))+ 
  geom_point()+ 
  geom_smooth(method = "lm", se = F)

boxplot(value_attribute$pBuilt1991)

hist(value_attribute$pBuilt1991)

#shapiro test to test for normality 
value_attribute%>%
  select(-CTUID)%>%
  map(shapiro.test) 

#Data Transformation
#values that do not pass shapiro test will be transformed
value_trans <- value_attribute%>%
  mutate(logValue = log(houseValue+1))%>%
  mutate(logDensity = log(PopDensity+1))%>%
  mutate(logIncome = log(meanIncome))%>%
  mutate(logRoom = log(meanRooms+1))%>%
  mutate(logSuit = log(pSuitability+1))%>%
  mutate(logDetach = log(pDetached+1))%>%
  mutate(logCondition = log(pGoodCondition+1))%>%
  mutate(logBu1991 = log(pBuilt1991+1))%>%
  select(-houseValue, -PopDensity, -meanIncome, -meanRooms, -pSuitability, -pDetached, -pGoodCondition, -pBuilt1991)

#Histograms of the dependent variable 
ggplot(value_trans)+
  geom_histogram(mapping = aes(x = logValue))+
  labs(title = "Histogram of log House Price", x = "log House Price", y = "Count")

#Correlation matrix
cor_mat <- cor(value_trans%>%
  select(-CTUID))
cor_mat

#plots correlation matrix
corrplot::corrplot(cor_mat, cl.pos = "b", tl.pos = "d")

#removes scientific notation
options(scipen=999)

#Model fitting #1
model_1 <- lm(logValue ~ logCondition, data = value_trans)
model_1
summary(model_1)

#checking mean residual 
mean(model_1$residuals)

#checking the homoscedasticity of residuals or equal variance
par(mfrow=c(2,2)) 
plot(model_1)

#checking for correlation between IV and residuals
cor.test(model_1$residuals, value_trans$logCondition)

#checking normality of residuals
hist(model_1$residuals)

#model 2 fitting

model_2 <- lm(logValue ~ logCondition + logRoom, data = value_trans)
summary(model_2)

#checking mean residuals
mean(model_2$residuals)

#checking the homoscedasticity of residuals
par(mfrow=c(2,2)) 
plot(model_2)

#checking multicollinearity of variables
vif(model_2)

#checking correlation between IV and residuals
cor.test(model_2$residuals, value_trans$logRoom)

#normality of residuals
hist(model_2$residuals)

#model 3 fitting
model_3 <- lm(logValue ~ logCondition + logRoom + logBu1991, data = value_trans)
summary(model_3)

#checking mean residuals
mean(model_3$residuals)

#checking the homoscedasticity of residuals
par(mfrow=c(2,2)) 
plot(model_3)

#checking multicollinearity of variables
vif(model_3)

#checking correlation between IV and residuals
cor.test(model_3$residuals, value_trans$logBu1991)

#normality of residuals
hist(model_3$residuals)

#model 4 fitting
model_4 <- lm(logValue ~ logCondition +logRoom + logBu1991 + logDetach, data = value_trans)
summary(model_4)

#checking mean residuals
mean(model_4$residuals)

#checking the homoscedasticity of residuals
par(mfrow=c(2,2)) 
plot(model_4)

#checking multicollinearity of variables
vif(model_4) #vif > 4 for logRoom

#model 5 fitting
model_5 <- lm(logValue ~ logCondition+ logBu1991 + logDetach+ logIncome, data = value_trans)
summary(model_5)

#checking mean residuals
mean(model_5$residuals)

#checking the homoscedasticity of residuals
par(mfrow=c(2,2)) 
plot(model_5)

#checking multicollinearity of variables
vif(model_5)

#checking correlation between IV and residuals
cor.test(model_5$residuals, value_trans$logIncome)

#normality of residuals
hist(model_5$residuals)

#model 6 fitting
model_6 <- lm(logValue ~ logCondition + logBu1991 + logDetach + logIncome + logSuit, data = value_trans)
summary(model_6) #no increase in R^2

#model 7 fitting
model_7 <- lm(logValue ~ logCondition + logBu1991 + logDetach + logIncome + pEmployment, data = value_trans)
summary(model_7) #decrease in R^2

#model 8 fitting
model_8 <- lm(logValue ~ logCondition + logBu1991 + logDetach + logIncome + meanHouseholdSize, data = value_trans)
summary(model_8)

#checking mean residuals
mean(model_8$residuals)

#checking the homoscedasticity of residuals
par(mfrow=c(2,2)) 
plot(model_8)

#checking multicollinearity of variables
vif(model_8)

#checking correlation between IV and residuals
cor.test(model_8$residuals, value_trans$meanHouseholdSize)

#normality of residuals
hist(model_8$residuals)

#model 9 fitting
model_9 <- lm(logValue ~ logCondition + logBu1991 + logDetach + logIncome + meanHouseholdSize + meanAge, data = value_trans)
summary(model_9)

#checking mean residuals
mean(model_9$residuals)

#checking the homoscedasticity of residuals
par(mfrow=c(2,2)) 
plot(model_9)

#checking multicollinearity of variables
vif(model_9) #vif > 4

#model 10 fitting
model_10 <- lm(logValue ~ logCondition + logBu1991 + logDetach + logIncome + meanHouseholdSize + logDensity, data = value_trans)
summary(model_10) #decrease in R^2


#renaming model_8 to final_model to test for spatial autocorrelation
final_model <- model_8

#reading in Hamilton boundary geometry in order to check for auto-correlation of residuals 
hamilton_polygons <- rgdal::readOGR("https://raw.githubusercontent.com/gisUTM/GGR376/master/Lab_1/houseValues.geojson") 

#combining model residuals and model input data into new data frame
final_model_df <- cbind(value_trans, residuals = final_model$residuals)
final_model_df

#joining dataframe to spatial data
hamilton_spdf <- merge(hamilton_polygons, final_model_df, by.x = "CTUID", all.x = FALSE)
spplot(hamilton_spdf, "residuals")

#creating a list of ploygons that share borders
hamilton_nb <- poly2nb(hamilton_spdf)

#Convert Neigbours List to Spatial Weights
hamilton_listw <- nb2listw(hamilton_nb)

#assigns which colours to use to a colour palette
col_palette <- brewer.pal(n = 7, name = "BrBG")

#assessing spatial auto-correlation with map
spplot(hamilton_spdf, "residuals",
       col.regions = col_palette, # The colour palette 
       cuts = 6, # Cuts is the number of colours - 1
       col = "black") # This sets the border colour

#conduct Moran's I calculation to assess spatial auto-correlation
moran.test(hamilton_spdf$residuals, hamilton_listw)
#Monte-Carlo version of Moran's I
moran.mc(hamilton_spdf$residuals, hamilton_listw, 999)

#creating a spatial lag model 
lag_model = lagsarlm(logValue ~ logCondition + logBu1991 + logDetach + logIncome + meanHouseholdSize,
                     data = hamilton_spdf,
                     listw = hamilton_listw)
summary(lag_model)

#adding residuals from lag model to SpatialPolygonDataFrame
hamilton_spdf$lagResids <- residuals(lag_model)
moran.mc(hamilton_spdf$lagResids, hamilton_listw, 999)

#spatial error model
error_model = errorsarlm(logValue ~ logCondition + logBu1991 + logDetach + logIncome + meanHouseholdSize,
                         data = hamilton_spdf,
                         listw = hamilton_listw)
summary(error_model)

#adding residuals from error model to SpatialPolygonDataFrame
hamilton_spdf$errorResids <- residuals(error_model)
moran.mc(hamilton_spdf$errorResids, hamilton_listw, 999) #p-value of 0.541, retain null hypothesis

#Lagrange Multiplier diagnostics to select either error or lag model 
summary(lm.LMtests(final_model, hamilton_listw, test="all")) #p value dictates use of lag model

#assigning fitted values and residuals to variables
Fitted <- fitted.values(final_model)
Residual <- residuals(final_model)

#plotting residuals of the final model (Question 8)
ggplot(data = final_model, mapping = aes(x = Fit, y = Residual))+
  labs(title = "Residuals vs Fitted Values", subtitle = "Final Model")+
  xlab("Fitted Values")+
  ylab("Residuals")+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

#map of residuals from lag model
spplot(hamilton_spdf, "lagResids",
       col.regions = col_palette, # The colour palette
       cuts = 6, # Cuts is the number of colours - 1
       col = "black",
       arrow,
       main = "Spatial Autoregressive Model of Residuals",
       sub = "Hamilton Census Tract")