#Loading appropriate libraries
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapproj)
library(gridExtra) 

#Function to import data
#!!!WILL NEED TO BE REVISED!!!
test <- function(){
  datasetHousing <- read_csv("F:/Syracuse/IST-687/Final Project/Dataset/housing.csv/housing.csv")
  str(datasetHousing)
  return(datasetHousing)
}

#Importing the returned data into a dataframe
housingData <- test()

#Gives a small printout of the data we will be using moving forward
head(housingData)

#Counts all missing values for the dataframe by column
sapply(housingData, function(x) sum(is.na(x)))

#Cleaning the data and adding back new values for the previously NA values
housingData$total_bedrooms[is.na(housingData$total_bedrooms)]<-mean(housingData$total_bedrooms,na.rm=TRUE)

#Double checking that data has been cleaned
sapply(housingData, function(x) sum(is.na(x)))

#Creating a model to best identify median housing value and identify 
#statistically significant
housingModel <- lm(formula = median_house_value ~ latitude + longitude + 
                     population +total_rooms + housing_median_age + 
                     total_bedrooms +households + median_income + 
                     ocean_proximity, data = housingData)
summary(housingModel)

#Adjusted R^2: 0.6336
#Statistically Significant Values: All

#Graphing all the plots to see any linear relationships between the data.
plotPop <- ggplot(housingData, 
                  aes(x=population, y=median_house_value)) + 
  geom_point()
plotRooms <- ggplot(housingData, 
                    aes(x=total_rooms, y=median_house_value)) + 
  geom_point()
plotBedrooms <- ggplot(housingData, 
                    aes(x=total_bedrooms, y=median_house_value)) + 
  geom_point()
plotAge <- ggplot(housingData, 
                       aes(x=housing_median_age, y=median_house_value)) + 
  geom_point()
plotHouseholds <- ggplot(housingData, 
                       aes(x=households, y=median_house_value)) + 
  geom_point()
plotIncome <- ggplot(housingData, 
                       aes(x=median_income, y=median_house_value)) + 
  geom_point()
plotLat <- ggplot(housingData, 
                       aes(x=latitude, y=median_house_value)) + 
  geom_point()
plotLong <- ggplot(housingData, 
                       aes(x=longitude, y=median_house_value)) + 
  geom_point()
plotProx <- ggplot(housingData, 
                       aes(x=ocean_proximity, y=median_house_value)) + 
  geom_point()

grid.arrange(plotBedrooms, plotProx, plotLat, plotIncome, plotHouseholds, 
          plotAge, plotLong, plotRooms, plotPop, ncol=5)
