#Load packages

library(tidyverse)
library(pastecs)

#Load in the data file ipums_wage.csv as an object

IncomeData <- read_csv("D:/ECON 385/ipums_wage.csv")

#Create a Scatter Plot for Hourly wages in relation to Years of Education

ggplot (data = IncomeData, aes(y = hourwage, x = educ_yrs)) +
  
  geom_point() +
  
  labs(x = "Years of Education", y = "Hourly Wages")

#Incase a Scatter Plot with a regression line is desired

ggplot (data = IncomeData, aes(y = hourwage, x = educ_yrs)) +
  
  geom_point() +
  
  labs(x = "Years of Education", y = "Hourly Wages") +

  geom_smooth(method = "lm", se = FALSE)


#Get Descriptive Statistics on the Variables

options(digits = 2)
options(scipen = 100)
stat.desc(IncomeData)

#Equation with hourwage as the DV and educ_yrs as the IV and print a summary

LinearModel.lm <- lm(hourwage ~ educ_yrs, data = IncomeData)

summary(LinearModel.lm)

#Previous Equation including our dummy variable nonwhite

LinearModel2.lm <- lm(hourwage ~ educ_yrs + nonwhite, data = IncomeData)

summary(LinearModel2.lm)

#Create Interaction term and get Descriptive Statistics

IncomeData$female_badeg <- IncomeData$female * IncomeData$ba_degree
options(digits = 2)
stat.desc(IncomeData$female_badeg)

#Number 14

LinearModel3.lm <- lm(hourwage ~ female, data = IncomeData)
summary(LinearModel3.lm)

LinearModel4.lm <- lm(hourwage ~ female + ba_degree + female_badeg, data = IncomeData)
summary(LinearModel4.lm)

