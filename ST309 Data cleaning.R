library(readr)
library(dplyr)
library(ggplot2)

crime2019 = read_csv("Crime_Data_from_2010_to_2019.csv")
dim(crime2019)

crime = read_csv("Crime_Data_from_2020_to_Present.csv")
dim(crime)

#Data cleaning
library(tidyverse)

#Checking for duplicate entries
crime2019 <- distinct(crime2019)
dim(crime2019)

crime2019$AREA <- as.numeric(crime2019$AREA)

crime <- distinct(crime)
dim(crime)

crime$AREA <- as.numeric(crime$AREA)
class(crime$AREA)

#Deciding whether to use only 2019-2020 data, proportions of crime severity is roughly the same
summary(crime2019$`Crm Cd`)
summary(crime$`Crm Cd`)

crime2019d <- density(crime2019$`Crm Cd`)
crimed <- density(crime$`Crm Cd`)

library(ggplot2)

par(mfrow=c(1,2))
plot(crime2019d, main="Density of crime severity for 2010-2019")
plot(crimed, main="Density of crime severity for 2019-2020")

#Checking for null entries in the whole table
sum(is.na(crime))
#Checking for null entries in crime code
sum(is.na(crime$`Crm Cd`))

#Crime codes 3 and 4 column seems empty
nrow(crime) - sum(is.na(crime$`Crm Cd 3`))

#Which other columns should be removed?
#Remove Crm Cd 4 because this column is empty
nrow(crime) - sum(is.na(crime$`Crm Cd 4`))

crime <- select(crime, -`Crm Cd 4`)
ncol(crime)

#Exploratory analysis - can't plot correlation graph
#Null value problem, enter 0 in null cells
quantitative <- sapply(crime$DR_NO,table(DR_NO,`TIME OCC`,AREA,`Rpt Dist No`,`Crm Cd`,Mocodes,`Vict Age`,`Premis Cd`,`Weapon Used Cd`,`Crm Cd 1`,`Crm Cd 2`,`Crm Cd 3`))
pairs(quantitative[,1:10])
