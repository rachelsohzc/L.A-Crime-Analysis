library(readr)
library(dplyr)

crime2019 = read_csv("Crime_Data_from_2010_to_2019.csv")
dim(crime2019)

crime2021 = read_csv("Crime_Data_from_2020_to_Present.csv")
dim(crime2021)

attach(crime2019)

#Data cleaning
library(tidyverse)

#Checking for duplicate entries
crime2019 <- distinct(crime2019)
dim(crime2019)

detach()

attach(crime2021)
crime2021 <- distinct(crime2021)
dim(crime2021)

summary(crime2021)

#Merging the datasets
crime <- rbind(crime2019, crime2021)
nrow(crime2019) + nrow(crime2021)
nrow(crime)

#Checking for null entries in the whole table
sum(is.na(crime))
#Checking for null entries in crime code
sum(is.na(crime$`Crm Cd`))

#Crime codes 3 and 4 column seems empty
nrow(crime) - sum(is.na(crime$`Crm Cd 3`))

#Remove Crm Cd 4 because this column is empty
nrow(crime) - sum(is.na(crime$`Crm Cd 4`))

crime <- select(crime, -`Crm Cd 4`)
ncol(crime)

#Exploratory analysis
library(ggplot2)
