library(readr)
library(dplyr)

crime2019 = read_csv("Crime_Data_from_2010_to_2019.csv")
dim(crime)

crime2021 = read_csv("Crime_Data_from_2020_to_Present")

attach(crime2019)

#Exploratory analysis

#Data cleaning
install.packages(tidyverse)
library(tidyverse)

#Checking for duplicate entries
crime2019 <- distinct(crime2019)
dim(crime2019)

#Checking for null entries in crime code
sum(is.na(crime$`Crm Cd`))
nrow(crime) - sum(is.na(crime$`Crm Cd 3`))

#Remove Crm Cd 4 because this column is empty
nrow(crime) - sum(is.na(crime$`Crm Cd 4`))

crime <- select(crime, -`Crm Cd 4`)
ncol(crime)
