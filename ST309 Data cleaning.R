library(readr)
library(dplyr)
library(ggplot2)
library(DataCombine)

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

par(mfrow=c(1,2))
plot(crime2019d, main="Density of crime severity for 2010-2019")
plot(crimed, main="Density of crime severity for 2019-2020")

#Renaming all the columns
names(crime) <- c('RecNo','ReportDate','DateOCC','TimeOCC','Area','AreaName','DistrictNo','Part','CrimeCode','CrmDesc','Mocodes','VictAge','VictSex','VictRace','PremiseCd','PremiseDesc','WeaponCd','WeaponDesc','Status','StatusDesc','CrimeCd1','CrimeCd2','CrimeCd3','Location','CrossStreet','Lat','Lon')

attach(crime)

#Checking for null entries in our predictors
sum(is.na(crime))

sum(is.na(Area))
sum(is.na(RecNo))
sum(is.na(CrimeCode))
sum(is.na(DistrictNo))
sum(is.na(Mocodes))
sum(is.na(VictAge))
sum(is.na(VictSex))
sum(is.na(VictRace))
sum(is.na(PremiseCd))
sum(is.na(WeaponCd))
#Mocodes, VictSex, VictRace, PremiseCd, WeaponCd have null entries

#Replacing values into empty cells

#Crime codes 3 and 4 column seems empty
nrow(crime) - sum(is.na(CrimeCd3))

#Remove Crm Cd 4 because this column is empty
nrow(crime) - sum(is.na(CrimeCd4))

crime <- select(crime, -CrimeCd4)
ncol(crime)

#Which other columns should be removed?

#Transforming CrimeCode column

#Transforming Victim Sex and Victim race columns 
crime$VictSex <- as.factor(crime$VictSex)
class(crime$VictSex)

class(crime$VictRace)

?FindReplace
crime <- FindReplace(data=crime,Var=crime$VictSex,replaceData=Replaces,from=c("M","F","X"),to=c("0","1","2"))
crime <- FindReplace(data=crime,Var=crime$VictRace,replaceData=Replaces,from=c("A","B","C","D","F","G","H","I","J","K","L","O","P","S","U","V","W","X","Z"),to=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19"))

#Exploratory analysis - can't plot correlation graph
#Null value problem, enter 0 in null cells
quantitative <- select(crime, RecNo,TimeOCC,Area,DistrictNo,CrimeCode,Mocodes,VictAge,PremiseCd,CrimeCd1,CrimeCd2,CrimeCd3)
pairs(quantitative[,1:5])

#Modelling predictors
