library(readr)
library(dplyr)
library(ggplot2)
library(DataCombine)
library(tidyverse)

crime2019 = read_csv("Crime_Data_from_2010_to_2019.csv")
dim(crime2019)

crime = read_csv("Crime_Data_from_2020_to_Present.csv")
dim(crime)

#Data cleaning
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
names(crime) <- c('RecNo','ReportDate','DateOCC','TimeOCC','Area','AreaName','DistrictNo','Part','CrimeCode','CrmDesc','Mocodes','VictAge','VictSex','VictRace','PremiseCd','PremiseDesc','WeaponCd','WeaponDesc','Status','StatusDesc','CrimeCd1','CrimeCd2','CrimeCd3','CrimeCd4','Location','CrossStreet','Lat','Lon')

#Checking for null entries in our predictors
sum(is.na(crime$DateOCC))
sum(is.na(crime$TimeOCC))
sum(is.na(crime$Area))
sum(is.na(crime$RecNo))
sum(is.na(crime$CrimeCode))
sum(is.na(crime$DistrictNo))
sum(is.na(crime$Mocodes))
sum(is.na(crime$VictAge))
sum(is.na(crime$VictSex))
sum(is.na(crime$VictRace))
sum(is.na(crime$PremiseCd))
sum(is.na(crime$WeaponCd))
#Mocodes, VictSex, VictRace, PremiseCd, WeaponCd have null entries

#Removing records with null values and illogical values
crime <- crime[!is.na(crime$Mocodes),]
crime <- crime[!is.na(crime$VictAge),]
crime <- crime[!is.na(crime$VictSex),]
crime <- crime[!is.na(crime$VictRace),]
crime <- crime[!is.na(crime$PremiseCd),]

#Replacing null values with 0 for WeaponCd to denote no weapon involved
crime$WeaponCd[is.na(crime$WeaponCd)] <- 0

sum(is.na(crime$Mocodes))
sum(is.na(crime$VictAge))
sum(is.na(crime$VictSex))
sum(is.na(crime$VictRace))
sum(is.na(crime$PremiseCd))
sum(is.na(crime$WeaponCd))

View(crime)

#Removing columns we do not need
attach(crime)
crime = subset(crime, select = -c(CrimeCd2,CrimeCd3,CrimeCd4))
crime = subset(crime, select = -c(ReportDate,CrossStreet,Location,StatusDesc,Status,WeaponDesc, PremiseDesc,PremiseCd,Mocodes, CrmDesc, Part, AreaName))
View(crime)

#Transforming columns - VictSex and VictRace columns
crime$VictSex <- as.factor(crime$VictSex)
class(crime$VictSex)

class(crime$VictRace)

?FindReplace
crime <- FindReplace(data=crime,Var=crime$VictSex,replaceData=Replaces,from=c("M","F","X"),to=c("0","1","2"))
crime <- FindReplace(data=crime,Var=crime$VictRace,replaceData=Replaces,from=c("A","B","C","D","F","G","H","I","J","K","L","O","P","S","U","V","W","X","Z"),to=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19"))

#Changing CrimeCode column

#Exploratory analysis - can't plot correlation graph
#quantitative <- select(crime, RecNo,TimeOCC,Area,DistrictNo,CrimeCode,Mocodes,VictAge,PremiseCd,CrimeCd1,CrimeCd2,CrimeCd3)
#pairs(quantitative[,1:5])

#Modelling predictors

