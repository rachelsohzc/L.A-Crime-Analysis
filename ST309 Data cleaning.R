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
sum(is.na(crime$PremiseDesc))
sum(is.na(crime$WeaponCd))
#Mocodes, VictSex, VictRace, PremiseCd, WeaponCd have null entries

#Removing records with null values and illogical values
crime <- crime[!is.na(crime$Mocodes),]
crime <- crime[!is.na(crime$VictAge),]
crime <- crime[!is.na(crime$VictSex),]
crime <- crime[!is.na(crime$VictRace),]
crime <- crime[!is.na(crime$PremiseDesc),]
crime <- crime[!is.na(crime$PremiseCd),]

#Replacing null values with 0 for WeaponCd to denote no weapon involved
crime$WeaponCd[is.na(crime$WeaponCd)] <- 0

sum(is.na(crime$Mocodes))
sum(is.na(crime$VictAge))
sum(is.na(crime$VictSex))
sum(is.na(crime$VictRace))
sum(is.na(crime$PremiseCd))
sum(is.na(crime$WeaponCd))

dim(crime)

#Removing unknown records in VictSex and VictRace
crime <- crime[!(crime$VictSex=="X"),]
crime <- crime[!(crime$VictRace=="X"),]
dim(crime)

#Removing columns we do not need
attach(crime)
crime = subset(crime, select = -c(CrimeCd2,CrimeCd3,CrimeCd4))
crime = subset(crime, select = -c(ReportDate,CrossStreet,Location,StatusDesc,Status,WeaponDesc, Mocodes, CrmDesc, Part, AreaName))

#Transforming columns - CrimeCode
Severity = ifelse(crime$CrimeCd1 < 300, 'Severe', 'Non-Severe')
Severity = as.factor(Severity)
crime <- data.frame(crime, Severity)

#Removing CrimeCode and CrimeCd1 as Severity has replaced it
crime = subset(crime, select = -c(CrimeCode,CrimeCd1))

#Transforming columns - VictSex and Weapon
Female <-ifelse(crime$VictSex == "F", 'Yes', 'No')
Female <- as.factor(Female)
crime <- data.frame(crime, Female)

Weapon <-ifelse(crime$WeaponCd == 0, 'No', 'Yes')
Weapon <- as.factor(Weapon)
crime <- data.frame(crime, Weapon)

crime = subset(crime, select = -c(VictSex,WeaponCd))

#Splitting premise into 4 categories: Commercial, residential, industrial and outdoors
premisetable <- table(crime['PremiseDesc'])
premisetable <- sort(premisetable,decreasing = TRUE)
premisetable[1:10]

OtherPremise = case_when(crime$PremiseDesc =='SINGLE FAMILY DWELLING'~'No',crime$PremiseDesc =='STREET'~'No',crime$PremiseDesc =='MULTI-UNIT DWELLING (APARTMENT, DUPLEX, ETC)'~'No',crime$PremiseDesc =='PARKING LOT'~'No',crime$PremiseDesc =='SIDEWALK'~'No',crime$PremiseDesc =='VEHICLE, PASSENGER/TRUCK'~'No',crime$PremiseDesc =='OTHER BUSINESS'~'No',crime$PremiseDesc =='GARAGE/CARPORT'~'No',crime$PremiseDesc =='DRIVEWAY'~'No',crime$PremiseDesc =='PARKING UNDERGROUND/BUILDING'~'No')
SFamDwelling = case_when(crime$PremiseDesc == 'SINGLE FAMILY DWELLING'~'Yes')
Street = case_when(crime$PremiseDesc == 'STREET'~'Yes')
MUDwelling = case_when(crime$PremiseDesc == 'MULTI-UNIT DWELLING (APARTMENT, DUPLEX, ETC)'~'Yes')
Parking = case_when(crime$PremiseDesc == 'PARKING LOT'~'Yes')
Sidewalk = case_when(crime$PremiseDesc == 'SIDEWALK'~'Yes')
Vehicle = case_when(crime$PremiseDesc=='VEHICLE, PASSENGER/TRUCK'~'Yes')
OtherBusiness = case_when(crime$PremiseDesc == 'OTHER BUSINESS'~'Yes')
Garage = case_when(crime$PremiseDesc == 'GARAGE/CARPORT'~'Yes')
Driveway = case_when(crime$PremiseDesc == 'DRIVEWAY'~'Yes')
UnderParking = case_when(crime$PremiseDesc=='PARKING UNDERGROUND/BUILDING'~'Yes')

crime = cbind(crime,SFamDwelling,Street,MUDwelling,Parking,Sidewalk,Vehicle,OtherBusiness,Garage,Driveway,UnderParking,OtherPremise)
crime$OtherPremise[is.na(crime$OtherPremise)] <- 'Yes'
crime[is.na(crime)] <- 'No'
View(crime)

crime <- subset(crime, select = -c(PremiseCd,PremiseDesc))

