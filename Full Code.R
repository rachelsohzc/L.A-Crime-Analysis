library(readr); library(dplyr); library(ggplot2); library(DataCombine); library(tidyverse); library(cowplot); library(tree); library(randomForest); library(ROCR)

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
plot(crime2019d, main="Density of crime severity for 2010 - 2019", col="Navy blue")
plot(crimed, main="Density of crime severity for 2020 - Present", col="Blue")
par(mfrow=c(1,1))

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

#Removing unknown records in VictSex and VictRace, Removing 0 in VictAge
crime <- crime[!(crime$VictSex=="X"),]
crime <- crime[!(crime$VictRace=="X"),]
crime <- crime[!(crime$VictAge==0),]
dim(crime)

attach(crime)
crime = subset(crime, select = -c(CrimeCd2,CrimeCd3,CrimeCd4))
crime = subset(crime, select = -c(Lat, Lon, RecNo,DateOCC,DistrictNo,ReportDate,CrossStreet,Location,StatusDesc,Status,WeaponDesc, Mocodes, CrmDesc, Part, AreaName))

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

#Exploratory Data Analysis
#Taking Severe Crimes only
severeexploratory = crime[!(crime$Severity=="Non-Severe"),]   
severeexploratory$TimeOCC = as.numeric(severeexploratory$TimeOCC)

#Time
TimeBar = ggplot(severeexploratory, aes(x = TimeOCC)) +
  geom_bar(stat = 'count', fill = 'red') +
  labs(x = 'Time of Crime', y = 'Number of Severe Crimes') +
  scale_x_continuous(limit = c(0,2400,0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

#Area
severeexploratory$Area = as.factor(severeexploratory$Area)

AreaBar = ggplot(severeexploratory, aes(x = Area)) +
  geom_bar(stat = 'count', fill = 'blue') +
  labs(x = 'Area', y = 'Number of Severe Crimes') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

#Sex
VictSexBar = ggplot(severeexploratory, aes(x = Female)) +
  geom_bar(stat = 'count', fill = c('red', 'green')) +
  labs(x = 'Is the Victim Female?', y = 'Number of Severe Crimes') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

#Race
VictRaceBar = ggplot(severeexploratory, aes(x = VictRace)) +
  geom_bar(stat = 'count', fill = c('purple')) +
  labs(x = 'Race', y = 'Number of Severe Crimes') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

#Age
AgeDescBar = ggplot(severeexploratory, aes(x = VictAge)) +
  geom_bar(stat = 'count', fill = c('orange')) +
  labs(x = 'AgeDesc', y = 'Number of Severe Crimes') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

#Premise
severeexploratory$PremiseDesc = as.factor(severeexploratory$PremiseDesc)

PremiseDescBar = ggplot(severeexploratory, aes(x = PremiseDesc)) +
  geom_bar(stat = 'count', fill = c('black')) +
  labs(x = 'PremiseDesc', y = 'Number of Severe Crimes') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank())

plot_grid(AreaBar, VictSexBar, TimeBar, AgeDescBar, VictRaceBar, PremiseDescBar, VictWeapBar)

#Weapon
VictWeapBar = ggplot(severeexploratory, aes(x = Weapon)) +
  geom_bar(stat = 'count', fill = c('red', 'green')) +
  labs(x = 'Was a Weapon Involved?', y = 'Number of Severe Crimes') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

VictWeapBar

#Splitting premise into 4 categories: Commercial, residential, industrial and outdoors
premisetable <- table(crime['PremiseDesc'])
premisetable <- sort(premisetable,decreasing = TRUE)
premisetable[1:10]

cat('Percentage of top 10 premises:',sum(premisetable[1:10])/nrow(crime)*100,'%')

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

crime <- subset(crime, select = -c(PremiseCd,PremiseDesc))

#Splitting race by groups
table(VictRace)
cat('Percentage of Asians in our dataset:',5829/nrow(crime)*100,"%")

Asian = case_when(crime$VictRace == 'A' ~ 'Yes', crime$VictRace == 'C' ~ 'Yes', crime$VictRace == 'D' ~ 'Yes', crime$VictRace == 'F' ~ 'Yes', crime$VictRace == 'J' ~ 'Yes', crime$VictRace == 'K' ~ 'Yes', crime$VictRace == 'L' ~ 'Yes', crime$VictRace == 'V' ~ 'Yes', crime$VictRace == 'Z' ~ 'Yes', TRUE ~ 'No')
table(Asian)
Black = ifelse(crime$VictRace == 'B', 'Yes', 'No')
Hispanic = ifelse(crime$VictRace == 'H', 'Yes', 'No')
White = ifelse(crime$VictRace == 'W', 'Yes', 'No')
OtherRace = case_when(crime$VictRace == 'O' ~ 'Yes', crime$VictRace == 'G' ~ 'Yes', crime$VictRace == 'I' ~ 'Yes', crime$VictRace == 'P' ~ 'Yes', crime$VictRace == 'S' ~ 'Yes', crime$VictRace == 'U' ~ 'Yes', TRUE ~ 'No')
crime = cbind(crime,Asian,Black,Hispanic,White,OtherRace)

crime <- subset(crime, select = -c(VictRace))


crime$TimeOCC = as.numeric(crime$TimeOCC)

#Splitting time into 4 groups  
Morning = ifelse(crime$TimeOCC <= 1159 & crime$TimeOCC >= 600, 'Yes', 'No')
Day = ifelse(crime$TimeOCC <= 1759 & crime$TimeOCC >= 1200, 'Yes', 'No')
Evening = ifelse(crime$TimeOCC <= 2359 & crime$TimeOCC >= 1800, 'Yes', 'No')
Night = ifelse(crime$TimeOCC <= 559 & crime$TimeOCC >= 0000, 'Yes', 'No')
crime = cbind(crime, Morning, Day, Evening, Night)

crime <- subset(crime, select = -c(TimeOCC))

#Splitting area into 4 boroughs: Valley, West, Central and South
Valley = case_when(crime$Area == 9 ~ 'Yes', crime$Area == 10 ~ 'Yes', crime$Area == 15 ~ 'Yes', crime$Area == 16 ~ 'Yes', crime$Area == 17 ~ 'Yes', crime$Area == 19 ~ 'Yes', crime$Area == 21 ~ 'Yes', TRUE ~ 'No')
West = case_when(crime$Area == 6 ~ 'Yes', crime$Area == 7 ~ 'Yes', crime$Area == 8 ~ 'Yes', crime$Area == 14 ~ 'Yes', crime$Area == 20 ~ 'Yes', TRUE ~ 'No')
Central = case_when(crime$Area == 1 ~ 'Yes', crime$Area == 2 ~ 'Yes', crime$Area == 4 ~ 'Yes', crime$Area == 11 ~ 'Yes', crime$Area == 13 ~ 'Yes', TRUE ~ 'No')
South = case_when(crime$Area == 3 ~ 'Yes', crime$Area == 5 ~ 'Yes', crime$Area == 12 ~ 'Yes', crime$Area == 18 ~ 'Yes', TRUE ~ 'No')
crime = cbind(crime, Valley, West, South, Central)

crime <- subset(crime, select = -c(Area))

View(crime)

#Decision tree modelling
library(tree)
set.seed(1)

convertcols <- c("Female", "Weapon", "SFamDwelling", "Street","MUDwelling", "Parking", "Sidewalk", "Vehicle", "OtherBusiness", "Garage", "Driveway", "UnderParking", "OtherPremise", "Asian", "Black", "Hispanic", "White", "OtherRace", "Morning", "Day", "Evening", "Night","Valley","West","South","Central")
crime[convertcols] <- lapply(crime[convertcols],factor)
sapply(crime, class)

#Testing the tree
tree1 = tree(Severity ~., data = crime)
summary(tree1)
plot(tree1)
text(tree1, pretty = 0)

cat('Percentage of severe crimes:',sum(crime$Severity=="Severe")/nrow(crime)*100,'%')

detach(crime)

#Cutting down the dataset because the above results were unsatisfactory
#Resampling the dataset to 10,000 samples only (5000 severe crimes, 5000 non-severe crimes)
#This will be our training data
nonsevere = crime[!(crime$Severity=="Severe"),]
severe = crime[!(crime$Severity=="Non-Severe"),]   

train = sample(1:nrow(severe),5000)
trainnotsevere = sample(1:nrow(nonsevere),5000)
testdatasev = severe[-train,]
traindatasev = severe[train,]
testdatanotsev = nonsevere[-trainnotsevere,]
traindatanonsev = nonsevere[trainnotsevere,]
traindatafinal = rbind(traindatasev,traindatanonsev)
testdatafinal = rbind(testdatasev, testdatanotsev)

#New trees
tree1 = tree(formula = Severity~., data = traindatafinal)
summary(tree1)
plot(tree1)
text(tree1, pretty = 0)

#Plotting tree 1 next to weapon percentage
table(traindatafinal$Severity, traindatafinal$Weapon)
cat('Percentage of severe crimes committed with weapons:',4961/(39+4961)*100,"%")
cat('Percentage of non-severe crimes committed with weapons:',1699/(1699+3301)*100,"%")

par(mfrow=c(1,2))
plot(tree1); text(tree1, pretty = 0)
plot(traindatafinal$Severity,traindatafinal$Weapon, xlab="Severity",ylab="Is there a weapon",col=c("Pink","Lavender"))
par(mfrow=c(1,1))

#Tree without weapons
tree2 = tree(formula = Severity ~.-Weapon, data = traindatafinal)
summary(tree2)
plot(tree2)
text(tree2, pretty = 0)

#Cross validation
#Applying to tree1
cv.crime1 = cv.tree(tree1, FUN=prune.misclass)

#Picking 3 nodes because our original already has 4 nodes
cv.crime1$size
cv.crime1$dev

prune.crime1 = prune.misclass(tree1,best=3)
plot(prune.crime1)
text(prune.crime1, pretty=0)

#Applying to tree2
cv.crime2 = cv.tree(tree2, FUN=prune.misclass)

#Picking 3 nodes
cv.crime2$size
cv.crime2$dev

prune.crime2 = prune.misclass(tree2,best=3)
plot(prune.crime2)
text(prune.crime2, pretty=0)

#Plotting both trees side by side
par(mfrow=c(1,2))
plot(prune.crime1); text(prune.crime1, pretty=0)
plot(prune.crime2); text(prune.crime2, pretty=0)
par(mfrow=c(1,1))

#Testing performance of tree1
crime.treePredict1=predict(prune.crime1, newdata = testdatafinal, type="class")
table(crime.treePredict1, testdatafinal$Severity)

cat("The misclassification rate for the testing data is",(28+59098)/(107775+28+59095+26684))

#Testing performance of tree2
crime.treePredict2=predict(prune.crime2, newdata = testdatafinal, type="class")
table(crime.treePredict2, testdatafinal$Severity)

cat("The misclassification rate for the testing data is",(14261+35951)/(130922+14261+35951+12451))

#ROC Curves for our decision trees
#Tree model 1
pred.tree1 = predict(prune.crime1, testdatafinal, type="vector")
prediction.tree1 = prediction(pred.tree1[,2], testdatafinal$Severity)
rocTree1=performance(prediction.tree1, measure = "tpr", x.measure = "fpr")

#Tree model 2
pred.tree2 = predict(prune.crime2, testdatafinal, type="vector")
prediction.tree2 = prediction(pred.tree2[,2], testdatafinal$Severity)
rocTree2=performance(prediction.tree2, measure = "tpr", x.measure = "fpr")

#Plotting both curves side by side
par(mfrow=c(1,2))
plot(rocTree1, lwd=3, colorkey=T, colorize=T, main="ROC Curve of Tree Model 1")
plot(rocTree2, lwd=3, colorkey=T, colorize=T, main="ROC Curve of Tree Model 2")
abline(0,1)

performance(prediction.tree1, measure = "auc")@y.values
performance(prediction.tree2, measure = "auc")@y.values

#Random forests
#With weapons
rf.crime1 = randomForest(Severity~., data = traindatafinal, mtry = 5, importance = T)
rf.crime1
varImpPlot(rf.crime1, col = c('red', 'blue'))

test.rf1 = predict(rf.crime1, newdata = testdatafinal, type = 'class')
table(test.rf1, testdatafinal$Severity)

cat("The misclassification rate for the testing data is",(1126+52587)/(114286+1126+52587+25586))

#Without weapons
rf.crime2 = randomForest(Severity~.-Weapon, data = traindatafinal, mtry = 5, importance = T)
rf.crime2
varImpPlot(rf.crime2, col = c('red', 'blue'))

test.rf2 = predict(rf.crime2, newdata = testdatafinal, type = 'class')
table(test.rf2, testdatafinal$Severity)

cat("The misclassification rate for the testing data is",(9090+52187)/(114686+9090+52187+17622))

#ROC Curves for random forests
#With weapons
pred.rf1 = predict(rf.crime1, testdatafinal)
prediction.rf1 = prediction((as.numeric(pred.rf1) - 1), (as.numeric(testdatafinal$Severity)-1))
rocrf1=performance(prediction.rf1, measure = "tpr", x.measure = "fpr")
plot(rocrf1, lwd=3, colorkey=T, colorize=T, main="ROC Curve of RF Model 1")
abline(0,1)
performance(prediction.rf1, measure = "auc")@y.values

pred.rf3 = predict(rf.crime1, testdatafinal)
prediction.rf3 = prediction(as.numeric(pred.rf3), as.numeric(testdatafinal$Severity))
rocrf3=performance(prediction.rf3, measure = "tpr", x.measure = "fpr")
plot(rocrf3, lwd=3, colorkey=T, colorize=T, main="ROC Curve of RF Model 3")
abline(0,1)
performance(prediction.rf3, measure = "auc")@y.values

#Without weapons
pred.rf2 = predict(rf.crime2, testdatafinal)
prediction.rf2 = prediction((as.numeric(pred.rf2) - 1), (as.numeric(testdatafinal$Severity)-1))
rocrf2=performance(prediction.rf2, measure = "tpr", x.measure = "fpr")
plot(rocrf2, lwd=3, colorkey=T, colorize=T, main='ROC Curve of RF2')
abline(0,1)
performance(prediction.rf2, measure = "auc")@y.values

pred.rf4 = predict(rf.crime2, testdatafinal)
prediction.rf4 = prediction(as.numeric(pred.rf4), as.numeric(testdatafinal$Severity))
rocrf4=performance(prediction.rf4, measure = "tpr", x.measure = "fpr")
plot(rocrf4, lwd=3, colorkey=T, colorize=T, main="ROC Curve of RF 4")
abline(0,1)
performance(prediction.rf4, measure = "auc")@y.values

#Plotting both curves side by side
par(mfrow=c(1,2))
plot(rocrf1, lwd=3, colorkey=T, colorize=T, main="ROC Curve of RF Model 1")
plot(rocrf2, lwd=3, colorkey=T, colorize=T, main='ROC Curve of RF2')
abline(0,1)

#Logistic regression
#Converting factors to numeric
traindatafinal$Weapon <- as.numeric(traindatafinal$Weapon) - 1
traindatafinal$Female <- as.numeric(traindatafinal$Female) - 1
traindatafinal$SFamDwelling = as.numeric(traindatafinal$SFamDwelling) - 1
traindatafinal$Street = as.numeric(traindatafinal$Street) - 1
traindatafinal$MUDwelling = as.numeric(traindatafinal$MUDwelling) - 1
traindatafinal$Parking = as.numeric(traindatafinal$Parking) - 1
traindatafinal$Sidewalk = as.numeric(traindatafinal$Sidewalk) - 1
traindatafinal$Vehicle = as.numeric(traindatafinal$Vehicle) - 1
traindatafinal$OtherBusiness = as.numeric(traindatafinal$OtherBusiness) - 1
traindatafinal$Garage = as.numeric(traindatafinal$Garage) - 1
traindatafinal$Driveway = as.numeric(traindatafinal$Driveway) - 1
traindatafinal$UnderParking = as.numeric(traindatafinal$UnderParking) - 1
traindatafinal$OtherPremise = as.numeric(traindatafinal$OtherPremise) - 1
traindatafinal$Asian = as.numeric(traindatafinal$Asian) - 1
traindatafinal$Black = as.numeric(traindatafinal$Black) - 1
traindatafinal$Hispanic = as.numeric(traindatafinal$Hispanic) - 1
traindatafinal$White = as.numeric(traindatafinal$White) - 1
traindatafinal$OtherRace = as.numeric(traindatafinal$OtherRace) - 1
traindatafinal$Morning = as.numeric(traindatafinal$Morning) - 1
traindatafinal$Day = as.numeric(traindatafinal$Day) - 1
traindatafinal$Evening = as.numeric(traindatafinal$Evening) - 1
traindatafinal$Night = as.numeric(traindatafinal$Night) - 1
traindatafinal$Valley = as.numeric(traindatafinal$Valley) - 1
traindatafinal$West = as.numeric(traindatafinal$West) - 1
traindatafinal$South = as.numeric(traindatafinal$South) - 1
traindatafinal$Central = as.numeric(traindatafinal$Central) - 1

testdatafinal$Weapon <- as.numeric(testdatafinal$Weapon) - 1
testdatafinal$Female <- as.numeric(testdatafinal$Female) - 1
testdatafinal$SFamDwelling = as.numeric(testdatafinal$SFamDwelling) - 1
testdatafinal$Street = as.numeric(testdatafinal$Street) - 1
testdatafinal$MUDwelling = as.numeric(testdatafinal$MUDwelling) - 1
testdatafinal$Parking = as.numeric(testdatafinal$Parking) - 1
testdatafinal$Sidewalk = as.numeric(testdatafinal$Sidewalk) - 1
testdatafinal$Vehicle = as.numeric(testdatafinal$Vehicle) - 1
testdatafinal$OtherBusiness = as.numeric(testdatafinal$OtherBusiness) - 1
testdatafinal$Garage = as.numeric(testdatafinal$Garage) - 1
testdatafinal$Driveway = as.numeric(testdatafinal$Driveway) - 1
testdatafinal$UnderParking = as.numeric(testdatafinal$UnderParking) - 1
testdatafinal$OtherPremise = as.numeric(testdatafinal$OtherPremise) - 1
testdatafinal$Asian = as.numeric(testdatafinal$Asian) - 1
testdatafinal$Black = as.numeric(testdatafinal$Black) - 1
testdatafinal$Hispanic = as.numeric(testdatafinal$Hispanic) - 1
testdatafinal$White = as.numeric(testdatafinal$White) - 1
testdatafinal$OtherRace = as.numeric(testdatafinal$OtherRace) - 1
testdatafinal$Morning = as.numeric(testdatafinal$Morning) - 1
testdatafinal$Day = as.numeric(testdatafinal$Day) - 1
testdatafinal$Evening = as.numeric(testdatafinal$Evening) - 1
testdatafinal$Night = as.numeric(testdatafinal$Night) - 1
testdatafinal$Valley = as.numeric(testdatafinal$Valley) - 1
testdatafinal$West = as.numeric(testdatafinal$West) - 1
testdatafinal$South = as.numeric(testdatafinal$South) - 1
testdatafinal$Central = as.numeric(testdatafinal$Central) - 1

#Modelling with weapon
names(crime)
logistic.crime=glm(Severity~VictAge+Female+Weapon+SFamDwelling+Street+MUDwelling+Parking+Sidewalk+Vehicle+OtherBusiness+Garage+Driveway+UnderParking+Asian+Black+Hispanic+White+Morning+Day+Night+Central+South+West, data=traindatafinal,family=binomial)
summary(logistic.crime)
logistic.crime2=glm(Severity~VictAge+Female+Weapon+SFamDwelling+Street+MUDwelling+Parking+Sidewalk+Vehicle+OtherBusiness+Garage+UnderParking+Asian+Black+Hispanic+White+Morning+Day+Night+Central+South+West, data=traindatafinal,family=binomial)
summary(logistic.crime2)
logistic.crime3=glm(Severity~VictAge+Female+Weapon+SFamDwelling+Street+MUDwelling+Parking+Sidewalk+Vehicle+Garage+UnderParking+Asian+Black+Hispanic+White+Morning+Day+Night+Central+South+West, data=traindatafinal,family=binomial)
summary(logistic.crime3)
logistic.crime4=glm(Severity~VictAge+Female+Weapon+SFamDwelling+Street+MUDwelling+Parking+Sidewalk+Vehicle+Garage+UnderParking+Asian+Black+Hispanic+Morning+Day+Night+Central+South+West, data=traindatafinal,family=binomial)
summary(logistic.crime4)
logistic.crime5=glm(Severity~VictAge+Female+Weapon+SFamDwelling+Street+MUDwelling+Parking+Sidewalk+Vehicle+UnderParking+Asian+Black+Hispanic+Morning+Day+Night+Central+South+West, data=traindatafinal,family=binomial)
summary(logistic.crime5)
logistic.crime6=glm(Severity~VictAge+Female+Weapon+SFamDwelling+Street+MUDwelling+Parking+Sidewalk+Vehicle+UnderParking+Black+Hispanic+Morning+Day+Night+Central+South+West, data=traindatafinal,family=binomial)
summary(logistic.crime6)
logistic.crime7=glm(Severity~VictAge+Female+Weapon+SFamDwelling+Street+MUDwelling+Parking+Sidewalk+Vehicle+Black+Hispanic+Morning+Day+Night+Central+South+West, data=traindatafinal,family=binomial)
summary(logistic.crime7)

contrasts(testdatafinal$Severity)

logistic.test1= predict(logistic.crime7, testdatafinal, type = 'response')
pred.crimeseverity1= rep('Non-Severe',193585)
pred.crimeseverity1[logistic.test1 > 0.5] = 'Severe'
table(pred.crimeseverity1, testdatafinal$Severity)

cat("The misclassification rate for the testing data is",(1261+52503)/(114370+1261+52503+25451))

#Logistic regression model without weapon
logistic.newcrime=glm(Severity~VictAge+Female+SFamDwelling+Street+MUDwelling+Parking+Sidewalk+Vehicle+OtherBusiness+Garage+Driveway+UnderParking+Asian+Black+Hispanic+White+Morning+Day+Night+Central+South+West, data=traindatafinal,family=binomial)
summary(logistic.newcrime)
logistic.newcrime2=glm(Severity~VictAge+Female+SFamDwelling+Street+MUDwelling+Sidewalk+Vehicle+OtherBusiness+Garage+Driveway+UnderParking+Asian+Black+Hispanic+White+Morning+Day+Night+Central+South+West, data=traindatafinal,family=binomial)
summary(logistic.newcrime2)
logistic.newcrime3=glm(Severity~VictAge+Female+SFamDwelling+Street+MUDwelling+Sidewalk+Vehicle+OtherBusiness+Garage+Driveway+UnderParking+Black+Hispanic+White+Morning+Day+Night+Central+South+West, data=traindatafinal,family=binomial)
summary(logistic.newcrime3)
logistic.newcrime4=glm(Severity~VictAge+Female+SFamDwelling+Street+MUDwelling+Sidewalk+Vehicle+OtherBusiness+Garage+Driveway+UnderParking+Black+Hispanic+Morning+Day+Night+Central+South+West, data=traindatafinal,family=binomial)
summary(logistic.newcrime4)

logistic.test2= predict(logistic.newcrime4, testdatafinal, type = 'response')
pred.crimeseverity2= rep('Non-Severe',193585)
pred.crimeseverity2[logistic.test2 > 0.5] = 'Severe'
table(pred.crimeseverity2, testdatafinal$Severity)

cat("The misclassification rate for the testing data is",(8203+58025)/(108848+8203+58025+18509))

#ROC Curves for logistic regression
#With weapons
pred.glm1 = predict(logistic.crime7, testdatafinal, type="response")
prediction.glm1 = prediction(pred.glm1, testdatafinal$Severity)
rocGlm1 = performance(prediction.glm1, measure = "tpr", x.measure = "fpr")

plot(rocGlm1, lwd=3, colorkey=T, colorize=T, main="ROC Curve of Logistic Regression with weapons")
abline(0,1)

performance(prediction.glm1, measure = "auc")@y.values

#Without weapons
pred.glm2 = predict(logistic.newcrime4, testdatafinal, type="response")
prediction.glm2 = prediction(pred.glm2, testdatafinal$Severity)
rocGlm2 = performance(prediction.glm2, measure = "tpr", x.measure = "fpr")

plot(rocGlm2, lwd=3, colorkey=T, colorize=T, main="ROC Curve of Logistic Regression without weapons")
abline(0,1)

performance(prediction.glm2, measure = "auc")@y.values

#Plotting both curves side by side
par(mfrow=c(1,2))
plot(rocGlm1, lwd=3, colorkey=T, colorize=T, main="ROC Curve of Logistic Regression with weapons")
plot(rocGlm2, lwd=3, colorkey=T, colorize=T, main="ROC Curve of Logistic Regression without weapons")
abline(0,1)
