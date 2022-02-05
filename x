Asian = case_when(crime$VictRace == 'A' ~ 'Yes', crime$VictRace == 'C' ~ 'Yes', crime$VictRace == 'D' ~ 'Yes', crime$VictRace == 'F' ~ 'Yes', crime$VictRace == 'J' ~ 'Yes', crime$VictRace == 'K' ~ 'Yes', crime$VictRace == 'L' ~ 'Yes', crime$VictRace == 'V' ~ 'Yes', crime$VictRace == 'Z' ~ 'Yes', TRUE ~ 'No')
table(Asian)
crime = data.frame(crime, Asian)
Black = ifelse(crime$VictRace == 'B', 'Yes', 'No')
crime = data.frame(crime, Black)
Hispanic = ifelse(crime$VictRace == 'H', 'Yes', 'No')
crime = data.frame(crime, Hispanic)
White = ifelse(crime$VictRace == 'W', 'Yes', 'No')
crime = data.frame(crime, White)
Other = case_when(crime$VictRace == 'O' ~ 'Yes', crime$VictRace == 'G' ~ 'Yes', crime$VictRace == 'I' ~ 'Yes', crime$VictRace == 'P' ~ 'Yes', crime$VictRace == 'S' ~ 'Yes', crime$VictRace == 'U' ~ 'Yes', TRUE ~ 'No')
crime = data.frame(crime, Other)
Asian = 
Asian = if(crime$VictRace in c('A', 'C', 'D', 'F', 'J', 'K', 'L', 'V', 'Z'))
  
Morning = ifelse(crime$TimeOCC <= 1159 & crime$TimeOCC >= 600, 'Yes', 'No')
Day = ifelse(crime$TimeOCC <= 1759 & crime$TimeOCC >= 1200, 'Yes', 'No')
Evening = ifelse(crime$TimeOCC <= 2359 & crime$TimeOCC >= 1800, 'Yes', 'No')
Night = ifelse(crime$TimeOCC <= 559 & crime$TimeOCC >= 0000, 'Yes', 'No')
crime = data.frame(crime, Morning)
crime = data.frame(crime, Day)
crime = data.frame(crime, Evening)
crime = data.frame(crime, Night)

Valley = case_when(crime$Area == 9 ~ 'Yes', crime$Area == 10 ~ 'Yes', crime$Area == 15 ~ 'Yes', crime$Area == 16 ~ 'Yes', crime$Area == 17 ~ 'Yes', crime$Area == 19 ~ 'Yes', crime$Area == 21 ~ 'Yes', TRUE ~ 'No')
West = case_when(crime$Area == 6 ~ 'Yes', crime$Area == 7 ~ 'Yes', crime$Area == 8 ~ 'Yes', crime$Area == 14 ~ 'Yes', crime$Area == 20 ~ 'Yes', TRUE ~ 'No')
Central = case_when(crime$Area == 1 ~ 'Yes', crime$Area == 2 ~ 'Yes', crime$Area == 4 ~ 'Yes', crime$Area == 11 ~ 'Yes', crime$Area == 13 ~ 'Yes', TRUE ~ 'No')
South = case_when(crime$Area == 3 ~ 'Yes', crime$Area == 5 ~ 'Yes', crime$Area == 12 ~ 'Yes', crime$Area == 18 ~ 'Yes', TRUE ~ 'No')
crime = cbind(crime, Valley, West, South, Central)

class(VictAge)
crime$SFamDwelling = as.factor(crime$SFamDwelling)
crime$Street = as.factor(crime$Street)
crime$MUDwelling = as.factor(crime$MUDwelling)
crime$Parking = as.factor(crime$Parking)
crime$Sidewalk = as.factor(crime$Sidewalk)
crime$Vehicle = as.factor(crime$Vehicle)
crime$OtherBusiness = as.factor(crime$OtherBusiness)
crime$Garage = as.factor(crime$Garage)
crime$Driveway = as.factor(crime$Driveway)
crime$UnderParking = as.factor(crime$UnderParking)
crime$OtherPremise = as.factor(crime$OtherPremise)
crime$Asian = as.factor(crime$Asian)
crime$Black = as.factor(crime$Black)
crime$Hispanic = as.factor(crime$Hispanic)
crime$White = as.factor(crime$White)
crime$Other = as.factor(crime$Other)
crime$Morning = as.factor(crime$Morning)
crime$Day = as.factor(crime$Day)
crime$Evening = as.factor(crime$Evening)
crime$Night = as.factor(crime$Night)
crime$West = as.factor(crime$West)
crime$Valley = as.factor(crime$Valley)
crime$South = as.factor(crime$South)
crime$Central = as.factor(crime$Central)
view(crime)

test = tree(Severity2 ~., data = newcrime)
summary(test)
plot(test)
text(test, pretty = 0)

newcrime$Weapon <- as.numeric(newcrime$Weapon)
newcrime$Weapon = newcrime$Weapon - 1
newcrime$Female <- as.numeric(newcrime$Female)
newcrime$Female = newcrime$Female - 1

newcrime[newcrime=="Yes"] <- 1
newcrime[newcrime=="No"] <- 0

convertcols <- c("SFamDwelling","Street","MUDwelling", "Parking", "Sidewalk", "Vehicle", "OtherBusiness", "Garage", "Driveway", "UnderParking", "OtherPremise", "Asian", "Black", "Hispanic", "White", "OtherRace", "Morning", "Day", "Evening", "Night","Valley","West","South","Central")
newcrime[convertcols] <- lapply(newcrime[convertcols],factor)

newcrime$SFamDwelling = as.numeric(newcrime$SFamDwelling) - 1
newcrime$Street = as.numeric(newcrime$Street) - 1
newcrime$MUDwelling = as.numeric(newcrime$MUDwelling) - 1
newcrime$Parking = as.numeric(newcrime$Parking) - 1
newcrime$Sidewalk = as.numeric(newcrime$Sidewalk) - 1
newcrime$Vehicle = as.numeric(newcrime$Vehicle) - 1
newcrime$OtherBusiness = as.numeric(newcrime$OtherBusiness) - 1
newcrime$Garage = as.numeric(newcrime$Garage) - 1
newcrime$Driveway = as.numeric(newcrime$Driveway) - 1
newcrime$UnderParking = as.numeric(newcrime$UnderParking) - 1
newcrime$OtherPremise = as.numeric(newcrime$OtherPremise) - 1
newcrime$Asian = as.numeric(newcrime$Asian) - 1
newcrime$Black = as.numeric(newcrime$Black) - 1
newcrime$Hispanic = as.numeric(newcrime$Hispanic) - 1
newcrime$White = as.numeric(newcrime$White) - 1
newcrime$OtherRace = as.numeric(newcrime$OtherRace) - 1
newcrime$Morning = as.numeric(newcrime$Morning) - 1
newcrime$Day = as.numeric(newcrime$Day) - 1
newcrime$Evening = as.numeric(newcrime$Evening) - 1
newcrime$Night = as.numeric(newcrime$Night) - 1
newcrime$Valley = as.numeric(newcrime$Valley) - 1
newcrime$West = as.numeric(newcrime$West) - 1
newcrime$South = as.numeric(newcrime$South) - 1
newcrime$Central = as.numeric(newcrime$Central) - 1

logistic.newcrime=glm(Severity~VictAge+Female+Weapon+SFamDwelling+Street+MUDwelling+Parking+Sidewalk+Vehicle+OtherBusiness+Garage+Driveway+UnderParking+Asian+Black+Hispanic+White+Morning+Day+Night+West+South+Central, data=newcrime,family=binomial)
summary(logistic.newcrime)
