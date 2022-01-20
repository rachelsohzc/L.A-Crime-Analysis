#ST309 - Exercise 6

library(readr)
library(dplyr)

CC = read_csv("creditcard.csv")
View(CC)
dim(CC)

CC1 = CC[CC$Class==1,]
dim(CC1)

train1 = sample(1:492, 392)
CC1train = CC1[train1,]
CC1test = CC1[-train1,]
CC0=CC[CC$Class==0,]
dim(CC0)

train0 = sample(1:284315, 58988)
Dtrain = bind_rows(CC1train, CC0[train0[1:1200],])
dim(Dtrain)

Dtrain = arrange(Dtrain, Time)
write.csv(Dtrain, row.names=F, "creditCardTrain.csv")
Dtest = bind_rows(CC1test, CC0[train0[1200:58988],])
dim(Dtest)

Dtest = arrange(Dtest, Time)
write.csv(Dtest, row.names=F, "creditCardTest.csv")
rm(CC, CC0, CC1, CC1test, CC1train, Dtest, Dtrain, train0, train1)

#Question 1: Missing values and outliers 
CCtrain = read_csv("creditCardTrain.csv")
CCtest = read_csv("creditCardTest.csv")

View(CCtrain)
View(CCtest)

sum(is.na(CC))
sum(is.na(CCtrain))
sum(is.na(CCtest))

summary(CC)
summary(CCtrain)
summary(CCtest)

boxplot(CCtrain[,2:29], main="Boxplots of training variables")
boxplot(CCtest[,2:29],main="Boxplots of testing variables")

#Question 1: Transformations
par(mfrow=c(2,5))
plot(density(CCtrain$Amount))
plot(density(CCtrain$Time))
plot(density(CCtrain$V1))
plot(density(CCtrain$V2))
plot(density(CCtrain$v3))
plot(density(CCtrain$V4))
plot(density(CCtrain$V5))
plot(density(CCtrain$V6))
plot(density(CCtrain$V7))
plot(density(CCtrain$V8))
plot(density(CCtrain$V9))

par(mfrow=c(2,1))
plot(density(CCtrain$Amount))
plot(density(log(CCtrain$Amount+1)))

par(mfrow=c(1,1))

#Question 1: Detecting frauds
names(CC)

model1 <- lm(Class~Amount+Time+V1+V10+V11, data=CC)
summary(model1)

model2 <- lm(Class~Amount+Time+V1+V10+V11, data=CCtrain)
summary(model2)

model3 <- lm(Class~Amount+Time+V1+V10+V11, data=CCtest)
summary(model3)

#Question 2a
dim(CCtrain)

Fraud = subset(CCtrain, Class == 1)
dim(Fraud)

exploss = (sum(Fraud$Amount)*0.98)/392
exploss

#Question 2b
Genuine = subset(CCtrain, Class == 0)
dim(Genuine)

expprofit = (sum(Genuine$Amount)*0.02)/1200
expprofit

#Question 2c
#pfraud*exploss + pgenuine*expprofit > 0
#pfraud*exploss + (1-pfraud)*expprofit > 0
#pfraud(exploss) + expprofit - pfraud*expprofit > 0
#pfraud(exploss-expprofit) > - expprofit
#pfraud > -expprofit/(exploss-expprofit)

pfraud = -expprofit/(-exploss-expprofit)
pfraud

#Question 3a
library(tree)
View(CC)

attach(CC) 
Fraudulent = factor(ifelse(Class==0,"No","Yes"))
CC2 = data.frame(CC, Fraudulent)
dim(CC2)
tree.CC = tree(Fraudulent~. -Class, CC2)

summary(tree.CC)

plot(tree.CC)
text(tree.CC, pretty=1, cex=0.6)

set.seed(1)
train = sample(1:nrow(CC2), 1592)
testData = CC2[-train,]
Fraudulent.test = Fraudulent[-train]
tree2 = tree(Fraudulent~.-Class,CC2,subset=train)
tree.pred = predict(tree2,testData,type="class")

table(tree.pred,Fraudulent.test)

cat("Misclassification rate of data: ", (95+876)/(281851+95+876+393))

detach()

#Question 3b
Fraudulent = as.factor(ifelse(CCtrain$Class==0,"No","Yes"))
CCtrain1 = data.frame(CCtrain,Fraudulent)

predTrain = predict(tree.CC, CCtrain1, type="vector")
predTrain.tree = predTrain[,2]
length(predTrain.tree)

pred = ifelse((predTrain.tree>=0.5),"Yes","No")
confusion = table(pred,CCtrain1$Fraudulent)

CB = matrix(c(1.7268, 124.3865, -1, 0), nrow=2, byrow=T)
sum(CB*confusion)/sum(confusion)

alpha = seq(0.5, 0.98, 0.01)
expbenefit = vector(length=length(alpha))
for(i in 1:length(alpha)){
  predLab = ifelse((predTrain.tree>=alpha[i]),"Yes","No")
  confusion = table(predLab, CCtrain1$Fraudulent, deparse.level=2)
  expbenefit[i] = sum(CB*confusion)/sum(confusion)
}

plot(alpha, expbenefit, type="l", lwd=3, col="darkred", xlab="Cut-off probability", ylab="Expected benefit")
alpha[expbenefit==max(expbenefit)]

#Question 3c
fraud = as.factor(ifelse(CCtest$Class==0,"No","Yes"))
CCtest1 = data.frame(CCtest,fraud)

predTest = predict(tree2,CCtest1,type="vector")
predTest.tree = predTest[,2]
length(predTest.tree)

pred = ifelse((predTest.tree>=0.5),"Yes","No")
confusion = table(pred, CCtest1$fraud)

sum(CB*confusion)/sum(confusion)

pred = ifelse((predTest.tree>0.7),"Yes","No")
confusion = table(pred,CCtest1$fraud)
sum(CB*confusion)/sum(confusion)

#Question 3d
logistic.CC2 = glm(Fraudulent~Time+Amount+V1+V2+V3+V4+V5+V6+V9+V11+V12+V15+V17+V18+V19+V20+V21+V22+V23+V24+V26, data=CC2, family=binomial)
summary(logistic.CC2)

#removing V1, V19, V21,V23
logistic.CC2 = glm(Fraudulent~Time+Amount+V2+V3+V4+V5+V6+V9+V11+V12+V15+V17+V18+V20+V22+V24+V26, data=CC2, family=binomial)
summary(logistic.CC2)

dim(CC2)
set.seed(1)
pred.CC2 = predict(logistic.CC2,type="response")
pred.CC2Fraudulent = rep("No", 1592)
pred.CC2Fraudulent[pred.CC2 > 0.5] = "Yes"
table(pred.CC2Fraudulent, Fraudulent)

cat("Accuracy rate of data: ", (44+2)/(1590+2+44+278))

#Question 3e
library(ROCR)

prediction.treeTest=prediction(predTest.tree, CCtest1$fraud)
rocTest = performance(prediction.treeTest,measure="tpr",x.measure="fpr")
plot(rocTest, lwd=2, colorkey=T, colorize=T, main="ROC curve on testing data")
abline(0,1)

performance(prediction.treeTest, measure="auc")@y.values

