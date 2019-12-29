# import data set
Bank_data #here i have data
attach(Bank_data)
View(Bank_data)

# check a structure of data
str(Bank_data)
summary(Bank_data)

#Eda analysis on that

cor(Bank_data)# we cant to corr on that
colnames(Bank_data)
plot(age,y) # sigmoid type

# Is there Na value
sum(is.na(Bank_data))

#Now fixing some values

Bank_data$y  = ifelse(Bank_data$y == 'yes',1,0)
Bank_data$y  = factor(Bank_data$y,levels = c(0,1))
table(Bank_data$y)
str(Bank_data)
Bank_data$job <- ifelse(Bank_data$job == "unknown",1,0)
Bank_data$education <-ifelse(Bank_data$education == "unknown",1,0 )
Bank_data$contact <- ifelse(Bank_data$contact == "unknown",1,0)
Bank_data$poutcome <- ifelse(Bank_data$poutcome == "unknown",1,0)

Bank_data$job <- as.numeric(as.factor(Bank_data$job))
Bank_data$marital <- as.numeric(as.factor(Bank_data$marital))
Bank_data$education <- as.numeric(as.factor(Bank_data$education))
Bank_data$default <- ifelse(Bank_data$default == "yes",1,0)
Bank_data$housing <- ifelse(Bank_data$housing == "yes",1,0)
Bank_data$loan <- as.numeric(as.factor(Bank_data$loan))
Bank_data$month <- as.numeric(as.factor(Bank_data$month))
Bank_data$contact <- as.numeric(as.factor(Bank_data$poutcome)
str(Bank_data)      

table(Bank_data$y)

# here how to handle class imbalance with upsampling and downsampling
library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.

# splitting a data 
set.seed(100)

trainDataIndex <- createDataPartition(Bank_data$y, p=0.7, list = F)  # 70% training data
trainData <- Bank_data[trainDataIndex, ]
testData <- Bank_data[-trainDataIndex, ]

table(trainData$y)
table(testData$y)

# use a approch Down sample
set.seed(100)

down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],y = trainData$y)
table(down_train$Class)

#up sampling
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "Class"],
                     y = trainData$y)

table(up_train$Class)
colnames(Bank_data)
# Build a logistic model
logitmod <- glm(Class ~ age + job + marital + education +default+balance+housing+loan+contact+day+month+duration+campaign+pdays+previous+poutcome,family = "binomial",data = down_train)

summary(logitmod)

pred <- predict(logitmod,newdata = testData,type = "response")
pred

# recode factors
y_pred_num <- ifelse(pred > 0.5,1,0)
y_pred <-factor(y_pred_num,levels = c(0,1))
y_act <-testData$y


# check a accuracy:
mean(y_pred == y_act)# 80% accuracy



# making a roccurve
library(ROCR)
rocrpred<-prediction(pred,y_act)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
windows()
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))


## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)
rocr_cutoff <- round(rocr_cutoff,6)


library(dplyr)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)

library(pROC)
auc <- performance(rocrpred,measure ="auc")
auc <- auc@y.values[[1]]
auc 


#we mainly do with train and split 
# other wise we do with fulll data
# check aic values , null deviance , Residual deviance
# roc and auc
# accuracy 

