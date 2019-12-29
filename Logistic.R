claimants<- read.csv(file.choose()) # Choose the claimants Data set
View(claimants)

# Imputation or Omiting Nas?

sum(is.na(claimants)) # check how many na value?
claimants1 <- na.omit(claimants)

sum(is.na(claimants1))
 # Omitting NA values from the Data 
# na.omit => will omit the rows which has atleast 1 NA value
dim(claimants1)
colnames(claimants1)

sum(is.na(claimants$CLMSEX))
sum(is.na(claimants$CLMAGE))
boxplot(claimants$CLMAGE)$out
boxplot(claimants$CLMAGE)
summary(claimants)
str(claimants)
# imputation for missing values
claimants$CLMAGE[is.na(claimants$CLMAGE)] <- 30
sum(is.na(claimants$CLMAGE))

claimants$CLMSEX[is.na(claimants$CLMSEX)] <- 1
table(claimants$CLMSEX)
sum(is.na(claimants$CLMSEX))

table(claimants$SEATBELT)
claimants$SEATBELT[is.na(claimants$SEATBELT)] <- 0
sum(is.na(claimants$SEATBELT))

table(claimants$CLMINSUR)
claimants$CLMINSUR[is.na(claimants$CLMINSUR)] <- 1

sum(is.na(claimants$CLMINSUR))

claimantsfinal <- claimants[,-1]
summary(claimantsfinal)

str(claimantsfinal)
summary(claimantsfinal)

# converting integers to factors
claimantsfinal$ATTORNEY <-factor(claimantsfinal$ATTORNEY)
claimantsfinal$CLMSEX <- factor(claimantsfinal$CLMSEX)
claimantsfinal$CLMINSUR <- factor(claimantsfinal$CLMINSUR)
claimantsfinal$SEATBELT <- factor(claimantsfinal$SEATBELT)

str(claimantsfinal)
# Preparing a linear regression 
mod_lm <- lm(ATTORNEY~.,data=claimantsfinal)
summary(mod_lm)# blank and error
mod_lm$fitted.values
# Linear Regression technique cnnot be employed

#if we use non converted it deploy but problem there
mod_lm1 <- lm(ATTORNEY ~.,data = claimants)
summary(mod_lm1)


# We can also include NA values but where ever it finds NA value
# probability values obtained using the glm will also be NA 
# So they can be either filled using imputation technique or
# exlclude those values 


# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model <- glm(ATTORNEY~.,data=claimantsfinal,family = "binomial")
summary(model)

# make a equation with that : ATTORNEY Y =0.155216+0.32057(CLMSEX)....
# PROBABLITY OF NOT HIRING AN ATTORNEY = 0.15 * 0.32(CLMS)
# aic 
model1 <- glm(ATTORNEY ~ .-SEATBELT, family ="binomial",data=claimantsfinal)
summary(model1)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))
model1$fitted.values


# Confusion matrix table 
prob <- predict(model1,claimantsfinal,type="response")
summary(model1)
prob
# We are going to use NULL and Residual Deviance to compare the between different models

# mismatch calculted by Confusion matrix and considering the threshold value as 0.5 

 table(claimantsfinal$ATTORNEY)
confusion<-table(prob>0.56,claimantsfinal$ATTORNEY)
confusion<- table(claimantsfinal$ATTORNEY,prob>0.56)
confusion


# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 70.62




# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob,claimants$ATTORNEY)
rocrperf<-performance(rocrpred,'tpr','fpr')
?performance
str(rocrperf)
windows()
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)
rocr_cutoff <- round(rocr_cutoff,2)

library(dplyr)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)


install.packages("pROC")
library(pROC)
auc <- performance(rocrpred,measure ="auc")
auc <- auc@y.values[[1]]
auc 
# accuracy and auc