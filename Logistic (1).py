import pandas as pd 
import numpy  as np
import matplotlib.pyplot as plt
#Importing Data
claimants = pd.read_csv("E:\\Bokey\\Excelr Data\\Python Codes\\all_py\\Logistic Regression\\claimants.csv")

claimants.head(4)
#removing CASENUM
claimants.drop(["CASENUM"],axis=1) 
claimants.drop(["CASENUM"],axis=1,inplace=True)

claimants.head(4)# to see top 10 observations

# usage lambda and apply function
# apply function => we use to apply custom function operation on 
# each column
# lambda just an another syntax to apply a function on each value 
# without using for loop 
claimants.isnull().sum()

import seaborn as sns
sns.boxplot(x="CLMSEX",y="CLMAGE",data=claimants)
claimants.describe()

claimants.apply(lambda x:x.mean()) 
claimants.mean()

#Imputating the missing values with most repeated values in that column  

# lambda x:x.fillna(x.value_counts().index[0]) 
# the above line gives you the most repeated value in each column  

claimants.ATTORNEY.value_counts()
claimants.ATTORNEY.value_counts().index[0] # gets you the most occuring value

claimants.CLMSEX.value_counts()
claimants.CLMSEX.value_counts().index[0] # gets you the most occuring value
claimants.isnull().sum()
# filling the missing value with most occuring value    
claimants.iloc[:,0:4] = claimants.iloc[:,0:4].apply(lambda x:x.fillna(x.value_counts().index[0]))
claimants.isnull().sum()
#claimants.SEATBELT = claimants.SEATBELT.fillna(claimants.SEATBELT.value_counts().index[0])

claimants.iloc[:,0:4].columns

claimants.CLMAGE = claimants.CLMAGE.fillna(claimants.CLMAGE.mean())

# filling the missing value with mean of that column
claimants.iloc[:,4:] = claimants.iloc[:,4:].apply(lambda x:x.fillna(x.mean()))


# Checking if we have na values or not 
claimants.isnull().sum() # No null values


#Model building 

import statsmodels.formula.api as sm
logit_model = sm.logit('ATTORNEY~CLMAGE+LOSS+CLMINSUR+CLMSEX+SEATBELT',data = claimants).fit()

from scipy import stats
import scipy.stats as st
st.chisqprob = lambda chisq, df: stats.chi2.sf(chisq, df)

#summary
logit_model.summary()
y_pred = logit_model.predict(claimants)

claimants["pred_prob"] = y_pred
# Creating new column for storing predicted class of Attorney

# filling all the cells with zeroes
claimants["Att_val"] = np.zeros(1340)

# taking threshold value as 0.5 and above the prob value will be treated 
# as correct value 
claimants.loc[y_pred>=0.5,"Att_val"] = 1
claimants.Att_val

from sklearn.metrics import classification_report
classification_report(claimants.Att_val,claimants.ATTORNEY)

# confusion matrix 
confusion_matrix = pd.crosstab(claimants['ATTORNEY'],claimants.Att_val)

confusion_matrix
accuracy = (436+504)/(1340) # 70.14
accuracy

# ROC curve 
from sklearn import metrics
# fpr => false positive rate
# tpr => true positive rate
fpr, tpr, threshold = metrics.roc_curve(claimants.ATTORNEY, y_pred)


# the above function is applicable for binary classification class 

plt.plot(fpr,tpr);plt.xlabel("False Positive");plt.ylabel("True Positive")
 
roc_auc = metrics.auc(fpr, tpr) # area under ROC curve 


### Dividing data into train and test data sets
claimants.drop("Att_val",axis=1,inplace=True)
from sklearn.model_selection import train_test_split

train,test = train_test_split(claimants,test_size=0.3)

# checking na values 
train.isnull().sum();test.isnull().sum()

# Building a model on train data set 

train_model = sm.logit('ATTORNEY~CLMAGE+LOSS+CLMINSUR+CLMSEX+SEATBELT',data = train).fit()

#summary
train_model.summary()
train_pred = train_model.predict(train.iloc[:,1:])

# Creating new column for storing predicted class of Attorney

# filling all the cells with zeroes
train["train_pred"] = np.zeros(938)

# taking threshold value as 0.5 and above the prob value will be treated 
# as correct value 
train.loc[train_pred>0.5,"train_pred"] = 1

# confusion matrix 
confusion_matrix = pd.crosstab(train['ATTORNEY'],train.train_pred)

confusion_matrix
accuracy_train = (436+504)/(1340) # 70.14
accuracy_train

# Prediction on Test data set

test_pred = train_model.predict(test)

# Creating new column for storing predicted class of Attorney

# filling all the cells with zeroes
test["test_pred"] = np.zeros(402)

# taking threshold value as 0.5 and above the prob value will be treated 
# as correct value 
test.loc[test_pred>0.5,"test_pred"] = 1

# confusion matrix 
confusion_matrix = pd.crosstab(test['ATTORNEY'],test.test_pred)

confusion_matrix
accuracy_test = (126+161)/(402) # 71.39
accuracy_test

