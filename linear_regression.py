# For reading data set
# importing necessary libraries
import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt


# reading a csv file using pandas library
wcat=pd.read_csv("E:\\Bokey\\Excelr Data\\Python Codes\\all_py\\Simple Linear Regression\\wc-at.csv")
wcat.columns

plt.hist(wcat.Waist)
plt.boxplot(wcat.Waist,0,"rs",0)


plt.hist(wcat.AT)
plt.boxplot(wcat.AT)

plt.plot(wcat.Waist,wcat.AT,"bo");plt.xlabel("Waist");plt.ylabel("AT")


wcat.AT.corr(wcat.Waist) # # correlation value between X and Y
np.corrcoef(wcat.AT,wcat.Waist)

# For preparing linear regression model we need to import the statsmodels.formula.api
import statsmodels.formula.api as smf
model=smf.ols("AT~Waist",data=wcat).fit()

# For getting coefficients of the varibles used in equation
model.params

# P-values for the variables and R-squared value for prepared model
model.summary()

model.conf_int(0.05) # 95% confidence interval

pred = model.predict(wcat.iloc[:,0]) # Predicted values of AT using the model

# Visualization of regresion line over the scatter plot of Waist and AT
# For visualization we need to import matplotlib.pyplot
import matplotlib.pylab as plt
plt.scatter(x=wcat['Waist'],y=wcat['AT'],color='red');plt.plot(wcat['Waist'],pred,color='black');plt.xlabel('WAIST');plt.ylabel('TISSUE')

pred.corr(wcat.AT) # 0.81

# Transforming variables for accuracy
model2 = smf.ols('AT~np.log(Waist)',data=wcat).fit()
model2.params
model2.summary()
print(model2.conf_int(0.01)) # 99% confidence level
pred2 = model2.predict(pd.DataFrame(wcat['Waist']))
pred2.corr(wcat.AT)
# pred2 = model2.predict(wcat.iloc[:,0])
pred2
plt.scatter(x=wcat['Waist'],y=wcat['AT'],color='green');plt.plot(wcat['Waist'],pred2,color='blue');plt.xlabel('WAIST');plt.ylabel('TISSUE')

# Exponential transformation
model3 = smf.ols('np.log(AT)~Waist',data=wcat).fit()
model3.params
model3.summary()
print(model3.conf_int(0.01)) # 99% confidence level
pred_log = model3.predict(pd.DataFrame(wcat['Waist']))
pred_log
pred3=np.exp(pred_log)  # as we have used log(AT) in preparing model so we need to convert it back
pred3
pred3.corr(wcat.AT)
plt.scatter(x=wcat['Waist'],y=wcat['AT'],color='green');plt.plot(wcat.Waist,np.exp(pred_log),color='blue');plt.xlabel('WAIST');plt.ylabel('TISSUE')
resid_3 = pred3-wcat.AT
# so we will consider the model having highest R-Squared value which is the log transformation - model3
# getting residuals of the entire data set
student_resid = model3.resid_pearson 
student_resid
plt.plot(model3.resid_pearson,'o');plt.axhline(y=0,color='green');plt.xlabel("Observation Number");plt.ylabel("Standardized Residual")

# Predicted vs actual values
plt.scatter(x=pred3,y=wcat.AT);plt.xlabel("Predicted");plt.ylabel("Actual")



# Quadratic model
wcat["Waist_Sq"] = wcat.Waist*wcat.Waist
model_quad = smf.ols("AT~Waist+Waist_Sq",data=wcat).fit()
model_quad.params
model_quad.summary()
pred_quad = model_quad.predict(wcat.Waist)

model_quad.conf_int(0.05) # 
plt.scatter(wcat.Waist,wcat.AT,c="b");plt.plot(wcat.Waist,pred_quad,"r")

plt.scatter(np.arange(109),model_quad.resid_pearson);plt.axhline(y=0,color='red');plt.xlabel("Observation Number");plt.ylabel("Standardized Residual")

plt.hist(model_quad.resid_pearson) # histogram for residual values 

############################### Implementing the Linear Regression model from sklearn library

from sklearn.linear_model import LinearRegression
import numpy as np
plt.scatter(wcat.Waist,wcat.AT)
model1 = LinearRegression()
model1.fit(wcat.Waist.values.reshape(-1,1),wcat.AT)
pred1 = model1.predict(wcat.Waist.values.reshape(-1,1))
# Adjusted R-Squared value
model1.score(wcat.Waist.values.reshape(-1,1),wcat.AT)# 0.6700
rmse1 = np.sqrt(np.mean((pred1-wcat.AT)**2)) # 32.760
model1.coef_
model1.intercept_

#### Residuals Vs Fitted values
import matplotlib.pyplot as plt
plt.scatter(pred1,(pred1-wcat.AT),c="r")
plt.hlines(y=0,xmin=0,xmax=300) 
# checking normal distribution for residual
plt.hist(pred1-wcat.AT)

### Fitting Quadratic Regression 
wcat["Waist_sqrd"] = wcat.Waist*wcat.Waist
model2 = LinearRegression()
model2.fit(X = wcat.iloc[:,[0,2]],y=wcat.AT)
pred2 = model2.predict(wcat.iloc[:,[0,2]])
# Adjusted R-Squared value
model2.score(wcat.iloc[:,[0,2]],wcat.AT)# 0.67791
rmse2 = np.sqrt(np.mean((pred2-wcat.AT)**2)) # 32.366
model2.coef_
model2.intercept_
#### Residuals Vs Fitted values
import matplotlib.pyplot as plt
plt.scatter(pred2,(pred2-wcat.AT),c="r")
plt.hlines(y=0,xmin=0,xmax=200)  
# Checking normal distribution
plt.hist(pred2-wcat.AT)
import pylab
import scipy.stats as st
st.probplot(pred2-wcat.AT,dist="norm",plot=pylab)

# Let us prepare a model by applying transformation on dependent variable
wcat["AT_sqrt"] = np.sqrt(wcat.AT)

model3 = LinearRegression()
model3.fit(X = wcat.iloc[:,[0,2]],y=wcat.AT_sqrt)
pred3 = model3.predict(wcat.iloc[:,[0,2]])
# Adjusted R-Squared value
model3.score(wcat.iloc[:,[0,2]],wcat.AT_sqrt)# 0.74051
rmse3 = np.sqrt(np.mean(((pred3)**2-wcat.AT)**2)) # 32.0507
model3.coef_
model3.intercept_
#### Residuals Vs Fitted values
import matplotlib.pyplot as plt
plt.scatter((pred3)**2,((pred3)**2-wcat.AT),c="r")
plt.hlines(y=0,xmin=0,xmax=300)  
# checking normal distribution for residuals 
plt.hist((pred3)**2-wcat.AT)
st.probplot((pred3)**2-wcat.AT,dist="norm",plot=pylab)

# Let us prepare a model by applying transformation on dependent variable without transformation on input variables 
model4 = LinearRegression()
model4.fit(X = wcat.Waist.values.reshape(-1,1),y=wcat.AT_sqrt)
pred4 = model4.predict(wcat.Waist.values.reshape(-1,1))
# Adjusted R-Squared value
model4.score(wcat.Waist.values.reshape(-1,1),wcat.AT_sqrt)# 0.7096
rmse4 = np.sqrt(np.mean(((pred4)**2-wcat.AT)**2)) # 34.165
model4.coef_
model4.intercept_
#### Residuals Vs Fitted values
import matplotlib.pyplot as plt
plt.scatter((pred4)**2,((pred4)**2-wcat.AT),c="r")
plt.hlines(y=0,xmin=0,xmax=300)  

st.probplot((pred4)**2-wcat.AT,dist="norm",plot=pylab)

# Checking normal distribution for residuals 
plt.hist((pred4)**2-wcat.AT)
