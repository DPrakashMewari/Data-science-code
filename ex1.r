# simple linear regression

# load the file 

rd = read.csv(file.choose())
colnames(rd)
attach(rd)

#checking missing values
is.na(rd)
sum(is.na(rd))

#here is eda
#  Exploratory data analysis
# quick plot
plot(YearsExperience ,Salary)  # plot(X,Y)

#Correlation Coefficient (r)
cor(YearsExperience,Salary)# cor(X,y)
qqplot(rd$YearsExperience,rd$Salary)


# basic model Simple Linear Regression model
reg <- lm(Salary ~ YearsExperience) # lm(Y ~ X)
print(reg)
summary(reg)

prediction <- predict(reg,newdata = data.frame(YearsExperience = 1)
prediction

reg$coefficients
reg$residuals  # errors

mean(reg$residuals) # which is approx zero


sqrt(mean(reg$residuals^2)) # rmse remember less value will be likely and high R2 square we want
confint(reg,level=0.95)
predict(reg,interval="predict")

library(ggplot2)

?ggplot2

ggplot(data =rd, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = rd, aes(x =YearsExperience, y=Salary))

cor(reg$fitted.values,rd$Salary)


# Rmse will be check only if we have different transformation

