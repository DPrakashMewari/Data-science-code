# linear regression is used  to predict the value of a continous variable y based on one or more input predictor variable x
# respone varible is Y 
# predictor variable is X
# You can use this formula to predict Y when x VALUE is know


data("cars")
head(cars)
tail(cars)
attach(cars)
colnames(cars)
rownames(cars)
# dist =y 
# speed = x

# In a graphical plot we will check the 
# scatter plot:vizulise the linear relation bw predictor and response
# box plot : for finding an ouliers
# Density plot : To see the distribution of the predictor variable ideally close to normaly distrbuted

scatter.smooth(x =cars$speed,y = cars$dist,main = "dist ~ speed")
boxplot(speed)$out
boxplot(dist)$out

# using Density plot to check response variable is close to normal
library(e1071) # for skewness func
par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'


polygon(density(cars$speed), col="red")

plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'

polygon(density(cars$dist), col="red")

# now correlation value 
cor(speed,dist)

# Make a linear model now
model <- lm(dist ~ speed,data = cars)
print(model)

#s this enough to actually use this model? NO!

#Because, before using a regression model to make predictions, you need to ensure that it is statistically significant. But How do you ensure this?

summary(model)

# before using a model for prediction you need to ensure that is statiscally significant 

## first we check pvalue which is based upon hypotheis if our condition lies there 
# a larger t value is better

# we check various thing before confirming a model

# predicting linear models 
# people suggest me to split a model before making BUT TRY USING WITH SAMPLE



# Now again make model but first train and test data
# create training and test data 
set.seed(100)
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

# build the model on training data 
lmMod <- lm(dist ~ speed,data = trainingData)
distpred <- predict(lmMod,testData)

summary(lmMod)

# calculation prediction and error rates
actual_preds <- data.frame(cbind(actuals=testData$dist,predicted = distpred))

correlation_accuracy <- cor(actual_preds)
correlation_accuracy

# now calculate minmac accuracy:
# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actual_preds, 1, min) / apply(actual_preds, 1, max))  
# => 38.00%, min_max accuracy
min_max_accuracy


# MAPE Calculation
mape <- mean(abs((actual_preds$predicteds - actual_preds$actual))/actual_preds$actual)  
# => 69.95%, mean absolute percentage deviatio
mape

DMwR::regr.eval(actual_preds$actual actual_preds$predicted)



https://www.machinelearningplus.com/machine-learning/complete-introduction-linear-regression-r/
  