require(pysch)
describe(cigratte)
data1 <-cigratte[-c(1)]
View(data1)

describe(data1)

mymodel = lm(price ~ ., data = data1)
print(mymodel)

newdata  <- data1[2,-7]
View(newdata)

prediction <-predict(mymodel,newdata)
prediction

?psych
