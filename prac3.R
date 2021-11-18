library("MASS")
myData <- Boston
?Boston #rows : house, column: crim, rm, age, %>% 

#goal : build linear model
#response: medv
set.seed(100)

# linear model
m <- lm(medv ~ ., #medv as function of all features (.)
        data = myData)

#random select indices for training set
trainIdx <- sample(506,300)
sample(10,4)
train <- myData[trainIdx, ]

# assign remaining to testing set
test <- myData[-trainIdx, ] #assign all rows NOT in trainIdx

# 2. build a linear model using training set
m2 <- lm(medv ~ ., data = train)
summary(m2)

# 3. predict medv values for testing set
predicted_medv <- predict(m2, test)

predicted_medv[1:10]
test$medv[1:10]

# 4. mean squared error sum((pred -true)^2) / (num row test)
test_MSE <- mean((predicted_medv-test$medv)^2)

m3 <- lm(medv ~ . - age - indus, data = myData)
predicted_medv_m3 <- predict(m3, test)
test_MSE_m3 <- mean((predicted_medv_m3 - test$medv)^2)



