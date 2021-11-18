library(MASS)
myData <- Boston
?Boston # rows: town, columns: crim, rm, age, ... 

# goal: build linear model
# response: medv 

# linear model
m <- lm(medv ~ ., # medv as function of all features (.)
        data = myData)

# 1. randomly split data into training/testing

# randomly select indices for training set
set.seed(100)
trainIdx <- sample(506, 300)
sample(10,4)
train <- myData[trainIdx, ]

# assign remaining to testing set
test <- myData[-trainIdx, ] # assigning all rows NOT in trainIdx

# 2. build a linear model using training set
m2 <- lm(medv ~ ., data = train)
summary(m2)

# 3. predict medv values for testing set
predicted_medv <- predict(m2, test)
predicted_medv[1:10]
test$medv[1:10]

# 4. mean squared error = sum((pred - true)^2) / (num row test)
test_MSE <- mean((predicted_medv - test$medv)^2) # 21.62

# another linear model
m3 <- lm(medv ~ . - age - indus, data = train)
predicted_medv_m3 <- predict(m3, test)
test_MSE_m3 <- mean((predicted_medv_m3 - test$medv)^2) # 21.51
