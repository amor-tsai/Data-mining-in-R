#install.packages("languageserver")
# install.packages("glmnet")
warnings()

library(MASS)
library(glmnet)
library(matlib)
library(ggplot2)

load("Cars93.RData")

#Part1

#1. (LASSO) We want to build a LASSO model using one of the values { 0.001, 0.01, 0.1, 1, 10 } 
#for the hyperparameter lambda. Perform 5-fold cross validation using cv.glmnet( ) in the package glmnet 
#on the training set. The data matrix is trainX and the response is saved separately as trainY.
# Which lambda value yields the lowest cross validation error?


head(trainX)
head(trainY)

?cv.glmnet
# output cvm(cross validation error for each lambda)
 
cv_output <- cv.glmnet(
  trainX,
  trainY,
  lambda = c(0.001,0.01,0.1,1,10),
  family = "gaussian",
  alpha = 1
)# for lasso

cv_output$lambda# return lambda values you used

cv_output$cvm# return cross validation error for each lambda
print("The lambda of 1 yields the lowest cross validation error")

best_lambda = cv_output$lambda.min
print(best_lambda)

#2. Build a LASSO model using glmnet( ) function with tuned lambda. 
# How many features are selected by the LASSO model? (How many nonzero coeﬃcients does the model have?)
lasso_best <- glmnet(
  trainX,
  trainY,
  lambda = best_lambda,
  alpha = 1
)

lasso_best_coeffs <- lasso_best$beta
lasso_best_coeffs
print("Nine features, which are Price, Max.Price, EngineSize, RPM, Passengers, Wheelbase, Width, Luggage.room and Weight")

#3. Predict Horsepower on the testing set. Compute the mean squared error. 
# How much horsepower error LASSO makes on average?


lasso_pred <- predict(lasso_best, s = best_lambda, newx = testX)

test_MSE <- mean((lasso_pred - testY)^2)
test_MSE
print("935.1782")

#4. (Ridge Regression) Use training data and build a ridge regression model to predict Horsepower 
#with all of the features. Use lambda = 0.1. In your model, what is the value of the intercept? 
#What is the slope for the features EngineSize?

?glmnet
ridge <- glmnet(
  trainX,
  trainY,
  family = "gaussian",
  alpha = 0,
  lambda = 0.1
)

summary(ridge)
ridge_coeff <- ridge$beta
ridge_coeff
ridge$a0

print("The value of the intercept is 146.3333. The slope for the feature EngineSize is 26.7766607")

#5. How many nonzero coeﬃcients does the model have?
print("17")

#Part2

#6. Implement your own ridge regression method called ridgemodel( ). 
# The function should take input of a data matrix X, a response vector Y , and a hyperparameter lambda. 
# As output, return the coeﬃcients of the ridge regression model.

?cbind
?nrow
?diag


ridgemodel <- function(inputX,inputY,lambda) {
  if (is.data.frame(inputX)) {
    inputX <- data.matrix(inputX)
  }
  if (is.data.frame(inputY)) {
    inputY <- data.matrix(inputY)
  }
  # add bias term
  input_mat <- cbind(1, inputX)
  diag_i <- diag(1,nrow = ncol(input_mat),ncol = ncol(input_mat))
  t_input_mat <- t(input_mat)
  
  coeffs <- inv(t_input_mat%*%input_mat + lambda*diag_i) %*% (t_input_mat %*% inputY)
  return(coeffs)
}

#7. Run your function with the training data and lambda = 0.1. Verify the obtained model is (almost)
# identical to the model from Question 4.

ridgemodel1_coeffs <- ridgemodel(trainX,trainY,0.1)
ridgemodel1_coeffs


# [1,]  -2.0252027
# [2,]   5.0740115
# [3,]   6.8411204
# [4,]  -8.4103336
# [5,]   8.0057799
# [6,]  26.8232437
# [7,]  22.7232307
# [8,]  -0.3769237
# [9,]  -0.5857696
# [10,]  -5.2726649
# [11,] -11.1443166
# [12,] -13.0277913
# [13,]   9.3237087
# [14,]   1.0644307
# [15,]   3.0565320
# [16,]  -4.2338877
# [17,]  37.5298198
# [18,] 146.0898932

#part3

# Gradient descent is an algorithm that ﬁnds a (stationary, local, or global) solution of a minimization 
# problem by iteratively moving along a descending direction of the function. 
# The goal of this part is to apply the algorithm to construct a ridge regression model as an alternative approach.



ridgemodel2 <- function(inputX, inputY, lambda = 0.01, alpha = 0.0005, epics = 1000, isPlot = FALSE){
  if (is.data.frame(inputX)) {
    inputX <- data.matrix(inputX)
  }
  if (is.data.frame(inputY)) {
    inputY <- data.matrix(inputY)
  }
  input_matX <- cbind(1,inputX)
  coeffs <- matrix(0,nrow = ncol(input_matX),ncol = 1)
  deri_per_iter <- list(epics)
  mse_per_iter <- list(epics)
  # print(ncol(coeffs))
  # print(nrow(coeffs))
  # print(ncol(input_matX))
  # print(nrow(input_matX))
  k <- 0
  while (k < epics) {
      t_input_matX = t(input_matX)
      derivative <- -2*(t_input_matX%*%inputY) + 2 * (t_input_matX%*%input_matX%*%coeffs) + 2*lambda*coeffs
      coeffs <- coeffs - alpha*derivative
      deri_per_iter[k+1] = mean(
        abs(derivative)
      )
      mse_per_iter[k+1] <- mean((inputY - input_matX%*%coeffs)^2)
      k <- k+1
  }
  if (isPlot) {
    plot(1:1000,deri_per_iter,type = "p", xlab = "iteration", ylab = "gradient value")
    plot(1:1000,mse_per_iter,type = "p", xlab = "iteration", ylab = "objective value")
  }
  return(coeffs)
}

#8. Verify that the ridge regression model obtained by the above procedure is comparable to the models from Question 4
# and 7.

ridgemodel2_coeffs <- ridgemodel2(trainX,trainY)
print(ridgemodel2_coeffs)

print(ridgemodel1_coeffs)

print(ridge$a0)
print(ridge_coeff)

#9.Plot the objective value at each iteration and the iteration count. 
# (This plot helps us to visualize how the algorithm converges.)
?plot
?data.frame

ridgemodel2_coeffs <- ridgemodel2(trainX,trainY,isPlot = TRUE)

# install.packages("tinytex")
