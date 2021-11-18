# load data
load("iris.RData")
View(myData)

# plot two classes 
plot(myData[ ,1], 
     myData[ ,2],
     xlab = "Sepal Length",
     ylab = "Sepal Width",
     pch = ifelse(myData[ ,3] == 1, 16, 17), # assign pch = 16 if response = 1
     col = ifelse(myData[ ,3] == 1, "red", "blue")) 

# Pre-process: append column of 1 to the left of myData
# ...
myData <- cbind(c(1),myData)

# Implement Perceptron

# step 1. initialize w, save data matrix and response column as X and Y
# ...
w <- runif(ncol(myData) - 1)
Y <- myData[ ,4]
X <- myData[ ,1:3]

# step 2. iterate your training data 100 times
for (outit in 1:1000){
  # iterate your training data
  for (i in 1:nrow(myData)){
    # update w if y(w^t x) <= 0
    if (Y[i] * X[i, ] %*% w <= 0){
      # update w
      w <- w + Y[i]*X[i, ]
    } else {
      # do not update w
    }
  }
}

# Plot perceptron decision boundary
# Decision boundary: w1*1 + w2*x2 + w3*x3 = 0
# Or equivalently, -w3*x3 = w1 + w2*x2, which is, x3 = (-w1/w3) - (w2/w3)*x2
abline(-w[1]/w[3], -w[2]/w[3]) # intercep, slope





