library(MASS)
library(caret)
library(ggplot2)

#install.packages("osqp")
#install.packages('tinytex')
library(osqp)

#install.packages("e1071")
library(e1071)
load("hw3.RData", verbose = TRUE)

str(test)
str(train)

?svm

?solve_osqp( )
?cbind
?rbind

x_test = test[,2:3]
x_test
y_test = test[,1]
x1_train <- train[,2]
x2_train <- train[,3]
y_train <- train[,1]

?svm
model_svm <- svm(Claim~.,data=train, type = "C-classification", kernel = "ridial")
summary(model_svm)

pred <- predict(model_svm, test[,2:3])
confusionMatrix(table(pred,y_test))
plot(model_svm,data = data.frame(Net.Sales = train[,2],Age = train[,3], Claim = factor(train[,1])))

?confusionMatrix()
#confusionMatrix()
?matrix
?cbind
?rep

# 4. Identify Q, A, q and b for the given training data.
#P <- diag(1,3+nrow(train),3+nrow(train))
P <- cbind(
  rbind(diag(1,2),)
)


q <- matrix(c(0,0,0,rep(1,nrow(train))),ncol = 1)
b <- matrix(c(0,0,0,rep(1,nrow(train))),ncol = 1)

first3_a <- matrix(
  c(rep(0,nrow(train)),1,0,0,rep(0,nrow(train)),0,1,0,rep(0,nrow(train)),0,0,1),
  nrow = 3, ncol = 3+nrow(train)
)
last_a <- cbind(x1_train*y_train,x2_train*y_train,y_train,diag(1,nrow(train),nrow(train)))

A <- rbind(first3_a,last_a)

# 5.Find SVM classiﬁer w ∗ and b ∗ by solving the QP form of (1). Use existing QP solvers, e.g., solve osqp( ) in the R-package osqp.

?solve_osqp()
res <- solve_osqp(P,q,A,u = b)
res$x

#w1 = 5.967096e-04
#w2 = -3.920038e-05
#b = -1.181932e-07

#6. Use the result of the previous part to predict Claim for the testing data. What is the c?

w_star = res$x[1:2]
w_star
b_star = res$x[3]
b_star


y_pred_raw = x_test%*%w_star + b_star
y_pred = y_pred_raw
y_pred[y_pred>0] = 1
y_pred[y_pred<0] = -1

confusionMatrix(table(y_pred,y_test))

#the result is 50%


# 7.Make a scatter plot to illustrate predicted outcome. Use Net.Sales as the horizontal axis and Age as the vertical axis. Use diﬀerent colors to represent response classes.
?plot()

plot(train[,2:3],col=factor(train[,1]))


#plot(test[],col=pred+2)

