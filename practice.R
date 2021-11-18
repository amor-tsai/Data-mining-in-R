X <- matrix(1:6, nrow = 2, ncol = 3)
Y <- matrix(100:105, nrow = 2, ncol = 3)
X[1,2]

X %*% t(Y) # normal matrix multiplication
X * Y # element-wise multiplication

# Data frame(A matrix with different data types)
a <- c("A","B","C")
b <- c(3,5,2)
df <- data.frame(a,b)
df


#1
m <- matrix(runif(25,min=0,max=1),nrow = 5, ncol = 5)

#2
n <- m[1:3,1:3]

#3
n[1, ] <- 0

#4
?mean

printNum <- function(x){
  for (i in 1:length(x))
    print(x[i])
}

printNum(m)

