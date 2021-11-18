# 8/31/2021
# R Introduction Part 2
# Miju Ahn

# Matrix 
X <- matrix(1:6, nrow = 2, ncol = 3)
Y <- matrix(100:105, nrow = 2, ncol = 3)
X[1,2] # 1st row, 2nd col component 
X[ ,2] # 2nd column
X[2, ] # 2nd row 
X[ ,2:3] # all rows, col 2 and 3 only

# Matrix multiplication (X: 2 by 3, t(Y): 3 by 2)
X %*% t(Y) #  normal matrix multiplication 
X * Y # element-wise multiplication 

# Data frame (A matrix with different data types)
names <- c("A", "B", "C")
values <- c(3, 5, 2)
df <- data.frame(names, values)
df
df$values # access columns by their name

# if 
z <- 100
if(z < 200){
  print("the number is less than 200")
}else{
  print("the number is no less than 200")
}

# for-loop
for (i in 1:10){
  print(i)
}

# while-loop
while(z < 110){
  print(z)
  z <- z + 1
}

# define a function 
incNum <- function(x){
  x + 1
}
incNum(5)

# 1. 
?runif
m <- matrix(runif(25), nrow = 5, ncol = 5)


# 2. 
n <- m[1:3, 1:3] 

# 3. 
n[1, ] <- 0 # one way
for (i in 1:3){ # another way
  n[1,i] <- 0
}

# 5. 
compAvg <- function(x){
  sum(x)/length(x)
}

# 6. 
printNum <- function(x){
  for (i in 1:length(x)){
    if (x[i] < 5){
      print(x[i])
    }
  }
}
