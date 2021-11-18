# 8/26/2021
# Data Mining R first session 

# Tips: to run single line
# command + enter (Mac)
# control + enter (Windows)

print("Hello World")

# vectors 
x <- c(-3,1,5)
x + 1
1/x

y <- c(-1,9,10)
x + y
c(x,y)

# some functions: min, max, sum, mean
mean(x)
?min

# logical vector
x < 3
z <- x < 3 # z is a logical vector 

# character vector 
q <- c("orange", "apple", "kiwi")

# access components of vector
x[3] # 3rd comp
x[1:2] # 1st and 2nd components
x[x < 3] # returns all comp that correspond to TRUE val

# 1. 
x <- c(3,12,6,-5,0,8,15,1,-10,7)
# 2. 
str(x)
# 3. 
?seq
evenIdx <- seq(from = 2, to = 10, by = 2)
x[evenIdx] - 5
# 4. 
sum(x)
mean(x)
# 5. 
?rev
rev(x)
x[10:1]
# 6. 
x < 0 
# 7. 
# x[x < 0] <- 0 # assign 0 for all neg. comp.
x <- x[x >= 0] # only keep nonneg. comp.
# 8.
length(x)
# 9.
rm(x)
?typeof
typeof(z)
















