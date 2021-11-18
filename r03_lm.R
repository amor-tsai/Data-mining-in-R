# Install packages 
install.packages("MASS") # install once
library("MASS")

# Load data from MASS
data("birthwt")
myData <- birthwt # save dataset as myData
?birthwt
head(birthwt) # print first 10 rows

# Scatter plot mom's weight vs baby's weight
plot(myData$lwt, myData$bwt,
     xlab = "Mother's weight",
     ylab = "Baby's weight",
     main = "Scatter Plot",
     cex = 0.5, # decrease size of marker
     pch = 4, # change shape of maker (4: x),
     col = "green")

# Babies with weight under 2.5 
# Use column "low"
table(myData$low) # count per value

# Plot histogram for mothers' age distribution
hist(myData$age)
?hist
hist(myData$age, 
     breaks = seq(10, 60, by = 5),
     xlab = "Mother's age",
     ylab = "Count",
     main = "Age Distribution")

# Build a linear model to predict bwt (baby weight) using lwt(mom's weight), age
?lm
m1 <- lm(bwt ~ lwt + age, data = myData)
summary(m1) # bwt = 2214.412 + 4.177 * lwt + 8.089 * age 

m2 <- lm(bwt ~ . - low, data = myData) # . means all features
summary(m2)

# Can you build a better model using only significant features? 










