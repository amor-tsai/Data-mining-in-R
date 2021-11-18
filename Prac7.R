# Decision boundary for K-nearest neighbor

# load breast cancer data, use columns radius, texture, and diagnosis
myData <- read.csv("breast_cancer.csv")
colnames(myData)
myData <- myData[ ,c(1,2,6)]

# plot features
plot(myData$mean_radius, myData$mean_texture)

# add colors for response
myCol <- rep("orange", nrow(myData))
myCol[myData$diagnosis == 0] <- "blue" # cancer: orange, benign:blue
myCol[1:10]
myData$diagnosis[1:10]

plot(myData$mean_radius, myData$mean_texture,
     col = myCol,
     xlab = "radius",
     ylab = "texture",
     main = "breast cancer data set",
     pch = 19,
     cex = 0.5
     )
legend("topright",
  legend = c("cancer","benign"),
  col = c("orange","blue"),
  pch = 19
  )

#decision boundary for K-nearest neighbor

#create synthetic testing set
x <- seq(0, 30, by = 0.5) # 0, 0.5, 1, ..., 30
y <- seq(0, 40, by = 0.5) 
grid <- expand.grid(x,y)
myTest <- data.frame(mean_radius = grid$Var1,
                     mean_texture = grid$Var2)
# apply KNN
library(FNN)
pred <- knn(myData[ ,1:2], myTest, myData[ ,3], k=3)

# visualize
plot(myTest$mean_radius, myTest$mean_texture,
     col = pred,
     main = "3-NN Decision Boundary",
     xlab = "radius",
     ylab = "texture"
     )

?knn





