
myData <- read.delim("circle.txt", header = FALSE, sep = ",")
plot(myData$V1, myData$V2, pch = 19, cex = 0.5)

# Define mapping phi(x,y) = (x^2, sqrt(2)xy, y^2)
applyPhi <- function(X){
  newX <- matrix(, nrow=nrow(X), ncol=3)
  for (i in 1:nrow(X)) {
    newX[i,1] = X[i,1]^2
    newX[i,2] = sqrt(2)*X[i,1]*X[i,2]
    newX[i,3] = X[i,2]^2
  }
  return(newX)
}

#transform myData
newData <- applyPhi(myData)

# 3-d scatter plot
#install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(newData[,1], newData[ ,2], newData[ ,3], pch = 19, cex.symbols = 0.5)
