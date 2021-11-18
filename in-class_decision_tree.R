# install.packages("tree")
library(tree)
library(randomForest)
load("Titanic.RData")

head(train)
head(test)
head(validation)


?tree

tree_model <- tree(
  Survived ~ .,
  train
)

plot(tree_model)
text(tree_model, cex=0.7)

errPerSize <- NULL
for (s in 5:10){
  m<- prune.tree(tree_model, best = s)
  pred <- predict(m, validation, type = "class")
  errPerSize[s] <- mean(pred != validation$Survived)
}
errPerSize #lowest error with 7-8 terminal nodes

?randomForest
random_tree_model <- randomForest(
  Survived ~ .,
  train,
  mtry = 3
)
plot(random_tree_model)


