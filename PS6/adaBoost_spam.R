source("adaBoost.R")

if (!require("adabag")) install.packages("adabag"); library(adabag)
if (!require("ggplot2")) install.packages("ggplot2");library(ggplot2)
if (!require("rpart")) install.packages("rpart");library(rpart)

set.seed(123)
SPAM <- read.csv("spambase.data")

# Split the dataset into training and test
smpSize <- floor(0.75 * nrow(SPAM))
index   <- sample(seq(1, nrow(SPAM)), smpSize, replace = FALSE, prob = NULL)

trainData  <- SPAM[index,]
testData   <- SPAM[-index,-58]
testLab    <- SPAM[-index,58]

trainData2 <- trainData
trainData2$X1 <- as.factor(trainData2$X1)

# Sequence of number of trees and formula
M <- seq(from = 1, to = 30, by = 2)
formula <- X1 ~ .
formula2 <- as.factor(X1) ~ .

# Training and test errors
errABtest   <- rep(NA, length(M))
errABtrain  <- rep(NA, length(M))
errADABAGtest  <- rep(NA, length(M))
errADABAGtrain <- rep(NA, length(M))


for (nt in 1:length(M)){
  
  lab1 <- adaBoost(formula, trainData, depth = 10, noTrees = M[nt], newData = NULL)
  errABtrain[nt] <- mean(lab1$predLabels != trainData[,58])
  
  lab2 <- adaBoost(formula, trainData, depth = 10, noTrees = M[nt], newData = testData)
  errABtest[nt] <- mean(lab2$predLabels != testLab)

  boost <-boosting(formula ,
                     data = trainData2, 
                     mfinal = M[nt],
                     coeflearn = "Freund",
                     control=rpart.control(cp= -1, minsplit = 0, maxdepth=10))
  
  errADABAGtrain[nt]  <- mean(as.numeric(predict(boost, trainData)$class) != trainData[,58])
  errADABAGtest[nt]   <- mean(as.numeric(predict(boost, testData)$class) != testLab)
}


# Plot
cl <- c( rep("adaBoost", 2*length(M)), rep("adabag", 2*length(M)))
tt <- c(rep("Training", length(M)), rep("Test", length(M)), rep("Training", length(M)), rep("Test", length(M)))
errors <- c(errABtrain, errABtest, errADABAGtrain, errADABAGtest)

dataPlot <- data.frame(noTrees = c(M, M), Errors = errors, Class = cl, Action = tt)

pdf("adaBoost.pdf")
ggplot(data=dataPlot, aes(x=noTrees, y=Errors, colour = Class, linetype = Action, group = Action:Class)) +
  geom_line() +
  geom_point( size=4, shape=21, fill="white") + 
  ggtitle("Evolution of training and test errors")
dev.off()
