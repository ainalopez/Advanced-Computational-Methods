source("cTree.R")
library(rpart)
library(ggplot2)

#findThreshold <- cmpfun(findThreshold)
set.seed(123)

SPAM <- read.csv("Downloads/Spam/spambase.data")

# Split the dataset into training and test
smpSize <- floor(0.75 * nrow(SPAM))
index   <- sample(seq(1, nrow(SPAM)), smpSize, replace = FALSE, prob = NULL)

trainData    <- SPAM[index,]
trainData$X1 <- as.factor(trainData$X1)
trainLab     <-  SPAM[index,58]
testData     <- SPAM[-index,-58]
testLab      <-  SPAM[-index,58]


# Training and test errors
K <- seq(from = 1, to = 15, by = 2)
formula <- X1 ~ .

errCTtest <- rep(NA, length(K))
errCTtrain <- rep(NA, length(K))
errRPtest <- rep(NA, length(K))
errRPtrain <- rep(NA, length(K))


for (dep in 1:length(K)){
  
  res <- cTree(formula, data = trainData, depth = K[dep], minPoints = 4, costFnc = "Gini", testData)
  errCTtest[dep] <- sum(res$predLabels != testLab)/length(testLab)
  
  res2 <- cTree(formula, data = trainData, depth = K[dep], minPoints = 4, costFnc = "Gini")
  errCTtrain[dep] <- sum(res2$predLabels != trainData[,58])/length(trainData[,58])
 
  fit <- rpart(formula, data = trainData, method = "class", parms = list(split = "gini"), control = rpart.control(minsplit = 4, maxdepth = K[dep]))
  pre1 <- predict(fit, trainData[,-58], type = "class")
  errRPtrain[dep] <- sum((as.double(pre1) - 1) != trainData[,58])/length(trainData[,58])
  
  pre <- predict(fit, testData, type = "class")
  errRPtest[dep] <- sum((as.double(pre)-1) != testLab)/length(testLab)

}


# Plot
cl <- c( rep("CTree", 2*length(K)), rep("rPart", 2*length(K)))
tt <- c(rep("Training", length(K)), rep("Test", length(K)), rep("Training", length(K)), rep("Test", length(K)))
errors <- c(errCTtrain, errCTtest, errRPtrain, errRPtest)

dataPlot <- data.frame(Depth = c(K, K), Errors = errors, Class = cl, Action = tt)

pdf("cTree.pdf")
ggplot(data=dataPlot, aes(x=Depth, y=Errors, colour = Class, linetype = Action, group = Action:Class)) +
  geom_line() +
  geom_point( size=4, shape=21, fill="white") + 
  ggtitle("Evolution of training and test errors")
dev.off()