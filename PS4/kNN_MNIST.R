library("class")
# setwd("Desktop/Advanced Computational methods/Advanced-Computational-Methods/")
training <- read.csv("PS4/MNIST/MNIST_training.csv", header = FALSE)
test     <- read.csv("PS4/MNIST/MNIST_test.csv", header = FALSE)

label <- training[,1]
feat  <- training[,2:257]

# Choose k and p using 4-Fold Cross Validation
k <- rep(seq(1,61,2))
lk <- length(k)

# 4 fold Cross-Validation
n <- nrow(training)
fold <- sample(1:4, n, replace=TRUE)

acc <- rep (NA, lk)
cvpred <- matrix(NA,nrow=n ,ncol=ncol(training))

for (h in 1:lk){
  ac <- rep(NA,4)
  for (i in 1:4){
    l <- knn(train = feat[fold!=i, ], test = feat[fold==i, ], cl = label[fold!=i], k = 101)
    ac[i] <- (1/length(l)) * sum(label[fold==i] == l)*100
  }
  acc[h] <- mean(ac)
}


# The best accuracy achieved has been with: k = 47.
#k.max <- k[which.max(acc)]
k.max <- 47

predlabel <- knn(train = feat, test = test, cl = label, k = k.max)
#displayDigit(as.numeric(test[2,]), as.numeric(predlabel[2]))
#displayDigit(as.numeric(training[2,2:257]), as.numeric(training[2,1]))
write.csv(predlabel, "MNIST_predictions.csv", row.names = FALSE)
