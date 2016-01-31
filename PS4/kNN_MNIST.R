
#setwd("Desktop/Advanced Computational methods/Advanced-Computational-Methods/")
training <- read.csv("PS4/MNIST/MNIST_training.csv", header = FALSE)
test     <- read.csv("PS4/MNIST/MNIST_test.csv", header = FALSE)

label <- training[,1]
feat  <- training[,2:257]

# Choose k and p using 4-Fold Cross Validation
k <- rep(seq(1,30),3)
p1 <- rep(1, 30)
p2 <- rep(2, 30)
p3 <- rep(Inf, 30)
p  <- c(p1,p2,p3)

# 4 fold Cross-Validation
n <- nrow(training)
fold <- sample(1:4, n, replace=TRUE)


cvpred <- matrix(NA,nrow=n ,ncol=ncol(training))

for (h in 1:90){
  for (i in 1:4){
    l <- kNN(feat[fold==i, ], label[fold!=i], k[h], p[h], memory = feat[fold!=i, ], type="predict")
    
  }
}
  for (i in 1:4)
    cvpred[which(fold==i),k] <- knn.predict(train=which(fold!=i),test=which(fold==i),cl,kdist,k=k)
# display misclassification rates for k=1:10
apply(cvpred,2,function(x) sum(cl!=x))




knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)