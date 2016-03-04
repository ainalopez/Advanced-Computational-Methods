
adaBoost <- function(formula, 
                     data, 
                     depth = 5, 
                     noTrees = 5, 
                     newData = NULL){
  
  # Verify that inputs are in a correct format
  if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
  if (!require("rpart")) install.packages("rpart"); library(rpart)
  
  try(if(class(formula) != "formula") stop("Variable formula is not of type formula"))
  is.data.frame(data)
  is.count(depth)
  is.count(noTrees)
  
  # Extract data from formula and data
  d2 <- model.frame(formula = formula, data = data)
  X <- data.frame(d2[,-1])
  y <- data.frame(d2[,1])
  labels <- unique(y[,1])
  
  Y <- y
  Y[y == labels[1]] <- 1
  Y[y == labels[2]] <- -1
  Y <- as.factor(Y[,1])
  
  data2    <- data.frame(Y,X)
  formula2 <- Y ~ .
  
  if(is.null(newData) ){
    newData <- X
  }
  
  N <- length(Y)
  w <- rep(1/N, N)
  
  G     <- list()
  err   <- rep(NA, noTrees)
  alpha <- rep(NA, noTrees)
  final <- rep(0, N)
  
  for( m in 1:noTrees){
    
    # Fit a classifier to the training data using weights
    G[m] <- list( rpart(formula2, 
                        data2, 
                        weights = w, 
                        control=rpart.control(maxdepth = depth)) )  
    
    pred <- predict(G[[m]], X, "class")
    
    # Compute err_m
    err[m] <- sum( w*(pred != Y) )/sum(w)
      
    # Compute alpha_m
    if(err[m] == 0) err[m] <- 1e-50
    alpha[m] <- log((1-err[m])/err[m])
    
    # Update weights
    w <- w * exp(alpha[m] * (pred != Y) )
    
    # Final prediction
    final <- final + alpha[m] * as.numeric(as.character(predict(G[[m]], newData, type = "class")))
  }

  
  predLabels <- final
  predLabels[sign(final) > 0] <- labels[1]
  predLabels[sign(final) < 0] <- labels[2]
  
  # Return named list with predLabels
  return(list(predLabels = predLabels))
}

