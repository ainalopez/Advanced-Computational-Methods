
# Loss functions definitions
MissError <- function(prob) {
  MissError <- 1 - apply(prob,2,max)
  return(MissError) 
}
Gini <- function(prob) {
  Gini <- colSums(prob*(1-prob))
  return(Gini)
}
CrossEntropy <- function(prob) {
  CrossEntropy <- - colSums(prob*log(prob))
  return(CrossEntropy)
}

# Compute overall error
ComputeGain <- function(regions, costFnc, labels){
  
  cumErr <- 0
  for ( r in 1:length(unique(regions)) ){
    lab <- labels[regions == r]
    prop <-  rep(NA, length(unique(labels)))
    
    # compute proportions of each label in each region
    for(j in 1:length(unique(labels))){
      prop[j] <- 1/length(lab) * sum( lab == unique(labels)[j])
      
      # In order to don't have problems with cross Entropy   
      if(prop[j] == 0) prop[j] <- 0.00000000000001
    }
    
    prop <- as.matrix(prop)
    
    if(costFnc == "ME"){
      cumErr <- cumErr + length(lab)*MissError(prop)
    } else if (costFnc == "Gini"){
      cumErr <- cumErr + length(lab)*Gini(prop)
    } else if (costFnc == "Entropy"){
      cumErr <- cumErr + length(lab)*CrossEntropy(prop)  
    }
  }
  
  return(cumErr)
}

# Find the threshold for a 1d data
findThreshold <- function(x, y, costFnc, minPoints) {
  noPoints    <- length(x)
  labels      <- unique(y)
  nclass      <- length(labels)
  gain        <- rep(10000, noPoints-1)
  thresholds  <- rep(NA, noPoints-1)
  splitLabels <- matrix(labels[1], ncol=2, nrow=noPoints-1)
  
  # we go sequentially over each point and cut between that point and the
  # closest neighbor
  for (idx in (minPoints):(noPoints-minPoints)) {
    
    # locate a potential threshold, a split between two points
    potThres <- mean(x[idx:(idx+1)])
    
    # check the classification error, when both sides, are classified with mean label
    predictedClasses <- rep(NA, noPoints)
    
    xl <- y[x < potThres]
    xr <- y[x >= potThres]
    
    # Compute proportion of class in each split
    propl <- rep(NA, nclass)
    propr <- rep(NA, nclass)
    
    for(j in 1:nclass){
      propl[j] <- 1/length(y[x <  potThres]) * sum( xl == labels[j])
      propr[j] <- 1/length(y[x >= potThres]) * sum( xr == labels[j])
      
      if(length(y[x <  potThres]) == 0) propl[j] <- 0.00000000000001
      if(length(y[x >=  potThres]) == 0) propr[j] <- 0.00000000000001
      
      # In order to don't have problems with cross Entropy   
      if(propl[j] == 0) propl[j] <- 0.00000000000001
      if(propr[j] == 0) propr[j] <- 0.00000000000001
      
      
      if(j > 1){
        if(propl[j] > propl[j-1]){
          splitLabels[idx, 1] <- as.character(labels[j])
        }
        if(propr[j] > propr[j-1]){
          splitLabels[idx, 2] <- as.character(labels[j])
        }
        
      }
    }
    
    # Compute probabilities of each class and the error
    propr <- as.matrix(propr)
    propl <- as.matrix(propl)
    
    if(costFnc == "ME"){
      gain[idx] <- length(xr)*MissError(propr) + length(xl)*MissError(propl)
    } else if (costFnc == "Gini"){
      gain[idx] <- length(xr)*Gini(propr) + length(xl)*Gini(propl) 
    } else if (costFnc == "Entropy"){
      gain[idx] <- length(xr)*CrossEntropy(propr) + length(xl)*CrossEntropy(propl)  
    }
    
    thresholds[idx] <- potThres
  }
  
  # next we find the minimum and the best threshold
  indexmax      <- which.min(gain)
  bestThreshold <- thresholds[indexmax]
  cl            <- splitLabels[indexmax, 1]
  cr            <- splitLabels[indexmax, 2]
  
  return(list(thres = bestThreshold, labels = c(cl,cr)))
}

# Function that recursively finds the best split
recursiveTree <- function(X, Y, costFnc, depth, minPoints, regions, k, labels, thresInfo){
  
  # Check if we have reached the maximum number of nodes
  if( k >= depth & k != 1){
    return (list(regions = regions, thresInfo = thresInfo, labels = labels) )
  }
  
  gainR <- rep(100000, length(unique(regions)))
  threshR <- rep(NA, length(unique(regions)))
  dimensR <- rep(NA, length(unique(regions)))
  labelsR <- matrix(NA, ncol = 2, nrow = length(unique(regions)))
  
  # iterate over all regions
  for(reg in 1:length(unique(regions)) ){
    
    cX <- data.frame(x = X[ regions== reg, ])
    cY <- data.frame(y = Y[ regions== reg, ])
    
    # if there are less or equal points than minPoints, go to the next region
    if( dim(cX)[1] <= minPoints){
      next
    }
    
    # if there are only labels of one class, go to the next region
    if( length(unique(cY[,])) == 1 ){
      next
    }
    
    minGain   <- 100000
    bestDim   <- NA
    bestThres <- NA
    bestLabel <- NA
    
    # Loop over all dimensions
    for(d in 1:dim(X)[2]){
      x <- cX[, d]
      xord <- order(x)

      # Find the best threshold
      result <- findThreshold(x[xord], cY[xord,], costFnc, minPoints)

      # Compute the overall error
      provregions <- regions
      
      provregions[X[, d] > result$thres & regions == reg] <- max(unique(regions))+1
      gain <- ComputeGain(provregions, costFnc, Y[,1])

      
      if(gain < minGain){
        minGain   <- gain
        bestDim   <- d
        bestThres <- result$thres
        bestLabel <- result$labels
      }
      
    }
    
    # Choose the best option for each region
    gainR[reg]    <- minGain
    threshR[reg]  <- bestThres
    dimensR[reg]  <- bestDim
    labelsR[reg,] <- bestLabel
  }
  
  if( sum(gainR) == 100000* length(unique(regions))){
    return (list(regions = regions, thresInfo = thresInfo, labels = labels))
  }
  
  # Choose the best region split
  SplitRegion <- which.min(gainR)

  # Update parameters
  if(k == 1){
    labels <- c( labelsR[SplitRegion,])
    thresInfo <- data.frame(node = 1, threshold = threshR[SplitRegion], Dimension =  dimensR[SplitRegion], parentRegion = SplitRegion)
    
  }else{
    labels[SplitRegion] <- labelsR[SplitRegion,1]
    labels <- c(labels, labelsR[SplitRegion,2])
  
    n <- max(unique(regions))+1
    t <- threshR[SplitRegion]
    d <- dimensR[SplitRegion]
    p <-  SplitRegion
    thresInfo <- rbind(thresInfo, c(n,t, d, p))
  } 
  
  dsr <- dimensR[SplitRegion]
  tsr <- threshR[SplitRegion]
  regions[X[, dsr] > tsr & regions == SplitRegion] <- max(unique(regions))+1
  
  k <- k + 1
  
  return (recursiveTree(X, Y, costFnc, depth, minPoints, regions, k, labels, thresInfo))
}



# Main Function: calls the recursive tree and computes the labels and probability of each label
cTree <- function(formula, data, depth = 4, minPoints = 4, costFnc = "Entropy", predictData=NULL){
  
  # test the inputs
  library(assertthat)
  
  try(if(class(formula) != "formula") stop("Variable formula is not of type formula"))
  is.string(costFnc); assert_that(costFnc %in% c("ME", "Gini", "Entropy"))
  is.data.frame(data)
  is.count(depth)
  is.count(minPoints)
  
  # Extract data from formula and data
  d2 <- model.frame(formula = formula, data = data)
  X <- data.frame(d2[,-1])
  Y <- data.frame(d2[,1])
  
  regions    <- rep(1, length(X[,1]))
  k          <- 1
  labels     <- data.frame() 
  threshInfo <- data.frame()
  
  res <- recursiveTree(X, Y, costFnc, depth, minPoints, regions, k, labels, threshInfo)
  
  
  labels <- res$labels 
  info   <- res$thresInfo
  
  
  # Compute predLabels and prob
  predLabels <- rep(NA, length(Y[,1]))
  prob       <- rep(NA, length(Y[,1]))
  
  if(is.null(predictData)){
    finalRegions <- res$regions
  } else {
    # Create Regions 
    finalRegions <- rep(1, dim(predictData)[1])
    reg <- 1
    
    for(i in 1:( max( c(1 , (max(info$node)-1)) ) )){
      
      d <- info$Dimension[i]
      r <- info$parentRegion[i]
      t <- info$threshold[i]

      finalRegions[predictData[ ,d] > t & finalRegions == r] <- reg + 1
      
      reg <- reg + 1
    }
  } 
  
  for(i in 1:length(unique(finalRegions))){
    predLabels[finalRegions == i] <- labels[i]
    prob[finalRegions == i] <- 1/length(Y[regions == i, ])* sum(Y[regions == i, ] == labels[i])
  }
  
  return( list(predLabels = predLabels, prob = prob) ) 
}


