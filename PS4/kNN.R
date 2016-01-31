kNN <- function(features, labels, k, p, memory = NULL, type="train"){
 
  # Needed packages
  if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
  if (!require("dplyr")) install.packages("dplyr")
  
  # Verify inputs: assertthat
  not_empty(features)
  if(is.data.frame(features)) features <- as.matrix(features);
  if (type == "train") {
    assert_that(nrow(features) == length(labels))
  }
  is.string(type); assert_that(type %in% c("train", "predict"))
  not_empty(labels)
  is.count(k)
  assert_that(p %in% c(1, 2, Inf))
  if (type == "predict") {
    assert_that(not_empty(memory) & 
                  ncol(memory) == ncol(features) & 
                  nrow(memory) == length(labels))
  }
  
  
  noObs <- nrow(features)
  features <- as.matrix(features)
  
  if (type == "train") {
    
    distMatrix <- matrix(NA, noObs, noObs)
      for (obs in 1:noObs) {
        # getting the probe for the current observation
        probe <- features[obs,]
        probeExpanded <- matrix(probe, nrow = noObs, ncol = 2, byrow = TRUE)
    
        # computing distances between the probe and exemplars 
        if (p %in% c(1,2)) {
          distMatrix[obs, ] <- ( rowSums((abs(features - probeExpanded))^p) )^(1/p)
          } else if (p==Inf) {
            distMatrix[obs, ] <- apply(abs(features - probeExpanded), 1, max)
          }
        }
  } else if (type == "predict") {
    noMemory <- nrow(memory)
    distMatrix <- matrix(NA, noMemory, noObs)
    for (obs in 1:noObs) {
      # getting the probe for the current observation
      probe <- as.numeric(memory[obs,])
      probeExpanded <- matrix(probe, nrow = noMemory, ncol = 2, byrow = TRUE)
    
      # computing distances between the probe and exemplars in the memory
      if (p %in% c(1,2)) {
        distMatrix[obs, ] <- (rowSums((abs(features - probeExpanded))^p) )^(1/p)
        } else if (p==Inf) {
          distMatrix[obs, ] <- apply(abs(features - probeExpanded), 1, max)
        }  
    }
    }
  
  # Finding the neighbors: Sort the distances for each point in increasing numerical order 
  neighbors <- apply(distMatrix, 2, order) %>% t()
  
  # the most frequent class in the k nearest neighbors
  prob       <- apply(as.matrix(neighbors[,1:k]), MARGIN = 1, function(x) max(table(labels[x])/k) ) 
  predLabels <- apply(as.matrix(neighbors[,1:k]), MARGIN = 1, function(x) as.integer(names(which.max(table(labels[x]))) ))

  return(named = list(predLabels = predLabels, prob = prob))
}
