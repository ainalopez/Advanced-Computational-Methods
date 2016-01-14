if (!require("extrafont")) install.packages("extrafont"); library(extrafont)
if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
# font_import()
# load the fonts into R session
loadfonts()


# create small wrapper functions
sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 2, 2, byrow = TRUE)
 
  return(VCmatrix) 
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) { 
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY) 
  return(rdraws)
}

# loanData function modified in order to add a third class: undecided
loanData3 <- function(noApproved, noDenied, noUndecided, muApproved, muDenied, muUndecided, sdApproved,
                     sdDenied, sdUndecided, rhoApproved, rhoDenied, rhoUndecided, seed=1111) {
  
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  sigmaUndecided <- sigmaXY(rho=rhoUndecided, sdX=sdUndecided[1], sdY=sdUndecided[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed = seed+2)
  loanDf <- as.data.frame(rbind(approved,denied, undecided))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied), rep("Undecided", noUndecided)) 
  target = c(rep(0, noApproved), rep(1, noDenied), rep(2, noUndecided))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target") 
  
  return(loanDf)
}

# Generate the 3-class dataset
noApproved  <- 50
noDenied    <- 50
noUndecided <- 50

LoanD3 <- loanData3(noApproved, noDenied, noUndecided, c(4, 150), c(10,100), c(8,230) ,
                    c(1,20), c(2,30), c(0.5,5), -0.1, 0.6, 0.5, 1221)

# add target variable
LoanD3C <- cbind(LoanD3,
                target1 = c(rep(0, noApproved), rep(1, noDenied), rep(0, noUndecided)),
                target2 = c(rep(1, noApproved), rep(0, noDenied), rep(0, noUndecided)),
                target3 = c(rep(0, noApproved), rep(0, noDenied), rep(1, noUndecided))
                )

# analytical solutions
X <- as.matrix(cbind(ind=rep(1,nrow(LoanD3C)),
                     LoanD3C[,c("PIratio","solvency")]))

Y <- cbind(denied_prob = c(rep(0, noApproved), rep(1, noDenied), rep(0, noUndecided)),
           approved_prob = c(rep(1, noApproved), rep(0, noDenied), rep(0, noUndecided)),
           undecided_prob = c(rep(0, noApproved), rep(0, noDenied), rep(1, noUndecided))
           )

weightsOptim <- solve(t(X) %*% X) %*% t(X) %*% Y

# Compute predictions
predictions <- X %*% weightsOptim

denied    <- (predictions==apply(predictions, 1, max))[,1]
approved  <- (predictions==apply(predictions, 1, max))[,2]
undecided <- (predictions==apply(predictions, 1, max))[,3]

predictedLabels <- ifelse(denied, "Denied", "Approved")
predictedLabels[undecided == TRUE] <- "Undecided"
  

# Create the final dataframe: dataset + predictions for each class and final decission
LoanD3 <- cbind(LoanD3, predictions, predictedLabels)

# Save the dataset in a csv file called predictions.csv
write.csv(LoanD3, file = "predictions.csv", row.names=FALSE)


# Plot of the data and decision boundaries in pdf: discFunction3C.PDF

# decision boundary 1: class 1 vs class 2
intercept1 <- (weightsOptim[1,1] - weightsOptim[1,2])/(weightsOptim[3,2] - weightsOptim[3,1])
slope1     <- (weightsOptim[2,1] - weightsOptim[2,2])/(weightsOptim[3,2] - weightsOptim[3,1])
x1         <- seq(min(LoanD3["PIratio"]), max(LoanD3["PIratio"]), length.out = nrow(LoanD3))
y1         <- slope1*x1 + intercept1
boundary1 <- data.frame(PIratio = x1, solvency = y1, deny = rep("Denied vs Approved"))

# decision boundary 2: class 1 vs class 3
intercept2 <- (weightsOptim[1,1] - weightsOptim[1,3])/(weightsOptim[3,3] - weightsOptim[3,1])
slope2     <- (weightsOptim[2,1] - weightsOptim[2,3])/(weightsOptim[3,3] - weightsOptim[3,1])
y2         <- slope2*x1 + intercept2
boundary2  <- data.frame(PIratio = x1, solvency = y2, deny = rep("Denied vs Undecided"))

# decision boundary 3: class 2 vs class 3
intercept3 <- (weightsOptim[1,3] - weightsOptim[1,2])/(weightsOptim[3,2] - weightsOptim[3,3])
slope3     <- (weightsOptim[2,3] - weightsOptim[2,2])/(weightsOptim[3,2] - weightsOptim[3,3])
y3         <- slope3*x1 + intercept3
boundary3  <- data.frame(PIratio = x1, solvency = y3, deny = rep("Undecided vs Approved"))

# plot
pdf("discFunction3C.pdf", family = "Tahoma", width=6, height=5)
ggplot(data = LoanD3,
       aes(x = solvency, y = PIratio, colour = deny)) + 
  geom_point() + 
  guides(shape=FALSE) +
  xlab("solvency") + 
  ylab("PIratio") + 
  ggtitle("Loan data and decision boundaries") + 
  geom_line(data = boundary1) + 
  geom_line(data = boundary2) + 
  geom_line(data = boundary3) + 
  scale_color_manual("", 
                     values = c("Approved" = "red", "Denied" = "green", "Undecided" = "blue", "Denied vs Approved"= "yellow",
                              "Denied vs Undecided"  = "cyan", "Undecided vs Approved" = "magenta"))
dev.off()
embed_fonts("discFunction3C.pdf")
