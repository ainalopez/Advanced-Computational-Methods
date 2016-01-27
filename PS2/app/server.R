# loading libraries
library(ggplot2)
library(mvtnorm)

shinyServer(function(input, output, session) {
  
  # Define functions
  sigmaXY <- function(rho, sdX, sdY) {
    covTerm <- rho * sdX * sdY
    VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2),2, 2, byrow = TRUE)
    return(VCmatrix)
  }
  
  genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
    if(!is.na(seed)) set.seed(seed)
    rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
    return(rdraws)
  }
  
  loanData <- function(noApproved = 50, noDenied = 50, muApproved, muDenied, sdApproved, 
                       sdDenied, rhoApproved=-0.1, rhoDenied= 0.6, seed=1111) {
    sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
    sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
    approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
    denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
    loanDf <- as.data.frame(rbind(approved,denied))
    deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
    target = c(rep(0, noApproved), rep(1, noDenied))
    loanDf <- data.frame(loanDf, deny, target)
    colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
    return(loanDf)
  }
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    loanData(muApproved = c(input$muAX, input$muAY), muDenied =c(input$muDX, input$muDY), 
             sdApproved = c(input$sdAX, input$sdAY), sdDenied = c(input$sdDX, input$sdDY))
  })
  
  # compute the fitting
  Datafit <- reactive({
    datafit <- lm(target ~ solvency + PIratio + 1, data=selectedData())
  })
  
  # Compute boundaries
  Boundaries <- reactive({
    weights <- coef(Datafit())[c("solvency", "PIratio")]
    intercept <- (-coef(Datafit())[1] + 0.5)/weights["PIratio"]
    slope <- -(weights["solvency"]/weights["PIratio"])
    return(data.frame(intercept = intercept, slope = slope))
  })


  # Compute the confusion matrix
  ConfMatrix <- reactive({
    predictedLabels <- ifelse(predict(Datafit()) < 0.5, "Approved", "Denied")
    confMatrixFreq <- table(selectedData()$deny, predictedLabels)
    
    return(confMatrixFreq)
  })
 
  # serving the plot of the data
  output$plot1 <- renderPlot({
    ggplot(data = selectedData(), 
           aes(x = solvency, y = PIratio, colour=deny, fill=deny)) + 
      geom_point() +
      xlab("solvency") +
      ylab("PIratio") +
      theme_bw() + 
      geom_abline(intercept = Boundaries()$intercept, slope = Boundaries()$slope)
  })
  
  # serving the table of the data
  output$table1 <- renderTable({
    ConfMatrix()
  })
  
})