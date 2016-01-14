# genData() returns a spiral dataset for benchmarking purposes. The output is a dataframe. 
# If csv and plot arguments are specified, then a csv file and a pdf plot are also returned.

genData <- function(noclass = 3, noelements = c(70, 70, 70), center = 0, rad = 1, sd = c(0.2,0.10,0.05), csv = TRUE, plotpdf = TRUE){
  # Install/ load the required packages
  if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
  if (!require("extrafont")) install.packages("extrafont"); library(extrafont)
  loadfonts() 
  
  # Initialize the data frame and the variable a
  dataspiral <- data.frame(x = 0, y = 0, target = 0, classes = 0)
  a <- noclass + 1
  
  for (j in 1:noclass){
    
    # Compute the radius and theta parameters of the spiral
    radius <- seq(center, center+rad, length= noelements[j])
    theta  <- seq(j*a,(j+1)*a, length = noelements[j]) + rnorm(noelements[j], sd = sd[j])
    
    # Compute x and y
    xvar <- radius*sin(theta)
    yvar <- radius*cos(theta)
    
    # create a vector with the name of the class, target, and create the dataframe
    classes    <- rep(paste0("class", toString(j)), noelements[j])
    target     <- rep(j-1, noelements[j])
    ds         <- data.frame(x = xvar, y = yvar, target = target, classes = classes)
    dataspiral <- rbind(dataspiral, ds)
  }
  
  # drop the first line of the data frame, created to initialize it. 
  dataspiral <- dataspiral[-1, ]
  
  # write the csv file
  if(csv){
    write.csv(dataspiral, file = "spiral_data.csv", row.names=FALSE)
  }

  # plot the dataset into a pdf
  if(plotpdf){
    pdf("SpiralData.pdf", family = "Tahoma", width=7, height=7)
    print(ggplot(data = dataspiral, aes(x, y, colour = classes, fill = classes))+ 
    geom_point() + 
    xlab("x") + 
    ylab("y") + 
    ggtitle("Spiral Dataset") + 
    theme_bw() + 
    theme(text = element_text(family = "Tahoma")))
    dev.off()
    embed_fonts("SpiralData.pdf")
  }
  
    
return(dataspiral)
}


# EXAMPLE: genData(noclass = 4, noelements = c(90,80,70,60), sd = c(0.2,0.10,0.05,0.01))

# REFERENCES: 
# http://www.mathworks.com/matlabcentral/fileexchange/41459-6-functions-for-generating-artificial-datasets
# http://cs231n.github.io/neural-networks-case-study/
