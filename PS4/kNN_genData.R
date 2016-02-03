if (!require("mvtnorm")) install.packages("mvtnorm")
if (!require("ggplot2")) install.packages("ggplot2")

dataset <- genSun(n = 400, features = 2, seed = 1111, saveData = FALSE, savePlot = FALSE) 

features <- as.matrix(cbind(dataset$x1,dataset$x2))
labels   <- as.numeric(as.vector(dataset$y))

# compute KNN
knn.res <- kNN(features, labels, k = 1, p = 1, type = "train")
df <- data.frame(dataset, knn.res)

# Write a csv file called: predictions.csv
write.csv(df, file = 'predictions.csv', row.names = FALSE)


# Plot the dataset and the decision boundaries

# Define a new grid of data
xmin <- min(dataset$x1); xmax <- max(dataset$x1)
ymin <- min(dataset$x2); ymax <- max(dataset$x2)

x1.n     <-seq(xmin, xmax, len=100) 
x2.n     <-seq(ymin, ymax, len=100) 
x1.new   <-rep(x1.n, 100) 
x2.new   <-rep(x2.n, rep(100,100)) 
y        <- rep(1, length(x1.new))

knn.res.db <- kNN(cbind(x1.new,x2.new), labels, k = 3, p = 1, memory = features, type = "predict")
pred       <-knn.res.db$predLabels

datafr <- data.frame(x1.new, x2.new)

pdf("dataPlot.pdf")
  ggplot(data = datafr, aes(x = x1.new, y = x2.new, colour = y)) +
    geom_point(size = 0.3)+
    geom_point(data = dataset, aes(x = x1, y = x2, colour = labels, fill = labels) )+
    xlab('x1') +
    ylab('x2') +
    stat_contour( aes(x = x1.new, y = x2.new, z = pred), binwidth = 1) +
    theme_bw()
dev.off()
