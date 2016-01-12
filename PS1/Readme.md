
# Problem Set 1

#### EXERCISE 1
genData() returns a spiral dataset for benchmarking purposes. The output is a dataframe. If csv and plot arguments are specified, 
then a csv file and a pdf plot are also returned. 

###### Parameters

The parameters of the function are:

* noclass: Number of classes of the dataset. By default 3. 
* noelements: A vector with the number of elements for each class. The size of this vector must be equal to the number of classes. By default: 70, 70, 70.
* center: center of the spiral. By default 0 .
* radius: radius of the spiral. By default 1.
* sd: A vector with the standar deviation of each class. It should take values from 0 to 1, taking into account that with more classes, large values can imply overlap between classes. By default:  0.2,0.10,0.05. 
* csv: if TRUE (by default), writes the dataset into a csv file called *spiral_data.csv*.
* plotpdf: if TRUE (by default), plots the dataset into a pdf called *SpiralData.pdf*.

###### Examples
genData(noclass = 4, noelements = c(90,80,70,60), sd = c(0.2,0.10,0.05,0.01))

genData()

###### References
http://www.mathworks.com/matlabcentral/fileexchange/41459-6-functions-for-generating-artificial-datasets
http://cs231n.github.io/neural-networks-case-study/
