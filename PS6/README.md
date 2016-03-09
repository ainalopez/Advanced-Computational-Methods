# PS5

#### adaBoost function 
The parameters data and newData should be data frames. Data is the dataframe for training and newData (if not NULL) will be the data that the algorithm will use to predict. 

Furthermore, in the formula, the labels of the target should not be defined as factors. That is, instead of as.factor(y) ~ . , you should write y ~ .



#### adaBoost_spam.R

Notice that the script can take one hour to run. If not desired, define a lower number of number of trees (variable M in line 22).
  