# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Profit, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)


#Linear

# Fitting Simple Linear Regression to the Training set
linear_regressor = lm(formula = Salary ~ ., data = dataset)
dataset$Level2 =  dataset$Level^2
dataset$Level3 =  dataset$Level^3
dataset$Level4 =  dataset$Level^4
poly_regressor = lm(formula = Salary ~ ., data = dataset)

linear_pred = predict(linear_regressor, newdata = dataset)
poly_pred = predict(poly_regressor, newdata = dataset)
# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(linear_regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

#Polynomial 


