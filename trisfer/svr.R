# SVR

# Importing the dataset
dataset = read.csv('flights_2.csv')
#dataset = dataset[2:3]
dataset$amountPerMinute <- dataset$amountAfterTax /  dataset$duration
dataset$amountXMinute <- dataset$amountAfterTax *  dataset$duration

dataset$amountAfterTaxScale <- scale(dataset$amountAfterTax)
dataset$durationScale <- scale(dataset$duration)


#Histogram
x = dataset$amountAfterTax
h<-hist(x, breaks=20, col="red", xlab="xlab",  main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit<-yfit*diff(h$mids[1:2])*length(x)*0.85 
lines(xfit, yfit, col="blue", lwd=2)

#Density
d <- density(x)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")

##----------use Kmeans
# kmdataset = dataset[1:2]
# kmeans = kmeans(x = kmdataset, centers = 3)
# y_kmeans = kmeans$cluster
# library(cluster)
# clusplot(kmdataset, y_kmeans,
#          lines = 0,shade = TRUE, color = TRUE, labels = 2,plotchar = TRUE,
#          span = TRUE, main = paste('Clusters of customers'), xlab = 'Amount', ylab = 'Duration')

## --------another clustering algo:
# kmdataset = dataset[1:2]
# dendrogram = hclust(d = dist(kmdataset, method = 'euclidean'), method = 'ward.D')
# plot(dendrogram,
#      main = paste('Dendrogram'),
#      xlab = 'Amount',
#      ylab = 'Duration')


# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting SVR to the dataset
# install.packages('e1071')
library(e1071)
regressor = svm(formula = Salary ~ .,
                data = dataset,
                type = 'eps-regression',
                kernel = 'radial')

# Predicting a new result
y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the SVR results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$amountAfterTax, y = dataset$duration),
             colour = 'red') +
  #geom_point(aes(x = dataset$amountAfterTax, y = dataset$connectionCount),
  #          colour = 'blue') +
  ggtitle('Truth or Bluff (SVR)') +
  xlab('Amount') +
  ylab('Duration')

# Visualising the SVR results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (SVR)') +
  xlab('Level') +
  ylab('Salary')