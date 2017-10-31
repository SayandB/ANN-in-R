#Importing Liibraries
library(neuralnet)
library(nnet)
library(ggplot2)

#Importing dataset
data <- datasets::iris

#One Hot Encoding!
data <- cbind(data[,1:4],class.ind(data$Species))

#Sampling
set.seed(1234)
index <- sample(1:nrow(data), replace = TRUE)
train <- data[index,]
test <- data[-index,]

#Model Fitting
fit <- neuralnet(setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = train, hidden = c(5,5), threshold = 0.01,
                 stepmax = 1e+05, algorithm = "rprop+",
                 err.fct = "sse", act.fct = "logistic")
#Plotting Model
plot(fit,col.entry="blue",col.hidden="green",col.out = "red")

#Cross Validation using set.seed() function
sol <- round(compute(fit, test[,1:(ncol(test)-3)])$net.result)
t1 <- sol
t1[,1] <- abs(t1[,1]-test$setosa)
t1[,2] <- abs(t1[,2]-test$versicolor)
t1[,3] <- abs(t1[,3]-test$virginica)

#Accuracy
err <- sum(t1==1)
acc <- ((nrow(t1)-err)/(nrow(t1)))*100
acc
