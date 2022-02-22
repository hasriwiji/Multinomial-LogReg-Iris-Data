#rload dataset
data<-iris
head(data)
tail(data)

# summarize the class distribution
percentage <- proportions(table(data$Species)) * 100 # Alternatif : prop.table
cbind(freq=table(data$Species), percentage=percentage)

#check levels of Species
levels(data$Species)

#check missing value
sum(is.na(data))

# summarize attribute distributions
summary(data)

# Create Data Partition
library(caret)
set.seed(123)
index <- createDataPartition(data$Species, p=0.80, list=FALSE)

# select 80% of the data for Training
training <- data[index,] # Data Frame [baris, kolom]
dim(training)

# use the remaining 20% of data to testing the models
testing <- data[-index,]

##### Multinomial Logistic Regression Analysis
library(nnet) 
training$Species <- relevel(training$Species, ref = "setosa") 

model <- multinom(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data = data)
summary(model)

# Predicting the values for train dataset
training$ClassPredicted <- predict(model, newdata = training, "class")

# Building classification table
tab <- table(training$Species, training$ClassPredicted)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2) # round(number, 2)

# Predicting the values for testing dataset
testing$ClassPredicted <- predict(model, newdata = testing, "class")

# Building classification table
tab2 <- table(testing$Species, testing$ClassPredicted)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab2))/sum(tab2))*100,2)
dim(testing)
