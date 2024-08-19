# Load required libraries
library(caret)
library(randomForest)

# Load the Iris dataset
data(iris)
plot(iris,col="blue")
# Explore the dataset
head(iris)
summary(iris)

# Check for missing values
sum(is.na(iris))

# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = .7, 
                                  list = FALSE, 
                                  times = 1)
irisTrain <- iris[trainIndex,]
irisTest <- iris[-trainIndex,]

# Train the Random Forest model
set.seed(123)
model <- randomForest(Species ~ ., data = irisTrain)

# Print the model
print(model)

# Predict on the test set
predictions <- predict(model, irisTest)

# Confusion matrix
confusionMatrix(predictions, irisTest$Species)
