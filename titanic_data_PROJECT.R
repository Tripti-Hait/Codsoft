# Load necessary libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(randomForest)

# Load the Titanic dataset
titanic_data=read.csv("C:/Users/tript/Downloads/archive (7)/Titanic-Dataset.csv")
titanic_data
str(titanic_data)
View(titanic_data)

# Display the first few rows of the dataset
head(titanic_data)

# Check for missing values
colSums(is.na(titanic_data))

# Fill missing values
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age, na.rm = TRUE)
titanic_data$Embarked[is.na(titanic_data$Embarked)] <- 'S'
titanic_data$Fare[is.na(titanic_data$Fare)] <- median(titanic_data$Fare, na.rm = TRUE)

# Convert categorical variables to factors
titanic_data$Pclass <- as.factor(titanic_data$Pclass)
titanic_data$Sex <- as.factor(titanic_data$Sex)
titanic_data$Embarked <- as.factor(titanic_data$Embarked)

# Visualize survival rate by gender
ggplot(titanic_data, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = 'dodge') +
  labs(title = 'Survival Rate by Gender', x = 'Gender', y = 'Count')

# Visualize survival rate by class
ggplot(titanic_data, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(position = 'dodge') +
  labs(title = 'Survival Rate by Class', x = 'Class', y = 'Count')



# Convert Survived to factor
titanic_data$Survived <- as.factor(titanic_data$Survived)
titanic_data$Survived


# Fill missing values in 'Embarked' with the most frequent value (mode)
#titanic_data$Embarked[is.na(titanic_data$Embarked)] <- get_mode(titanic_data$Embarked)

# Convert categorical variables to factors
titanic_data$Sex <- as.factor(titanic_data$Sex)
titanic_data$Embarked <- as.factor(titanic_data$Embarked)
titanic_data$Survived <- as.factor(titanic_data$Survived)
titanic_data$Pclass <- as.factor(titanic_data$Pclass)

# Select relevant features for the model
features <- c('Pclass', 'Age', 'SibSp', 'Parch', 'Fare', 'Sex', 'Embarked')
target <- 'Survived'

# Create the feature matrix (X) and target vector (y)
X <- titanic_data[, features]
y <- titanic_data[, target]

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]
# If the new levels in the test set are not critical, you can recode them to an existing level or a new “Other” category.
X_test$Embarked <- factor(X_test$Embarked, levels = levels(X_train$Embarked))



# Train a logistic regression model
model <- train(X_train, y_train, method = 'glm', family = 'binomial')

# Make predictions
predictions <- predict(model,X_test)

# Evaluate the model
confusionMatrix(predictions, y_test)
