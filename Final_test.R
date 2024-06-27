finalproject <- read.csv("D:/Data Science Project/Dataset_Project 2.csv", header=TRUE, sep=",")
options(max.print = 5000)
finalproject
summary(finalproject)
colSums(is.na(finalproject))

# Clean the dataset by removing rows with NA values
finalproject <- finalproject[complete.cases(finalproject), ]

# Chi-square test for Ingredient1
contingency_table <- table(finalproject$Ingredient1, finalproject$ClassificationOutput)

if (any(contingency_table == 0)) {
  print("There are zero entries in the contingency table. Check for missing or invalid data.")
} else {
  chi_result <- chisq.test(contingency_table)
  print(chi_result)
}

# Perform Chi-square test for Ingredient1, Ingredient2, and Ingredient3
tableIng1 <- table(finalproject$Ingredient1, finalproject$ClassificationOutput)
chi_1 <- chisq.test(tableIng1)
pVal_1 <- chi_1$p.value

if (pVal_1 < 0.05) {
  print("Ingredient 1 is Significant")
} else {
  print("Ingredient 1 is Not Significant")
}

tableIng2 <- table(finalproject$Ingredient2, finalproject$ClassificationOutput)
chi_2 <- chisq.test(tableIng2)
pVal_2 <- chi_2$p.value

if (pVal_2 < 0.05) {
  print("Ingredient 2 is Significant")
} else {
  print("Ingredient 2 is Not Significant")
}

tableIng3 <- table(finalproject$Ingredient3, finalproject$ClassificationOutput)
chi_3 <- chisq.test(tableIng3)
pVal_3 <- chi_3$p.value

if (pVal_3 < 0.05) {
  print("Ingredient 3 is Significant")
} else {
  print("Ingredient 3 is Not Significant")
}

# Naive Bayes classification
install.packages("e1071")
library(e1071)
nb <- naiveBayes(ClassificationOutput ~ Ingredient1 + Ingredient2 + Ingredient3, data = finalproject)

multi_unknown_instance <- data.frame(
  Ingredient1 = c("Mushroom", "Pumpkin", "Mango"),
  Ingredient2 = c("Butter", "Apples", "Honey"),
  Ingredient3 = c("Sage", "Curry", "Lime")
)
predicted_class <- predict(nb, newdata = multi_unknown_instance)
print("Predicted Class:")
print(predicted_class)

single_unknown_instance <- data.frame(
  Ingredient1 = "Mushroom",
  Ingredient2 = "Butter",
  Ingredient3 = "Sage"
)
predicted_class <- predict(nb, newdata = single_unknown_instance)
print("Predicted Class:")
print(predicted_class)

# Data splitting and training the model
install.packages("caret") 
library(caret)
train_index <- sample(1:nrow(finalproject), size = 0.8 * nrow(finalproject)) 
train_data <- finalproject[train_index, ] 
test_data <- finalproject[-train_index, ]
train_data
test_data

num_of_test_data <- nrow(test_data)
print("No of Test Data set: ")
num_of_test_data

num_of_train_data <- nrow(train_data)
print("No of Tranning Data set: ")
num_of_train_data

# Train the model
ML_nb_model <- naive_bayes(ClassificationOutput ~ ., data = train_data) 
ML_nb_model

# Make predictions on the test data
predicted_class <- predict(ML_nb_model, newdata = test_data)
correctly_classified <- sum(predicted_class == test_data$ClassificationOutput)
cat("Number of correctly classified instances :", correctly_classified, "\n")

accuracy <- correctly_classified / nrow(test_data)
cat("accuracy is:")
accuracy

# Cross-validation
CrossValidation <- trainControl(method = "cv", number = 10) 
CrossValidation
cv_model <- train(ClassificationOutput ~ ., data = finalproject, method = "naive_bayes", trControl = CrossValidation)
cv_model

all_fold <- cv_model$resample
all_fold

accuracy <- cv_model$results$Accuracy
accuracy

# Confusion matrix
cm <- confusionMatrix(cv_model)
cm

