#---------------------------------------------Naive Bayes(Oversampled and Undersampled)----------------------------------------------------------------------
library(caret)
library(pROC)
library(e1071)

# Assuming 'final_data' is your original dataset
model_name <- 'NaiveBayes-KFold(undersampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(undersampled_data$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initialize variables to store evaluation metrics
accuracy_nb <- numeric(num_folds)
precision_nb <- numeric(num_folds)
recall_nb <- numeric(num_folds)
f1_score_nb <- numeric(num_folds)
auc_roc_nb <- numeric(num_folds)

# Iterate through folds
for (fold in seq_along(folds)) {
  # Extract the test set for the current fold
  test_indices <- unlist(folds[fold])
  test_data <- undersampled_data[test_indices, ]
  
  # The remaining data is the training set
  train_data <- undersampled_data[-test_indices, ]
  
  # Convert labels to factors (if not already)
  train_data$successful_app <- as.factor(train_data$successful_app)
  test_data$successful_app <- as.factor(test_data$successful_app)
  
  # Fit Naive Bayes model
  nb_model <- naiveBayes(successful_app ~ ., data = train_data)
  
  # Make predictions on the test set
  predictions <- predict(nb_model, newdata = test_data)
  
  # Evaluate the model
  confusion_matrix <- table(test_data$successful_app, predictions)
  
  # Calculate metrics for the current fold
  accuracy_nb[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_nb[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_nb[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_nb[fold] <- 2 * (precision_nb[fold] * recall_nb[fold]) / (precision_nb[fold] + recall_nb[fold])
  
  # Calculate AUC-ROC
  roc_curve <- roc(as.numeric(test_data$successful_app) - 1, as.numeric(predictions) - 1)
  auc_roc_nb[fold] <- auc(roc_curve)
}

# Calculate average metrics over all folds
avg_accuracy_nb <- mean(accuracy_nb)
avg_precision_nb <- mean(precision_nb)
avg_recall_nb <- mean(recall_nb)
avg_f1_score_nb <- mean(f1_score_nb)
avg_auc_roc_nb <- mean(auc_roc_nb)

# Print or store the average metrics
print(paste("Average Accuracy:", avg_accuracy_nb))
print(paste("Average Precision:", avg_precision_nb))
print(paste("Average Recall:", avg_recall_nb))
print(paste("Average F1-Score:", avg_f1_score_nb))
print(paste("Average AUC-ROC:", avg_auc_roc_nb))

#Plot the ROC curve
plot(roc_curve, main = "NaiveBayes-UnderSampled Data-ROC Curve", col = "blue", lwd = 2)


# Function to append results to a dataframe
append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy = avg_accuracy_nb,
    Precision = avg_precision_nb,
    Recall = avg_recall_nb,
    F1_Score = avg_f1_score_nb,
    AUC_ROC = avg_auc_roc_nb
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

# Example usage:
# Replace 'model_name', 'precision', 'recall', 'f1_score', 'auc_roc' with actual values
results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Print the updated dataframe
print(results_df)
#=======================================================================================================================================================================

library(caret)
library(pROC)
library(e1071)

# Assuming 'final_data' is your original dataset
model_name <- 'NaiveBayes-KFold(oversampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(oversampled_data$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initialize variables to store evaluation metrics
accuracy_nb <- numeric(num_folds)
precision_nb <- numeric(num_folds)
recall_nb <- numeric(num_folds)
f1_score_nb <- numeric(num_folds)
auc_roc_nb <- numeric(num_folds)

# Iterate through folds
for (fold in seq_along(folds)) {
  # Extract the test set for the current fold
  test_indices <- unlist(folds[fold])
  test_data <- oversampled_data[test_indices, ]
  
  # The remaining data is the training set
  train_data <- oversampled_data[-test_indices, ]
  
  # Convert labels to factors (if not already)
  train_data$successful_app <- as.factor(train_data$successful_app)
  test_data$successful_app <- as.factor(test_data$successful_app)
  
  # Fit Naive Bayes model
  nb_model <- naiveBayes(successful_app ~ ., data = train_data)
  
  # Make predictions on the test set
  predictions <- predict(nb_model, newdata = test_data)
  
  # Evaluate the model
  confusion_matrix <- table(test_data$successful_app, predictions)
  
  # Calculate metrics for the current fold
  accuracy_nb[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_nb[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_nb[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_nb[fold] <- 2 * (precision_nb[fold] * recall_nb[fold]) / (precision_nb[fold] + recall_nb[fold])
  
  # Calculate AUC-ROC
  roc_curve <- roc(as.numeric(test_data$successful_app) - 1, as.numeric(predictions) - 1)
  auc_roc_nb[fold] <- auc(roc_curve)
}

# Calculate average metrics over all folds
avg_accuracy_nb <- mean(accuracy_nb)
avg_precision_nb <- mean(precision_nb)
avg_recall_nb <- mean(recall_nb)
avg_f1_score_nb <- mean(f1_score_nb)
avg_auc_roc_nb <- mean(auc_roc_nb)

# Print or store the average metrics
print(paste("Average Accuracy:", avg_accuracy_nb))
print(paste("Average Precision:", avg_precision_nb))
print(paste("Average Recall:", avg_recall_nb))
print(paste("Average F1-Score:", avg_f1_score_nb))
print(paste("Average AUC-ROC:", avg_auc_roc_nb))

#Plot the ROC curve
plot(roc_curve, main = "NaiveBayes-OverSampled Data-ROC Curve", col = "blue", lwd = 2)


# Function to append results to a dataframe
append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy = avg_accuracy_nb,
    Precision = avg_precision_nb,
    Recall = avg_recall_nb,
    F1_Score = avg_f1_score_nb,
    AUC_ROC = avg_auc_roc_nb
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

# Example usage:
# Replace 'model_name', 'precision', 'recall', 'f1_score', 'auc_roc' with actual values
results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Print the updated dataframe
print(results_df)

