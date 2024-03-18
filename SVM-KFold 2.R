#---------------------------------------------SVM(Oversampled and Undersampled)----------------------------------------------------------------------
library(caret)
library(pROC)
library(e1071)

model_name <- 'SVM-KFold(oversampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(oversampled_data_scaled$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initialize variables to store evaluation metrics
accuracy_svm <- numeric(num_folds)
precision_svm <- numeric(num_folds)
recall_svm <- numeric(num_folds)
f1_score_svm <- numeric(num_folds)
auc_roc_svm <- numeric(num_folds)

# Iterate through folds
for (fold in seq_along(folds)) {
  # Extract the test set for the current fold
  test_indices <- unlist(folds[fold])
  test_data <- oversampled_data_scaled[test_indices, ]
  
  # The remaining data is the training set
  train_data <- oversampled_data_scaled[-test_indices, ]
  
  # Convert labels to factors (if not already)
  train_data$successful_app <- as.factor(train_data$successful_app)
  test_data$successful_app <- as.factor(test_data$successful_app)
  
  # Fit SVM model
  svm_model <- svm(successful_app ~ ., data = train_data, kernel = "radial")
  
  # Make predictions on the test set
  predictions <- predict(svm_model, newdata = test_data)
  
  # Evaluate the model
  confusion_matrix <- table(test_data$successful_app, predictions)
  
  # Calculate metrics for the current fold
  accuracy_svm[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_svm[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_svm[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_svm[fold] <- 2 * (precision_svm[fold] * recall_svm[fold]) / (precision_svm[fold] + recall_svm[fold])
  
  # Calculate AUC-ROC
  roc_curve <- roc(as.numeric(test_data$successful_app) - 1, as.numeric(predictions) - 1)
  auc_roc_svm[fold] <- auc(roc_curve)
}

# Calculate average metrics over all folds
avg_accuracy_svm <- mean(accuracy_svm)
avg_precision_svm <- mean(precision_svm)
avg_recall_svm <- mean(recall_svm)
avg_f1_score_svm <- mean(f1_score_svm)
avg_auc_roc_svm <- mean(auc_roc_svm)

# Print or store the average metrics
print(paste("Average Accuracy:", avg_accuracy_svm))
print(paste("Average Precision:", avg_precision_svm))
print(paste("Average Recall:", avg_recall_svm))
print(paste("Average F1-Score:", avg_f1_score_svm))
print(paste("Average AUC-ROC:", avg_auc_roc_svm))
plot(roc_curve, main = "SVM-OverSampled Data-ROC Curve", col = "blue", lwd = 2)


append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_svm,
    Precision = avg_precision_svm,
    Recall = avg_recall_svm,
    F1_Score = avg_f1_score_svm,
    AUC_ROC = avg_auc_roc_svm
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
model_name <- 'SVM-KFold(undersampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(undersampled_data_scaled$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initialize variables to store evaluation metrics
accuracy_svm <- numeric(num_folds)
precision_svm <- numeric(num_folds)
recall_svm <- numeric(num_folds)
f1_score_svm <- numeric(num_folds)
auc_roc_svm <- numeric(num_folds)

# Iterate through folds
for (fold in seq_along(folds)) {
  # Extract the test set for the current fold
  test_indices <- unlist(folds[fold])
  test_data <- undersampled_data_scaled[test_indices, ]
  
  # The remaining data is the training set
  train_data <- undersampled_data_scaled[-test_indices, ]
  
  # Convert labels to factors (if not already)
  train_data$successful_app <- as.factor(train_data$successful_app)
  test_data$successful_app <- as.factor(test_data$successful_app)
  
  # Fit SVM model
  svm_model <- svm(successful_app ~ ., data = train_data, kernel = "radial")
  
  # Make predictions on the test set
  predictions <- predict(svm_model, newdata = test_data)
  
  # Evaluate the model
  confusion_matrix <- table(test_data$successful_app, predictions)
  
  # Calculate metrics for the current fold
  accuracy_svm[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_svm[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_svm[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_svm[fold] <- 2 * (precision_svm[fold] * recall_svm[fold]) / (precision_svm[fold] + recall_svm[fold])
  
  # Calculate AUC-ROC
  roc_curve <- roc(as.numeric(test_data$successful_app) - 1, as.numeric(predictions) - 1)
  auc_roc_svm[fold] <- auc(roc_curve)
}

# Calculate average metrics over all folds
avg_accuracy_svm <- mean(accuracy_svm)
avg_precision_svm <- mean(precision_svm)
avg_recall_svm <- mean(recall_svm)
avg_f1_score_svm <- mean(f1_score_svm)
avg_auc_roc_svm <- mean(auc_roc_svm)

# Print or store the average metrics
print(paste("Average Accuracy:", avg_accuracy_svm))
print(paste("Average Precision:", avg_precision_svm))
print(paste("Average Recall:", avg_recall_svm))
print(paste("Average F1-Score:", avg_f1_score_svm))
print(paste("Average AUC-ROC:", avg_auc_roc_svm))

plot(roc_curve, main = "SVM-UnderSampled Data-ROC Curve", col = "blue", lwd = 2)


append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_svm,
    Precision = avg_precision_svm,
    Recall = avg_recall_svm,
    F1_Score = avg_f1_score_svm,
    AUC_ROC = avg_auc_roc_svm
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

# Example usage:
# Replace 'model_name', 'precision', 'recall', 'f1_score', 'auc_roc' with actual values
results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Print the updated dataframe
print(results_df)

