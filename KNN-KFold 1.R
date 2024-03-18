#---------------------------------------------KNN(Oversampled and Undersampled)----------------------------------------------------------------------
library(caret)
library(pROC)
library(class)

# Assuming 'oversampled_data_scaled' is your oversampled and scaled dataset
model_name <- 'KNN-KFold(oversampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(oversampled_data_scaled$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initialize variables to store evaluation metrics
accuracy_knn <- numeric(num_folds)
precision_knn <- numeric(num_folds)
recall_knn <- numeric(num_folds)
f1_score_knn <- numeric(num_folds)
auc_roc_knn <- numeric(num_folds)

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
  
  # Fit KNN model
  knn_model <- knn(train = as.matrix(train_data[, -ncol(train_data)]),
                   test = as.matrix(test_data[, -ncol(test_data)]),
                   cl = train_data$successful_app, k = 5)  # Set k as needed
  
  # Evaluate the model
  confusion_matrix <- table(test_data$successful_app, knn_model)
  
  # Calculate metrics for the current fold
  accuracy_knn[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_knn[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_knn[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_knn[fold] <- 2 * (precision_knn[fold] * recall_knn[fold]) / (precision_knn[fold] + recall_knn[fold])
  
  # Calculate AUC-ROC
  roc_curve <- roc(as.numeric(test_data$successful_app) - 1, as.numeric(knn_model) - 1)
  auc_roc_knn[fold] <- auc(roc_curve)
}

# Calculate average metrics over all folds
avg_accuracy_knn <- mean(accuracy_knn)
avg_precision_knn <- mean(precision_knn)
avg_recall_knn <- mean(recall_knn)
avg_f1_score_knn <- mean(f1_score_knn)
avg_auc_roc_knn <- mean(auc_roc_knn)

# Print or store the average metrics
print(paste("Average Accuracy:", avg_accuracy_knn))
print(paste("Average Precision:", avg_precision_knn))
print(paste("Average Recall:", avg_recall_knn))
print(paste("Average F1-Score:", avg_f1_score_knn))
print(paste("Average AUC-ROC:", avg_auc_roc_knn))

plot(roc_curve, main = "KNN-OverSampled Data-ROC Curve", col = "blue", lwd = 2)

append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_knn,
    Precision = avg_precision_knn,
    Recall = avg_recall_knn,
    F1_Score = avg_f1_score_knn,
    AUC_ROC = avg_auc_roc_knn
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

# Example usage:
# Replace 'model_name', 'precision', 'recall', 'f1_score', 'auc_roc' with actual values
results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)
#=======================================================================================================================================================================
library(caret)
library(pROC)
library(class)

# Assuming 'oversampled_data_scaled' is your oversampled and scaled dataset
model_name <- 'KNN-KFold(undersampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(undersampled_data_scaled$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initialize variables to store evaluation metrics
accuracy_knn <- numeric(num_folds)
precision_knn <- numeric(num_folds)
recall_knn <- numeric(num_folds)
f1_score_knn <- numeric(num_folds)
auc_roc_knn <- numeric(num_folds)

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
  
  # Fit KNN model
  knn_model <- knn(train = as.matrix(train_data[, -ncol(train_data)]),
                   test = as.matrix(test_data[, -ncol(test_data)]),
                   cl = train_data$successful_app, k = 5)  # Set k as needed
  
  # Evaluate the model
  confusion_matrix <- table(test_data$successful_app, knn_model)
  
  # Calculate metrics for the current fold
  accuracy_knn[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_knn[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_knn[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_knn[fold] <- 2 * (precision_knn[fold] * recall_knn[fold]) / (precision_knn[fold] + recall_knn[fold])
  
  # Calculate AUC-ROC
  roc_curve <- roc(as.numeric(test_data$successful_app) - 1, as.numeric(knn_model) - 1)
  auc_roc_knn[fold] <- auc(roc_curve)
}

# Calculate average metrics over all folds
avg_accuracy_knn <- mean(accuracy_knn)
avg_precision_knn <- mean(precision_knn)
avg_recall_knn <- mean(recall_knn)
avg_f1_score_knn <- mean(f1_score_knn)
avg_auc_roc_knn <- mean(auc_roc_knn)

# Print or store the average metrics
print(paste("Average Accuracy:", avg_accuracy_knn))
print(paste("Average Precision:", avg_precision_knn))
print(paste("Average Recall:", avg_recall_knn))
print(paste("Average F1-Score:", avg_f1_score_knn))
print(paste("Average AUC-ROC:", avg_auc_roc_knn))

plot(roc_curve, main = "KNN-UnderSampled Data-ROC Curve", col = "blue", lwd = 2)

append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_knn,
    Precision = avg_precision_knn,
    Recall = avg_recall_knn,
    F1_Score = avg_f1_score_knn,
    AUC_ROC = avg_auc_roc_knn
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

# Example usage:
# Replace 'model_name', 'precision', 'recall', 'f1_score', 'auc_roc' with actual values
results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

print(results_df)

