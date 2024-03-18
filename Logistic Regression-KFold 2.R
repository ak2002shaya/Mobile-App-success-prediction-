#---------------------------------------------Logistic Regression(Oversampled and Undersampled)----------------------------------------------------------------------
library(caret)
library(pROC)

model_name <- 'LOGISTIC REGRESSION-KFold(oversampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(oversampled_data$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initialize variables to store evaluation metrics
accuracy_lr <- numeric(num_folds)
precision_lr <- numeric(num_folds)
recall_lr <- numeric(num_folds)
f1_score_lr <- numeric(num_folds)
auc_roc_lr <- numeric(num_folds)

# Iterate through folds
for (fold in seq_along(folds)) {
  # Extract the test set for the current fold
  test_indices <- unlist(folds[fold])
  test_data <- oversampled_data[test_indices, ]
  
  # The remaining data is the training set
  train_data <- oversampled_data[-test_indices, ]
  
  # Fit logistic regression model
  logistic_model <- glm(successful_app ~ price+rating_count_tot+rating_count_ver+user_rating_ver+
                          cont_rating+prime_genre+sup_devices.num+ipadSc_urls.num+lang.num+vpp_lic
                        +size_mb+paid_status+older_ver_rating, data = train_data, family = binomial)
  
  # Make predictions on the test set
  predictions <- predict(logistic_model, newdata = test_data, type = "response")
  
  # Convert predicted probabilities to binary predictions (0 or 1)
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  
  # Evaluate the model
  confusion_matrix <- table(test_data$successful_app, predicted_classes)
  
  # Calculate metrics for the current fold
  accuracy_lr[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_lr[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_lr[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_lr[fold] <- 2 * (precision_lr[fold] * recall_lr[fold]) / (precision_lr[fold] + recall_lr[fold])
  
  # Calculate AUC-ROC
  roc_curve <- roc(test_data$successful_app, predictions)
  auc_roc_lr[fold] <- auc(roc_curve)
}

# Calculate average metrics over all folds
avg_accuracy_lr <- mean(accuracy_lr)
avg_precision_lr <- mean(precision_lr)
avg_recall_lr <- mean(recall_lr)
avg_f1_score_lr <- mean(f1_score_lr)
avg_auc_roc_lr <- mean(auc_roc_lr)

# Print or store the average metrics
print(paste("Average Accuracy:", avg_accuracy_lr))
print(paste("Average Precision:", avg_precision_lr))
print(paste("Average Recall:", avg_recall_lr))
print(paste("Average F1-Score:", avg_f1_score_lr))
print(paste("Average AUC-ROC:", avg_auc_roc_lr))

# Plot ROC curve
plot(roc_curve, main = "LR-OverSampled Data-ROC Curve", col = "blue", lwd = 2)


append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_lr,
    Precision = avg_precision_lr,
    Recall = avg_recall_lr,
    F1_Score = avg_f1_score_lr,
    AUC_ROC = avg_auc_roc_lr
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Print the updated dataframe
print(results_df)

#=======================================================================================================================================================================

model_name <- 'LOGISTIC REGRESSION-KFold(undersampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(undersampled_data$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initialize variables to store evaluation metrics
accuracy_lr <- numeric(num_folds)
precision_lr <- numeric(num_folds)
recall_lr <- numeric(num_folds)
f1_score_lr <- numeric(num_folds)
auc_roc_lr <- numeric(num_folds)

# Iterate through folds
for (fold in seq_along(folds)) {
  # Extract the test set for the current fold
  test_indices <- unlist(folds[fold])
  test_data <- undersampled_data[test_indices, ]
  
  # The remaining data is the training set
  train_data <- undersampled_data[-test_indices, ]
  
  # Fit logistic regression model
  logistic_model <- glm(successful_app ~ price+rating_count_tot+rating_count_ver+user_rating_ver+
                          cont_rating+prime_genre+sup_devices.num+ipadSc_urls.num+lang.num+vpp_lic
                        +size_mb+paid_status+older_ver_rating, data = train_data, family = binomial)
  
  # Make predictions on the test set
  predictions <- predict(logistic_model, newdata = test_data, type = "response")
  
  # Convert predicted probabilities to binary predictions (0 or 1)
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  
  # Evaluate the model
  confusion_matrix <- table(test_data$successful_app, predicted_classes)
  
  # Calculate metrics for the current fold
  accuracy_lr[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_lr[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_lr[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_lr[fold] <- 2 * (precision_lr[fold] * recall_lr[fold]) / (precision_lr[fold] + recall_lr[fold])
  
  # Calculate AUC-ROC
  roc_curve <- roc(test_data$successful_app, predictions)
  auc_roc_lr[fold] <- auc(roc_curve)
}

# Calculate average metrics over all folds
avg_accuracy_lr <- mean(accuracy_lr)
avg_precision_lr <- mean(precision_lr)
avg_recall_lr <- mean(recall_lr)
avg_f1_score_lr <- mean(f1_score_lr)
avg_auc_roc_lr <- mean(auc_roc_lr)

# Print or store the average metrics
print(paste("Average Accuracy:", avg_accuracy_lr))
print(paste("Average Precision:", avg_precision_lr))
print(paste("Average Recall:", avg_recall_lr))
print(paste("Average F1-Score:", avg_f1_score_lr))
print(paste("Average AUC-ROC:", avg_auc_roc_lr))

# Plot ROC curve
plot(roc_curve, main = "LR-UnderSampled Data-ROC Curve", col = "blue", lwd = 2)


append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_lr,
    Precision = avg_precision_lr,
    Recall = avg_recall_lr,
    F1_Score = avg_f1_score_lr,
    AUC_ROC = avg_auc_roc_lr
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

# Example usage:
# Replace 'model_name', 'precision', 'recall', 'f1_score', 'auc_roc' with actual values
results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Print the updated dataframe
print(results_df)