#---------------------------------------------Decision Tree(Oversampled and Undersampled)----------------------------------------------------------------------
library(caret)
library(pROC)
library(rpart)

# Assuming 'final_table' is your original dataset
model_name <- 'DECISION TREE-KFold(oversampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(oversampled_data$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initialize variables to store evaluation metrics
accuracy_dt <- numeric(num_folds)
precision_dt <- numeric(num_folds)
recall_dt <- numeric(num_folds)
f1_score_dt <- numeric(num_folds)
auc_roc_dt <- numeric(num_folds)

# Iterate through folds
for (fold in seq_along(folds)) {
  # Extract the test set for the current fold
  test_indices <- unlist(folds[fold])
  test_data <- oversampled_data[test_indices, ]
  
  # The remaining data is the training set
  train_data <- oversampled_data[-test_indices, ]
  
  # Fit Decision Tree model
  decision_tree_model <- rpart(successful_app ~ price + rating_count_tot + rating_count_ver +
                                 user_rating_ver + cont_rating + prime_genre + sup_devices.num +
                                 ipadSc_urls.num + lang.num + vpp_lic + size_mb + paid_status +
                                 older_ver_rating, data = train_data, method = "class")
  
  # Make predictions on the test set
  predictions <- predict(decision_tree_model, newdata = test_data, type = "class")
  
  # Evaluate the model
  confusion_matrix <- table(test_data$successful_app, predictions)
  
  # Calculate metrics for the current fold
  accuracy_dt[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_dt[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_dt[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_dt[fold] <- 2 * (precision_dt[fold] * recall_dt[fold]) / (precision_dt[fold] + recall_dt[fold])
  
  # Calculate AUC-ROC
  roc_curve <- roc(test_data$successful_app, as.numeric(predictions))
  auc_roc_dt[fold] <- auc(roc_curve)
}

# Calculate average metrics over all folds
avg_accuracy_dt <- mean(accuracy_dt)
avg_precision_dt <- mean(precision_dt)
avg_recall_dt <- mean(recall_dt)
avg_f1_score_dt <- mean(f1_score_dt)
avg_auc_roc_dt <- mean(auc_roc_dt)

# Print or store the average metrics
print(paste("Average Accuracy:", avg_accuracy_dt))
print(paste("Average Precision:", avg_precision_dt))
print(paste("Average Recall:", avg_recall_dt))
print(paste("Average F1-Score:", avg_f1_score_dt))
print(paste("Average AUC-ROC:", avg_auc_roc_dt))

# Plot ROC curve for the last fold (you can customize this part)
plot(roc_curve, main = "DT-OverSampled Data-ROC Curve", col = "blue", lwd = 2)


append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_dt,
    Precision = avg_precision_dt,
    Recall = avg_recall_dt,
    F1_Score = avg_f1_score_dt,
    AUC_ROC = avg_auc_roc_dt
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

model_name <- 'DECISION TREE-KFold(undersampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(undersampled_data$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initialize variables to store evaluation metrics
accuracy_dt <- numeric(num_folds)
precision_dt <- numeric(num_folds)
recall_dt <- numeric(num_folds)
f1_score_dt <- numeric(num_folds)
auc_roc_dt <- numeric(num_folds)

# Iterate through folds
for (fold in seq_along(folds)) {
  # Extract the test set for the current fold
  test_indices <- unlist(folds[fold])
  test_data <- undersampled_data[test_indices, ]
  
  # The remaining data is the training set
  train_data <- undersampled_data[-test_indices, ]
  
  # Fit Decision Tree model
  decision_tree_model <- rpart(successful_app ~ price + rating_count_tot + rating_count_ver +
                                 user_rating_ver + cont_rating + prime_genre + sup_devices.num +
                                 ipadSc_urls.num + lang.num + vpp_lic + size_mb + paid_status +
                                 older_ver_rating, data = train_data, method = "class")
  
  # Make predictions on the test set
  predictions <- predict(decision_tree_model, newdata = test_data, type = "class")
  
  # Evaluate the model
  confusion_matrix <- table(test_data$successful_app, predictions)
  
  # Calculate metrics for the current fold
  accuracy_dt[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_dt[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_dt[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_dt[fold] <- 2 * (precision_dt[fold] * recall_dt[fold]) / (precision_dt[fold] + recall_dt[fold])
  
  # Calculate AUC-ROC
  roc_curve <- roc(test_data$successful_app, as.numeric(predictions))
  auc_roc_dt[fold] <- auc(roc_curve)
}

# Calculate average metrics over all folds
avg_accuracy_dt <- mean(accuracy_dt)
avg_precision_dt <- mean(precision_dt)
avg_recall_dt <- mean(recall_dt)
avg_f1_score_dt <- mean(f1_score_dt)
avg_auc_roc_dt <- mean(auc_roc_dt)

# Print or store the average metrics
print(paste("Average Accuracy:", avg_accuracy_dt))
print(paste("Average Precision:", avg_precision_dt))
print(paste("Average Recall:", avg_recall_dt))
print(paste("Average F1-Score:", avg_f1_score_dt))
print(paste("Average AUC-ROC:", avg_auc_roc_dt))

# Plot ROC curve for the last fold (you can customize this part)
plot(roc_curve, main = "DT-UnderSampled Data-ROC Curve", col = "blue", lwd = 2)


append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_dt,
    Precision = avg_precision_dt,
    Recall = avg_recall_dt,
    F1_Score = avg_f1_score_dt,
    AUC_ROC = avg_auc_roc_dt
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

# Example usage:
# Replace 'model_name', 'precision', 'recall', 'f1_score', 'auc_roc' with actual values
results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Print the updated dataframe
print(results_df)