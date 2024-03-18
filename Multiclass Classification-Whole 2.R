#---------------------------------------------MultiClass Classification----------------------------------------------------------------------
library(randomForest)
library(pROC)
#=============
#Random Forest
#=============
model_name <- 'RANDOM FOREST(Multiclass)'
set.seed(123)
train_indices <- sample(1:nrow(final_table_multiclass), 0.7 * nrow(final_table_multiclass))
train_data <- final_table_multiclass[train_indices, ]
test_data <- final_table_multiclass[-train_indices, ]

# Fit Random Forest model

random_forest_model <- randomForest(rating_category ~ price + rating_count_tot + rating_count_ver +
                                      user_rating_ver + cont_rating + prime_genre + sup_devices.num +
                                      ipadSc_urls.num + lang.num + vpp_lic + size_mb + paid_status +
                                      older_ver_rating, data = train_data)

# Make predictions on the test set
predictions <- predict(random_forest_model, newdata = test_data)

# Evaluate the model
confusion_matrix <- table(test_data$rating_category, predictions)


# Calculate metrics
accuracy_rf <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision_rf <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall_rf <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)

print(paste("Accuracy:", accuracy_rf))
print(paste("Precision:", precision_rf))
print(paste("Recall:", recall_rf))
print(paste("F1-Score:", f1_score_rf))
print("Confusion Matrix:")
print(confusion_matrix)

# Plot ROC curve

roc_curve <- roc(test_data$rating_category, as.numeric(predictions))
auc_roc_rf <- auc(roc_curve)
print(paste("AUC-ROC:", auc_roc_rf))
plot(roc_curve, main = "RF-Multiclass-ROC Curve", col = "blue", lwd = 2)

append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =accuracy_rf,
    Precision = precision_rf,
    Recall = recall_rf,
    F1_Score = f1_score_rf,
    AUC_ROC = auc_roc_rf
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

# Example usage:
# Replace 'model_name', 'precision', 'recall', 'f1_score', 'auc_roc' with actual values
results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Print the updated dataframe
print(results_df)
#===============================================================================================================================================
#=============
#SVM
#=============
model_name <- 'SVM-KFold(Multiclass)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(final_data_multiclass$rating_category, k = num_folds, list = TRUE, returnTrain = FALSE)

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
  test_data <- final_data_multiclass[test_indices, ]
  
  # The remaining data is the training set
  train_data <- final_data_multiclass[-test_indices, ]
  
  # Convert labels to factors (if not already)
  train_data$rating_category <- as.factor(train_data$rating_category)
  test_data$rating_category <- as.factor(test_data$rating_category)
  
  # Fit SVM model
  svm_model <- svm(rating_category ~ ., data = train_data, kernel = "radial")
  
  # Make predictions on the test set
  predictions <- predict(svm_model, newdata = test_data)
  
  # Evaluate the model
  confusion_matrix <- table(test_data$rating_category, predictions)
  
  # Calculate metrics for the current fold
  accuracy_svm[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_svm[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_svm[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_svm[fold] <- 2 * (precision_svm[fold] * recall_svm[fold]) / (precision_svm[fold] + recall_svm[fold])
  
  # Calculate AUC-ROC
  roc_curve <- roc(as.numeric(test_data$rating_category) - 1, as.numeric(predictions) - 1)
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

plot(roc_curve, main = "SVM-Multiclass-ROC Curve", col = "blue", lwd = 2)


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
#===============================================================================================================================================
#=============
#Naive Bayes
#=============
model_name <- 'NaiveBayes-KFold(Multiclass)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(final_data_multiclass$rating_category, k = num_folds, list = TRUE, returnTrain = FALSE)

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
  test_data <- final_data_multiclass[test_indices, ]
  
  # The remaining data is the training set
  train_data <- final_data_multiclass[-test_indices, ]
  
  # Convert labels to factors (if not already)
  train_data$rating_category <- as.factor(train_data$rating_category)
  test_data$rating_category <- as.factor(test_data$rating_category)
  
  # Fit Naive Bayes model
  nb_model <- naiveBayes(rating_category ~ ., data = train_data)
  
  # Make predictions on the test set
  predictions <- predict(nb_model, newdata = test_data)
  
  # Evaluate the model
  confusion_matrix <- table(test_data$rating_category, predictions)
  
  # Calculate metrics for the current fold
  accuracy_nb[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_nb[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_nb[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_nb[fold] <- 2 * (precision_nb[fold] * recall_nb[fold]) / (precision_nb[fold] + recall_nb[fold])
  
  # Calculate AUC-ROC
  roc_curve <- roc(as.numeric(test_data$rating_category) - 1, as.numeric(predictions) - 1)
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

plot(roc_curve, main = "NB-Multiclass-ROC Curve", col = "blue", lwd = 2)


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
#===============================================================================================================================================
#=============
#Decision Tree
#=============
model_name <- 'DECISION TREE-KFold(Multiclass)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(final_table_multiclass$rating_category, k = num_folds, list = TRUE, returnTrain = FALSE)

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
  test_data <- final_table_multiclass[test_indices, ]
  
  # The remaining data is the training set
  train_data <- final_table_multiclass[-test_indices, ]
  
  # Fit Decision Tree model
  decision_tree_model <- rpart(rating_category ~ price + rating_count_tot + rating_count_ver +
                                 user_rating_ver + cont_rating + prime_genre + sup_devices.num +
                                 ipadSc_urls.num + lang.num + vpp_lic + size_mb + paid_status +
                                 older_ver_rating, data = train_data, method = "class")
  
  # Make predictions on the test set
  predictions <- predict(decision_tree_model, newdata = test_data, type = "class")
  
  # Evaluate the model
  confusion_matrix <- table(test_data$rating_category, predictions)
  
  # Calculate metrics for the current fold
  accuracy_dt[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_dt[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_dt[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_dt[fold] <- 2 * (precision_dt[fold] * recall_dt[fold]) / (precision_dt[fold] + recall_dt[fold])
  
  # Calculate AUC-ROC
  roc_curve <- roc(test_data$rating_category, as.numeric(predictions))
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
plot(roc_curve, main = "DT-MultiClass-ROC Curve", col = "blue", lwd = 2)


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
#===============================================================================================================================================
