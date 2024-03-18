#---------------------------------------------Random Forest(Oversampled and Undersampled-Important Features)----------------------------------------------------------------------
library(randomForest)
library(pROC)

model_name <- 'RANDOM FOREST(oversampled-Important Features)'
set.seed(123)
train_indices <- sample(1:nrow(oversampled_data), 0.7 * nrow(oversampled_data))
train_data <- oversampled_data[train_indices, ]
test_data <- oversampled_data[-train_indices, ]

# Fit Random Forest model

random_forest_model <- randomForest(successful_app ~ rating_count_tot + rating_count_ver +
                                      user_rating_ver + older_ver_rating, data = train_data)

# Make predictions on the test set
predictions <- predict(random_forest_model, newdata = test_data)

# Evaluate the model
confusion_matrix <- table(test_data$successful_app, predictions)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate metrics
accuracy_rf <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision_rf <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall_rf <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)

print(paste("Accuracy:", accuracy_rf))
print(paste("Precision:", precision_rf))
print(paste("Recall:", recall_rf))
print(paste("F1-Score:", f1_score_rf))

# Plot ROC curve

roc_curve <- roc(test_data$successful_app, as.numeric(predictions))
auc_roc_rf <- auc(roc_curve)
print(paste("AUC-ROC:", auc_roc_rf))
plot(roc_curve, main = "RF-OverSampled Data-ROC Curve(Important Features)", col = "blue", lwd = 2)

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
#print(results_df)


#=======================================================================================================================================================================
# Assuming 'final_data' is your dataset with a binary target variable named 'successful_app'
# Split the data into training and testing sets
model_name <- 'RANDOM FOREST(undersampled-Important Features)'
set.seed(123)
train_indices <- sample(1:nrow(undersampled_data), 0.7 * nrow(undersampled_data))
train_data <- undersampled_data[train_indices, ]
test_data <- undersampled_data[-train_indices, ]

# Fit Random Forest model
library(randomForest)
random_forest_model <- randomForest(successful_app ~ rating_count_tot + rating_count_ver +
                                      user_rating_ver + older_ver_rating, data = train_data)

# Make predictions on the test set
predictions <- predict(random_forest_model, newdata = test_data)

# Evaluate the model
confusion_matrix <- table(test_data$successful_app, predictions)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate metrics
accuracy_rf <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision_rf <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall_rf <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)

print(paste("Accuracy:", accuracy_rf))
print(paste("Precision:", precision_rf))
print(paste("Recall:", recall_rf))
print(paste("F1-Score:", f1_score_rf))

# Plot ROC curve
library(pROC)
roc_curve <- roc(test_data$successful_app, as.numeric(predictions))
auc_roc_rf <- auc(roc_curve)
print(paste("AUC-ROC:", auc_roc_rf))
plot(roc_curve, main = "RF-UnderSampled Data-ROC Curve(Important Features)", col = "blue", lwd = 2)

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


