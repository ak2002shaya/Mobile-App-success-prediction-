#---------------------------------------------------------------Evaluation table creation----------------------------------------------------------------------
# Creating an empty dataframe to store results
results_df <- data.frame(
  Model = character(),
  Accuracy = character(),
  Precision = numeric(),
  Recall = numeric(),
  F1_Score = numeric(),
  AUC_ROC = numeric(),
  stringsAsFactors = FALSE
)


