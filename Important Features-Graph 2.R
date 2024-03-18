# Load necessary libraries
library(ggplot2)
library(tidyr)
#==============================================================================================================================
# Random Forest
rf_importance <- importance(random_forest_model)
rf_df <- data.frame(Feature = rownames(rf_importance), Importance = rf_importance[, "MeanDecreaseGini"])

# Plot for Random Forest
ggplot(rf_df, aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Random Forest Variable Importance", x = "Feature", y = "Importance")
#==============================================================================================================================
# Decision Tree
dt_importance <- varImp(decision_tree_model)
dt_df <- data.frame(Feature = rownames(dt_importance), Importance = dt_importance[, "Overall"])

# Plot for Decision Tree
ggplot(dt_df, aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Decision Tree Variable Importance", x = "Feature", y = "Importance")

