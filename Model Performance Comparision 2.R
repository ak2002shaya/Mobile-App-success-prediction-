# Load necessary libraries
library(ggplot2)
library(tidyr)

# Assuming 'results_df' is your dataframe
# You can directly use your existing dataframe

# Melt the dataframe for better visualization
melted_df <- gather(results_df, Metric, Value, -Model)

# Plotting
ggplot(melted_df, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Model Comparison", x = "Model", y = "Metric Value", fill = "Metric")