#---------------------------------------------Preparing the data for Modelling----------------------------------------------------------------------
# Load necessary libraries
library(dplyr)

#Below are the datasets that will be created for the Machine Learning Model:
#---------------------------------------------------------------------------
#final_table_binary = Actual cleaned data
#final_data = Scaled and Encoded data
#final_data_woscaled = Encoded data
#----------------------------------------------------------------------------

#Taking the copy of cleaned table:
final_table_binary <- final_table

#Creating Binary Target variable based on the "user_rating" column:
final_table_binary$successful_app <- ifelse(final_table_binary$user_rating >= 4, 1,0)  # will be creating a separate dataframe with this column.
final_table_binary$successful_app <- factor(as.character(final_table_binary$successful_app))
class(final_table_binary$successful_app )

#Removing Insignificant variables:
#dropping byte size:
final_table_binary$size_bytes <- NULL

#dropping X column:
final_table_binary$X <- NULL

#dropping id column:
final_table_binary$id<- NULL

#dropping track_name column:
final_table_binary$track_name <- NULL

#dropping ver column:
final_table_binary$ver <- NULL

#dropping currency column:
final_table_binary$currency <- NULL

#dropping app_desc column:
final_table_binary$app_desc <- NULL

#checking the columns:
names(final_table_binary)

#find the datatype of each columns:
column_data_types <- function(dataframe) {
  sapply(dataframe, function(x) class(x))
}
result <- column_data_types(final_table_binary)
print(result)
#==============================================================================================================================
#Splitting the data into Dependent and Independent variable:
target_column <- "successful_app"

features <- final_table_binary[, !(names(final_table_binary) %in% target_column), drop = FALSE]

df_target <- as.data.frame(final_table_binary[[target_column]])
colnames(df_target) <- "successful_app"

head(features)
head(df_target)
#==============================================================================================================================
#Encoding the categorical values:

# Identify non-numeric columns
non_numeric_columns <- sapply(features, function(x) !is.numeric(x))
non_numeric_columns

# Extract non-numeric columns
non_numeric_data <- features[, non_numeric_columns]

# Perform one-hot encoding
one_hot_encoded <- model.matrix(~ . - 1, data = non_numeric_data)
one_hot_encoded
#==============================================================================================================================
#Scaling the numerical values:

# Select only numerical columns
numerical_columns <- sapply(features, is.numeric)

# Min-Max Scaling function
min_max_scale <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Apply Min-Max scaling to numerical columns
scaled_data <- as.data.frame(apply(features[, numerical_columns], 2, min_max_scale))


#Merging the scaled numerical columns and the encoded categorical columns:
df_features <- cbind(scaled_data, one_hot_encoded)
# Print the scaled dataframe
head(df_features)


#find the datatype of each columns:
column_data_types <- function(dataframe) {
  sapply(dataframe, function(x) class(x))
}
result <- column_data_types(df_features)
#================================================================================================================================================

#Combining the df_feature and df_target to create a single final dataframe:
final_data <- cbind(df_features, df_target)
head(final_data)


#find the datatype of each columns:
column_data_types <- function(dataframe) {
  sapply(dataframe, function(x) class(x))
}
result <- column_data_types(final_data)
print(result)
#================================================================================================================================================

#Creating non-scaled datafame:
features <- as.data.frame(final_table_binary[, !(names(final_table_binary) %in% target_column), drop = FALSE])
names(features)

numerical_columns <- sapply(features, is.numeric)
numeric_dataframe <- final_table_binary[,numerical_columns]
head(numeric_dataframe)
names(numeric_dataframe)

features_woscaled <- cbind(numeric_dataframe,one_hot_encoded)
head(features_woscaled)
names(features_woscaled)

final_data_woscaled <- cbind(features_woscaled,df_target)
head(features_woscaled)
names(features_woscaled)

column_data_types <- function(dataframe) {
  sapply(dataframe, function(x) class(x))
}
result <- column_data_types(features_woscaled)
print(result)
#=================================================================================================================================================
#Handling data imbalance(Binary Classification):
#-----------------------------------------------
# Find the number of unique classes
unique_classes <- unique(final_table_binary$successful_app)
num_classes <- length(unique_classes)

# Calculate the percentage of each class
class_percentages <- table(final_table_binary$successful_app) / length(final_table_binary$successful_app) * 100
cat("Number of Classes:", num_classes, "\n\n")
cat("Class\t\tPercentage\n")
cat("-----------------------\n")
for (class in unique_classes) {
  cat(class, "\t\t", class_percentages[class], "%\n")
}
#================================================================================================================================================
# Identify the minority and majority classes
minority_class <- final_table_binary[final_table_binary$successful_app == 0, ]
majority_class <- final_table_binary[final_table_binary$successful_app == 1, ]

# Oversampling 
oversampled_data <- rbind(final_table_binary, rep(minority_class, times = 1))
# Undersampling
undersampled_data <- rbind(minority_class, majority_class[sample(nrow(majority_class), size = nrow(minority_class)), ])

# Shuffle the data
oversampled_data <- oversampled_data[sample(nrow(oversampled_data)), ]
undersampled_data <- undersampled_data[sample(nrow(undersampled_data)), ]

names(oversampled_data)
names(undersampled_data)
#=================================================================================================================================================
unique_classes <- unique(oversampled_data$successful_app)
num_classes <- length(unique_classes)

# Calculate the percentage of each class
class_percentages <- table(oversampled_data$successful_app) / length(oversampled_data$successful_app) * 100

# Display the results
cat("Number of Classes:", num_classes, "\n\n")
cat("Class\t\tPercentage\n")
cat("-----------------------\n")
for (class in unique_classes) {
  cat(class, "\t\t", class_percentages[class], "%\n")
}
#=================================================================================================================================================

# Identify the minority and majority classes
minority_class <- final_data[final_data$successful_app == 0, ]
majority_class <- final_data[final_data$successful_app == 1, ]

# Oversampling
oversampled_data_1 <- rbind(final_data, rep(minority_class, times = 1))

# Undersampling
undersampled_data_1 <- rbind(minority_class, majority_class[sample(nrow(majority_class), size = nrow(minority_class)), ])

# Shuffle the data
oversampled_data_scaled <- oversampled_data_1[sample(nrow(oversampled_data_1)), ]
undersampled_data_scaled <- undersampled_data_1[sample(nrow(undersampled_data_1)), ]
#=================================================================================================================================================

names(oversampled_data)
names(undersampled_data)
names(oversampled_data_scaled)
names(undersampled_data_scaled)

# Assuming your categorical variable is stored in a column named 'class' in your dataset
class_counts <- table(final_table_binary$successful_app)

# Print the frequency table
print(class_counts)


  