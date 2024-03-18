#---------------------------------------------Preparing the data for Modelling----------------------------------------------------------------------

#final_table_multiclass = Actual cleaned data
#final_data = Scaled and Encoded data
#final_data_woscaled = Encoded data

#Taking the copy of cleaned table:
final_table_multiclass <- final_table

#Creating Binary Target variable based on the "user_rating" column:
final_table_multiclass$rating_category <- cut(final_table_multiclass$user_rating, 
                                      breaks = c(0, 2, 3, 4, 5), 
                                      labels = c(1, 2, 3, 4), 
                                      include.lowest = TRUE, 
                                      right = FALSE)
final_table_multiclass$rating_category <- factor(as.character(final_table_multiclass$rating_category))
class(final_table_multiclass$rating_category )

#Removing Insignificant variables:
#dropping byte size:
final_table_multiclass$size_bytes <- NULL

#dropping X column:
final_table_multiclass$X <- NULL

#dropping id column:
final_table_multiclass$id<- NULL

#dropping track_name column:
final_table_multiclass$track_name <- NULL

#dropping ver column:
final_table_multiclass$ver <- NULL

#dropping currency column:
final_table_multiclass$currency <- NULL

#dropping app_desc column:
final_table_multiclass$app_desc <- NULL

#checking the columns:
names(final_table_multiclass)

#find the datatype of each columns:
column_data_types <- function(dataframe) {
  sapply(dataframe, function(x) class(x))
}
result <- column_data_types(final_table_multiclass)
print(result)
#==============================================================================================================================
#Splitting the data into Dependent and Independent variable:
target_column <- "rating_category"

features <- final_table_multiclass[, !(names(final_table_multiclass) %in% target_column), drop = FALSE]

df_target <- as.data.frame(final_table_multiclass[[target_column]])
colnames(df_target) <- "rating_category"

head(features)
head(df_target)
#==============================================================================================================================
#Encoding the categorical values:

# Load necessary libraries
library(dplyr)

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
final_data_multiclass <- cbind(df_features, df_target)
head(final_data)


#find the datatype of each columns:
column_data_types <- function(dataframe) {
  sapply(dataframe, function(x) class(x))
}
result <- column_data_types(final_data)
print(result)
#================================================================================================================================================

#Creating non-scaled datafame:
features <- as.data.frame(final_table_multiclass[, !(names(final_table_multiclass) %in% target_column), drop = FALSE])
names(features)

numerical_columns <- sapply(features, is.numeric)
numeric_dataframe <- final_table_multiclass[,numerical_columns]
head(numeric_dataframe)
names(numeric_dataframe)

features_woscaled <- cbind(numeric_dataframe,one_hot_encoded)
head(features_woscaled)
names(features_woscaled)

final_data_multiclass_woscaled <- cbind(features_woscaled,df_target)
head(features_woscaled)
names(features_woscaled)

column_data_types <- function(dataframe) {
  sapply(dataframe, function(x) class(x))
}
result <- column_data_types(features_woscaled)
print(result)
#================================================================================================================================================