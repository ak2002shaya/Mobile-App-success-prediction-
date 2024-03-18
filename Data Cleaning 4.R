#-----------------------------------------Data Preparation and Cleaning-------------------------------------
# loading the required packages
library(dplyr)
library(ggplot2)
library(corrplot)

#Duplicates handling, null value check, outliers.

# clears all objects in "global environment"
rm(list=ls())

# clears the console area
cat("\014")
 
# Starts random numbers at the same sequence
set.seed(123)

# prints the current working directory
print(paste("WORKING DIRECTORY: ",getwd()))

#to view the list of files present in the directory
dir()

#reading the input files:
app_data <- read.csv('AppleStore.csv')
#app_data

app_desc <- read.csv('appleStore_description.csv')
#app_desc

#checking the number of rows and columns in the data
dim(app_data)
dim(app_desc)

#checking the datatype of the data
str(app_data)
str(app_desc)

# checking for duplicates in the whole dataset
sum(duplicated(app_data))
sum(duplicated(app_desc))

#There are no row level duplicates present on the dataset. Since we are going to analyse on the apps, we will
#go ahead on step and check if there are app values repeated in the dataset.


# Using dplyr to find duplicate values
duplicate_counts <- app_data |>
  add_count(track_name) |>
  filter(n > 1) |>
  distinct()
duplicate_counts

#We have identified that there are two apps "VR Roller Coaster" and "Mannequin Challenge" are repeated in
#the dataset, which will be difficult for our analysis. So we will consider the record with latest version.

df_duplicate <- data.frame(track_name=unique(duplicate_counts$track_name))
df_duplicate

#Finding the old version of the duplicated apps
df_1 <- duplicate_counts %>%
  group_by(track_name) %>%
  summarise(max = min(ver, na.rm=TRUE))

#creating conditions to remove the duplicated apps and retain the apps with latest version
values_to_remove_1 <- c(df_1$max)
values_to_remove_2 <- c(df_1$track_name)
rows_to_remove <- app_data$ver %in% values_to_remove_1 & app_data$track_name %in% values_to_remove_2

#applying the condition on the table and create a new clean table
app_data_clean <- app_data[!rows_to_remove,]
dim(app_data_clean)

#joining with the description table:
final_table_1 <- inner_join(app_data_clean,app_desc, by='id')
final_table_1

final_table <- select(final_table_1,X,id,track_name.x,size_bytes.x,currency,price,rating_count_tot,rating_count_ver,user_rating,user_rating_ver,
ver,cont_rating,prime_genre,sup_devices.num,ipadSc_urls.num,lang.num,vpp_lic,app_desc)

final_table <- final_table %>%
  rename(
    track_name = track_name.x,
    size_bytes = size_bytes.x
    )

final_table

names(final_table)

#find the datatype of each columns:
column_data_types <- function(dataframe) {
  sapply(dataframe, function(x) class(x))
}

result <- column_data_types(final_table)

#Correlation Matrix:
numerical_columns <- sapply(final_table_binary, is.numeric)
numerical_data <- final_table[, numerical_columns]
correlation_matrix <- cor(numerical_data)
corrplot(correlation_matrix, method = "color")
corrplot(correlation_matrix, method = "color")

numerical_data
