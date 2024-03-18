#Feature Engineering:
#Below are the variables that will be created and derived from the data

#1.Convert the size from bytes to mb:
final_table$size_mb <- round(final_table$size_bytes / (1024 * 1024), digits =2)

#2.Paid/ Non-paid- Paid>0, non-paid=0
final_table$paid_status <- ifelse(final_table$price > 0, "Paid", "Free")


#3.Review count for older versions:Total review count-current count
final_table$older_ver_rating <- final_table$rating_count_tot - final_table_1$rating_count_ver




