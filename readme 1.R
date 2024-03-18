#Scripts Explanation:

#Data Cleaning.R:
Read the data, checked for data discrpencies.

#Feature Engineering.R:
Created new features and transformed existing features as per the project requirements.

#Data Preparation for Modelling-Binary Classification.R:
Created dataframes for the modelling by creating binary target variable and performed Scaling and Encoding. To handle data imbalance did Random sampling(UnderSampling and overSampling).

#Data Preparation for Modelling-MultiClass Classification.R:
Created dataframes for the modelling by creating performed Scaling and Encoding.

#Logistic Regression-KFold.R :
Perfomed Binary Classification both OverSampled and UnderSampled Data KFold.

#Decision Tree-KFold.R:
Perfomed Binary Classification both OverSampled and UnderSampled Data using KFold.

#Random Forest.R:
Perfomed Binary Classification both OverSampled and UnderSampled Data using KFold.

#KNN-KFold.R:
Perfomed Binary Classification both OverSampled and UnderSampled Data using KFold.

#Naive Bayes-KFold.R:
Perfomed Binary Classification both OverSampled and UnderSampled Data using KFold.

#Naive Bayes-KFold.R:
Perfomed Binary Classification both OverSampled and UnderSampled Data using KFold.

#Binary Classification-Full.R:
Single script whic contains the code for all the binary classification models.

#Multiclass Classification-Full.R:
Single script whic contains the code for all the MultiClass classification models.

#Evaluation Table.R:
Script to create a table to append and compare the metrics of each model.

#Important Features-Graph.R:
Script to find the important features that are identified in each model.

Order of Execution of the scripts:
For Binary Classification:
Step-1: Execute Data Cleaning.R:
Step-2: Execute Feature Engineering.R
Step-3: Execute Data Preparation for Modelling-Binary Classification.R
Step-4: Execute Evaluation Table.R
Step-5: Execute Logistic Regression-KFold.R,Decision Tree-KFold.R,Random Forest.R,KNN-KFold.R,Naive Bayes-KFold.R,Naive Bayes-KFold.R in sequence or can also execute the Binary Classification-Full.R.

For MultiClass Classification:
Step-1: Execute Data Cleaning.R:
Step-2: Execute Feature Engineering.R
Step-3: Execute Data Preparation for Modelling-MultiClass Classification.R
Step-4: Execute Multiclass Classification-Full.R
  