
# RetainIQ — Customer Churn Prediction

RetainIQ is a customer churn analytics project built using R and
Shiny. It predicts customer churn risk and helps businesses
prioritize retention activities.

## Features

- Customer churn prediction using Logistic Regression and Random Forest
- Churn-risk segmentation: High, Medium, and Low
- Model evaluation using ROC-AUC, PR-AUC, Precision, Recall, and F1
- Estimated monthly revenue exposure
- Ranked customer retention action list
- Interactive Shiny dashboard
- CSV report downloads

## Technology Stack

- R and RStudio
- caret and ranger
- dplyr and ggplot2
- pROC and PRROC
- Shiny, DT, and Plotly

## How to Run

1. Install the required R packages.
2. Place the dataset in the data folder.
3. Run `01_data_analysis.R`.
4. Run `02_churn_model.R`.
5. Run `03_retention_engine.R`.
6. Run `app.R` to launch the dashboard.

## Dataset

This project uses the IBM Telco Customer Churn dataset.
Check the original dataset source and its usage terms before
redistributing the data.

## Important Note

Revenue exposure is an estimate based on predicted churn
probability and monthly charges. It does not guarantee actual
revenue loss or retention success.

## Project Status

Educational machine-learning and customer-retention analytics
project.
