
# ==========================================
# RetainIQ - Retention Intelligence Engine
# ==========================================

library(dplyr)

# 1. Load model predictions and customer data
predictions <- read.csv(
  "output/churn_predictions.csv"
)

data <- read.csv(
  "output/cleaned_customers.csv"
)

# Check that row references match
stopifnot(
  all(predictions$Customer_Row >= 1),
  all(predictions$Customer_Row <= nrow(data))
)

# 2. Add customer information
predictions$MonthlyCharges <- data$MonthlyCharges[
  predictions$Customer_Row
]

# Recover customer IDs from original dataset if available
raw_file <- "data/Telco-Customer-Churn.csv"

if (file.exists(raw_file)) {
  original <- read.csv(
    raw_file,
    stringsAsFactors = FALSE
  )
  
  original$TotalCharges <- suppressWarnings(
    as.numeric(trimws(original$TotalCharges))
  )
  
  original <- na.omit(original)
  
  if (nrow(original) == nrow(data) &&
      "customerID" %in% names(original)) {
    predictions$Customer <- original$customerID[
      predictions$Customer_Row
    ]
  } else {
    predictions$Customer <- paste0(
      "Customer-", predictions$Customer_Row
    )
  }
} else {
  predictions$Customer <- paste0(
    "Customer-", predictions$Customer_Row
  )
}

# 3. Calculate monthly revenue exposure
predictions$Estimated_Monthly_Revenue_At_Risk <-
  predictions$Churn_Probability *
  predictions$MonthlyCharges

# 4. Assign retention recommendations
predictions$Priority <- ifelse(
  predictions$Risk == "High",
  "High",
  ifelse(predictions$Risk == "Medium", "Medium", "Low")
)

predictions$Recommended_Action <- case_when(
  predictions$Risk == "High" ~
    "Contact customer and investigate concerns",
  predictions$Risk == "Medium" ~
    "Send personalized engagement offer",
  TRUE ~
    "Continue regular engagement"
)

# 5. Rank customers by estimated monthly exposure
predictions <- predictions %>%
  arrange(desc(Estimated_Monthly_Revenue_At_Risk)) %>%
  mutate(
    Retention_Rank = row_number()
  )

# 6. Create top 100 retention contact list
top_customers <- predictions %>%
  filter(Risk != "Low") %>%
  slice_head(n = 100)

# 7. Calculate summary metrics
high_risk_count <- sum(predictions$Risk == "High")
medium_risk_count <- sum(predictions$Risk == "Medium")

revenue_at_risk <- sum(
  predictions$Estimated_Monthly_Revenue_At_Risk,
  na.rm = TRUE
)

cat("\n====================================\n")
cat("RETAINIQ RETENTION REPORT\n")
cat("====================================\n")

cat("Customers analyzed:", nrow(predictions), "\n")
cat("High-risk customers:", high_risk_count, "\n")
cat("Medium-risk customers:", medium_risk_count, "\n")

cat(
  "Estimated monthly revenue exposure: ",
  round(revenue_at_risk, 2), "\n"
)

# 8. Save reports
dir.create("output", showWarnings = FALSE)

write.csv(
  predictions,
  "output/retention_report.csv",
  row.names = FALSE
)

write.csv(
  top_customers,
  "output/top_100_retention_actions.csv",
  row.names = FALSE
)

cat("\nReports saved successfully:\n")
cat("output/retention_report.csv\n")
cat("output/top_100_retention_actions.csv\n")
