
# RetainIQ - Customer Churn Data Analysis

library(dplyr)
library(ggplot2)

# Load dataset
data <- read.csv(
  "data/Telco-Customer-Churn.csv",
  stringsAsFactors = FALSE
)

# Clean column names
names(data) <- trimws(names(data))

# Remove customer ID from analysis
data$customerID <- NULL

# Convert TotalCharges to numeric
data$TotalCharges <- as.numeric(
  trimws(data$TotalCharges)
)

# Remove rows with missing values
data <- na.omit(data)

# Display dataset information
cat("===== RETAINIQ DATA ANALYSIS =====\n")
cat("Total customers:", nrow(data), "\n")
cat("Total columns:", ncol(data), "\n\n")

# Churn distribution
print(table(data$Churn))

# Calculate churn percentage
churn_rate <- mean(data$Churn == "Yes") * 100
cat("\nChurn rate:", round(churn_rate, 2), "%\n")

# Average monthly charges by churn
charges <- data %>%
  group_by(Churn) %>%
  summarise(
    Average_Monthly_Charges =
      mean(MonthlyCharges, na.rm = TRUE),
    .groups = "drop"
  )

print(charges)

# Plot churn distribution
p <- ggplot(data, aes(x = Churn, fill = Churn)) +
  geom_bar() +
  labs(
    title = "RetainIQ - Customer Churn Distribution",
    x = "Customer Churn",
    y = "Number of Customers"
  ) +
  theme_minimal()

print(p)

# Save cleaned data
dir.create("output", showWarnings = FALSE)

write.csv(
  data,
  "output/cleaned_customers.csv",
  row.names = FALSE
)

cat("\nAnalysis completed successfully!\n")
cat("Cleaned data saved in output/cleaned_customers.csv\n")
