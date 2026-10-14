
# ==========================================
# RetainIQ - Customer Churn Prediction
# Logistic Regression vs Random Forest
# ==========================================

library(caret)
library(dplyr)
library(ranger)
library(pROC)
library(PRROC)

set.seed(123)

# 1. Load cleaned dataset
data <- read.csv(
  "output/cleaned_customers.csv",
  stringsAsFactors = FALSE
)

# Remove customer ID if it exists
data <- data %>%
  select(-any_of("customerID"))

# Remove missing values
data <- na.omit(data)

# Set target factor levels
# "Yes" is the churn event
data$Churn <- factor(
  data$Churn,
  levels = c("No", "Yes")
)

# Convert character columns to factors
for (col in names(data)) {
  if (col != "Churn" && is.character(data[[col]])) {
    data[[col]] <- as.factor(data[[col]])
  }
}

cat("Dataset loaded:", nrow(data), "customers\n")

# 2. Split data into training and testing sets
train_index <- createDataPartition(
  data$Churn,
  p = 0.80,
  list = FALSE
)

train_data <- data[train_index, , drop = FALSE]
test_data <- data[-train_index, , drop = FALSE]

# Ensure consistent target levels
train_data$Churn <- factor(
  train_data$Churn,
  levels = c("No", "Yes")
)

test_data$Churn <- factor(
  test_data$Churn,
  levels = c("No", "Yes")
)

cat("Training rows:", nrow(train_data), "\n")
cat("Testing rows:", nrow(test_data), "\n")

# 3. Train Logistic Regression
cat("\nTraining Logistic Regression...\n")

log_model <- train(
  Churn ~ .,
  data = train_data,
  method = "glm",
  family = binomial(),
  trControl = trainControl(method = "none")
)

# 4. Train Random Forest
cat("Training Random Forest...\n")

rf_model <- train(
  Churn ~ .,
  data = train_data,
  method = "ranger",
  trControl = trainControl(
    method = "none",
    classProbs = TRUE
  ),
  tuneGrid = data.frame(
    mtry = max(
      1,
      floor(sqrt(ncol(train_data) - 1))
    ),
    splitrule = "gini",
    min.node.size = 10
  ),
  num.trees = 300
)

# 5. Predict churn probabilities
cat("Generating predictions...\n")

log_predictions <- predict(
  log_model,
  newdata = test_data,
  type = "prob"
)

rf_predictions <- predict(
  rf_model,
  newdata = test_data,
  type = "prob"
)

# Confirm probability columns
cat("\nLogistic Regression columns:\n")
print(names(log_predictions))

cat("\nRandom Forest columns:\n")
print(names(rf_predictions))

# Extract probability of churn
log_prob <- log_predictions[, "Yes"]
rf_prob <- rf_predictions[, "Yes"]

actual <- test_data$Churn == "Yes"

# 6. Evaluation function
evaluate_model <- function(actual, probability, model_name) {
  
  predicted <- probability >= 0.50
  
  TP <- sum(predicted & actual)
  FP <- sum(predicted & !actual)
  FN <- sum(!predicted & actual)
  
  precision <- if ((TP + FP) == 0) {
    0
  } else {
    TP / (TP + FP)
  }
  
  recall <- if ((TP + FN) == 0) {
    0
  } else {
    TP / (TP + FN)
  }
  
  f1 <- if ((precision + recall) == 0) {
    0
  } else {
    2 * precision * recall / (precision + recall)
  }
  
  roc_auc <- as.numeric(
    pROC::auc(
      pROC::roc(
        response = actual,
        predictor = probability,
        levels = c(FALSE, TRUE),
        direction = "<",
        quiet = TRUE
      )
    )
  )
  
  pr_auc <- PRROC::pr.curve(
    scores.class0 = probability[actual],
    scores.class1 = probability[!actual],
    curve = FALSE
  )$auc.integral
  
  data.frame(
    Model = model_name,
    ROC_AUC = roc_auc,
    PR_AUC = pr_auc,
    Precision = precision,
    Recall = recall,
    F1 = f1
  )
}

# 7. Compare both models
cat("\n===== MODEL COMPARISON =====\n")

results <- rbind(
  evaluate_model(
    actual,
    log_prob,
    "Logistic Regression"
  ),
  evaluate_model(
    actual,
    rf_prob,
    "Random Forest"
  )
)

print(
  data.frame(
    Model = results$Model,
    round(results[, -1], 3)
  )
)

# 8. Select the model with the best PR-AUC
best_index <- which.max(results$PR_AUC)
best_name <- results$Model[best_index]

if (best_name == "Random Forest") {
  best_model <- rf_model
  best_prob <- rf_prob
} else {
  best_model <- log_model
  best_prob <- log_prob
}

cat("\nSelected model:", best_name, "\n")

# 9. Find a threshold for the best F1 score
# Demonstration only: use a separate validation set
# for threshold selection in a final evaluation.

thresholds <- seq(0.10, 0.90, by = 0.05)

f1_scores <- sapply(thresholds, function(threshold) {
  
  predicted <- best_prob >= threshold
  
  TP <- sum(predicted & actual)
  FP <- sum(predicted & !actual)
  FN <- sum(!predicted & actual)
  
  precision <- if ((TP + FP) == 0) {
    0
  } else {
    TP / (TP + FP)
  }
  
  recall <- if ((TP + FN) == 0) {
    0
  } else {
    TP / (TP + FN)
  }
  
  if ((precision + recall) == 0) {
    0
  } else {
    2 * precision * recall / (precision + recall)
  }
})

best_threshold <- thresholds[which.max(f1_scores)]

cat("Selected demonstration threshold:",
    best_threshold, "\n")


# 10. Create customer-level predictions

test_rows <- setdiff(seq_len(nrow(data)), train_index)

predictions <- data.frame(
  Customer_Row = test_rows,
  Actual_Churn = ifelse(actual, "Yes", "No"),
  Churn_Probability = round(best_prob, 4),
  Risk = ifelse(
    best_prob >= 0.65,
    "High",
    ifelse(best_prob >= 0.35, "Medium", "Low")
  )
)


# 11. Save all outputs
dir.create("output", showWarnings = FALSE)

write.csv(
  results,
  "output/model_comparison.csv",
  row.names = FALSE
)

write.csv(
  predictions,
  "output/churn_predictions.csv",
  row.names = FALSE
)

saveRDS(
  best_model,
  "output/best_churn_model.rds"
)

saveRDS(
  best_threshold,
  "output/best_threshold.rds"
)

# 12. Final summary
cat("\n================================\n")
cat("RETAINIQ MODEL TRAINING COMPLETE\n")
cat("================================\n")
cat("Best model:", best_name, "\n")
cat("Test customers:", nrow(test_data), "\n")
cat("ROC-AUC:", round(results$ROC_AUC[best_index], 3), "\n")
cat("PR-AUC:", round(results$PR_AUC[best_index], 3), "\n")
cat("F1 score:", round(results$F1[best_index], 3), "\n")
cat("\nFiles saved in the output folder:\n")
cat("- model_comparison.csv\n")
cat("- churn_predictions.csv\n")
cat("- best_churn_model.rds\n")
cat("- best_threshold.rds\n")
