# Load necessary libraries
library(dplyr)
library(ggplot2)
library(rdd)

# Assuming you have a data frame called data_counties
# Define the cutoff (margin at 0)
cutoff <- 0

# Create a running variable (e.g., margin)
# Assuming 'vote_margin' is the running variable column in your dataset
data_counties <- data_counties %>%
  mutate(running_variable = vote_margin)

# Ensure that all necessary columns are numeric
columns_to_check <- c("gdp_price_deflator", "total_income_dollars", "population_count", "avg_annual_precipitation", "avg_annual_temperature",
                      "election_candidate_votes_DEMOCRAT", "election_total_votes", "fair_market_rent_0_bedrooms",
                      "fair_market_rent_1_bedroom", "fair_market_rent_3_bedrooms", "fair_market_rent_4_bedrooms")

# Convert columns to numeric and handle non-numeric values
data_counties <- data_counties %>%
  mutate(across(all_of(columns_to_check), as.numeric, .names = "cleaned_{col}"))

# Function to perform RDD analysis and plot results for each column
perform_rdd_analysis <- function(data, running_variable, outcome_variable, cutoff) {
  # Remove rows with non-finite values in the running and outcome variables
  data <- data %>% filter(is.finite(!!sym(running_variable)) & is.finite(!!sym(outcome_variable)))
  
  formula <- as.formula(paste(outcome_variable, "~", running_variable))
  rdd_model <- RDestimate(formula, cutpoint = cutoff, data = data)
  
  # Print summary of the RDD model
  print(paste("Summary of RDD for:", outcome_variable))
  print(summary(rdd_model))
  
  # Plot the results
  data_below <- data %>% filter(!!sym(running_variable) < cutoff)
  data_above <- data %>% filter(!!sym(running_variable) >= cutoff)
  
  p <- ggplot(data, aes_string(x = running_variable, y = outcome_variable)) +
    geom_point() +
    geom_smooth(data = data_below, method = "lm", aes_string(x = running_variable, y = outcome_variable), color = "blue") +
    geom_smooth(data = data_above, method = "lm", aes_string(x = running_variable, y = outcome_variable), color = "red") +
    geom_vline(xintercept = cutoff, linetype = "dashed") +
    labs(title = paste("Regression Discontinuity Design for", outcome_variable),
         x = "Running Variable (Margin)",
         y = outcome_variable)
  
  print(p)
}

# Loop through each column and perform the RDD analysis
for (column in columns_to_check) {
  perform_rdd_analysis(data_counties, "running_variable", paste("cleaned", column, sep = "_"), cutoff)
}