# ============================================================
# Script 05: Simple linear regression
# Model: resting_bp_systolic ~ age
# Package: moderndive (regression tables and summaries)
# ============================================================

# --- Load packages and clean data ----------------------------
library(tidyverse)   
library(moderndive)   

# readRDS() loads the cleaned data with the factor levels intact
heart <- readRDS("data/clean/heart_clean.rds")

# --- 1. Fit the model ----------------------------------------
# lm() means "linear model"
# The formula reads: resting_bp_systolic is explained by age
# The outcome goes on the LEFT of ~ and the predictor on the RIGHT
# data = heart tells lm() which data frame holds these columns
# The result is stored in an object called model_age
model_age <- lm(resting_bp_systolic ~ age, data = heart)

# --- 2. Regression table (moderndive) ------------------------
# get_regression_table() gives a tidy table with one row per coefficient:
#   estimate  = the fitted value of the intercept or slope
#   std_error = how much that estimate would vary from sample to sample
#   statistic = the t value (estimate divided by std_error)
#   p_value   = probability of a result this extreme if the true slope were 0
#   lower_ci / upper_ci = the 95% confidence interval
# digits = 3 rounds the output to 3 decimal places
get_regression_table(model_age, digits = 3)

# --- 3. Model fit summary (moderndive) -----------------------
# get_regression_summaries() gives one row of overall fit measures:
#   r_squared     = share of the variation in BP explained by age (0 to 1)
#   adj_r_squared = the same, adjusted for the number of predictors
#   mse / rmse    = typical size of the prediction errors
#   sigma         = the standard deviation of the residuals
#   statistic / p_value = the overall F test
get_regression_summaries(model_age)

# --- 4. Fitted values and residuals (moderndive) -------------
# get_regression_points() gives one row per patient:
#   resting_bp_systolic = the observed BP
#   age                 = the predictor value
#   resting_bp_systolic_hat = the BP the model predicts
#   residual            = observed minus predicted (the error)
# head() shows only the first 6 rows
regression_points <- get_regression_points(model_age)
head(regression_points)

# --- 5. Plot the fitted line ---------------------------------
# geom_point() draws each patient; alpha = 0.15 makes points see-through
# so dense areas look darker
# geom_smooth(method = "lm") draws the least-squares line
# se = TRUE adds the 95% confidence band around the line
p_simple <- ggplot(heart, aes(x = age, y = resting_bp_systolic)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", se = TRUE, colour = "red") +
  labs(title = "Systolic BP versus age",
       subtitle = "Simple linear regression line",
       x = "Age (years)", y = "Systolic BP (mmHg)") +
  theme_minimal()

p_simple

# --- 6. Predict BP for specific ages -------------------------
# predict() uses the fitted model to estimate BP for new values
# newdata must be a data frame with the same column name as the predictor
# tibble() builds a small data frame holding the ages we want
new_ages <- tibble(age = c(30, 50, 70))

# interval = "confidence" adds the 95% confidence interval for the mean BP
# at each age; bind_cols() attaches the predictions next to the ages
bind_cols(new_ages,
          as_tibble(predict(model_age, newdata = new_ages, interval = "confidence")))

# --- 7. Save results -----------------------------------------
# write_csv() saves the regression table as a CSV
get_regression_table(model_age, digits = 3) |>
  write_csv("outputs/tables/simple_regression_age.csv")

# ggsave() saves the plot; width and height are in inches
ggsave("outputs/plots/simple_regression_age.png", p_simple, width = 7, height = 5)

# saveRDS() keeps the model object for later use (diagnostics, comparisons)
saveRDS(model_age, "outputs/models_simple_age.rds")
