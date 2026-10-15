# ============================================================
# Script 06: Multiple linear regression
# Outcome: resting_bp_systolic

# Load packages
library(tidyverse)    
library(moderndive)

heart <- readRDS("data/clean/heart_clean.rds")

# --- 1. Fit the full model -----------------------------------
# Each + adds another predictor to the right-hand side of the formula
# Numeric predictors get one slope each
# Factors (sex, smoker_status, chest_pain_type) are automatically split
# into 0/1 dummy variables, one for every level EXCEPT the reference level:
#   sex             -> compared with Female
#   smoker_status   -> compared with Never
#   chest_pain_type -> compared with Asymptomatic
# We leave out hdl, ldl and triglycerides because they overlap with
# cholesterol_total (checked in Step 7), and patient_id because it is only a label
model_full <- lm(resting_bp_systolic ~ age + bmi + cholesterol_total +
                   stress_score + daily_steps + sleep_hours +
                   exercise_minutes_per_week +
                   sex + smoker_status + chest_pain_type,
                 data = heart)

# --- 2. Regression table (moderndive) ------------------------
get_regression_table(model_full, digits = 3)

# --- 3. Overall fit ------------------------------------------

get_regression_summaries(model_full)

# --- 4. Compare with the simple model ------------------------
# Reload the simple model from Script 05 
model_age <- readRDS("outputs/models_simple_age.rds")

# bind_rows() stacks the two one-row summaries into one table
# .id adds a column naming which model each row came from
bind_rows(
  simple = get_regression_summaries(model_age),
  full   = get_regression_summaries(model_full),
  .id = "model"
) |>
  select(model, r_squared, adj_r_squared, rmse)   # keep the columns we compare

# --- 5. Do the categorical variables earn their place? -------
# A factor with several levels has several coefficients, so we test the
# whole factor at once by comparing the model with and without it.
# anova() runs an F test; a small p-value means the factor adds something real.
model_no_cat <- lm(resting_bp_systolic ~ age + bmi + cholesterol_total +
                     stress_score + daily_steps + sleep_hours +
                     exercise_minutes_per_week,
                   data = heart)

# Compare the numeric-only model with the full model
anova(model_no_cat, model_full)

# --- 6. Reduced model ----------------------------------------
# If the factors and sleep_hours contribute nothing, a simpler model
# is easier to explain and just as accurate.
# Edit this formula to drop whatever Step 2 and Step 5 show is not useful.
model_reduced <- lm(resting_bp_systolic ~ age + bmi + cholesterol_total +
                      stress_score + daily_steps + exercise_minutes_per_week,
                    data = heart)

get_regression_table(model_reduced, digits = 3)
get_regression_summaries(model_reduced)

# --- 7. Save results -----------------------------------------
dir.create("outputs/models", showWarnings = FALSE)   # make the folder if missing

get_regression_table(model_full, digits = 3) |>
  write_csv("outputs/tables/multiple_regression_full.csv")

get_regression_table(model_reduced, digits = 3) |>
  write_csv("outputs/tables/multiple_regression_reduced.csv")

# saveRDS() keeps the model objects for the diagnostics step
saveRDS(model_full,    "outputs/models/full.rds")
saveRDS(model_reduced, "outputs/models/reduced.rds")
