# ============================================================
# Script 07: Final model and diagnostics
# ============================================================

# --- Load packages 
library(tidyverse)  
library(moderndive)   
library(car)          


heart <- readRDS("data/clean/heart_clean.rds")

# --- 1. Fit the final model ----------------------------------
# Keeps only the predictors that earned their place:
# age, bmi, cholesterol_total, stress_score, exercise_minutes_per_week
model_final <- lm(resting_bp_systolic ~ age + bmi + cholesterol_total +
                    stress_score + exercise_minutes_per_week,
                  data = heart)

get_regression_table(model_final, digits = 3)   # coefficients and 95% CIs
get_regression_summaries(model_final)           # R-squared, RMSE and F test

# --- 2. Get fitted values and residuals ----------------------
# get_regression_points() returns one row per patient with:
#   resting_bp_systolic_hat = predicted BP
#   residual                = observed minus predicted
points <- get_regression_points(model_final)

# --- 3. Linearity and constant variance ----------------------
# Plot residuals against fitted values
# Good: a flat, even band of points around 0
# Bad: a curve (not linear) or a funnel shape (non-constant variance)
p_resid <- ggplot(points, aes(x = resting_bp_systolic_hat, y = residual)) +
  geom_point(alpha = 0.2) +                            # one dot per patient
  geom_hline(yintercept = 0, colour = "red") +         # the zero-error line
  geom_smooth(se = FALSE, colour = "blue") +           # a smooth trend; should be flat
  labs(title = "Residuals versus fitted values",
       x = "Fitted systolic BP (mmHg)", y = "Residual (mmHg)") +
  theme_minimal()

p_resid

# ncvTest() tests for non-constant variance
# A small p-value means the spread changes with the fitted value.
# With 9,000 patients tiny departures give small p-values, so trust the plot more
ncvTest(model_final)

# --- 4. Normality of residuals -------------------------------
# Histogram: should look like a bell centred on 0
p_hist <- ggplot(points, aes(x = residual)) +
  geom_histogram(binwidth = 3, fill = "steelblue", colour = "white") +
  labs(title = "Distribution of residuals",
       x = "Residual (mmHg)", y = "Number of patients") +
  theme_minimal()

# Q-Q plot: points should follow the red line
p_qq <- ggplot(points, aes(sample = residual)) +
  geom_qq(alpha = 0.3) +
  geom_qq_line(colour = "red") +
  labs(title = "Normal Q-Q plot of residuals",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_minimal()

p_hist
p_qq

# --- 5. Residuals against each predictor ---------------------
# A curve in any panel would mean that predictor needs a non-linear term
# pivot_longer() stacks the predictors so one plot can show them all
resid_long <- heart |>
  select(age, bmi, cholesterol_total, stress_score, exercise_minutes_per_week) |>
  mutate(residual = points$residual) |>                # add each patient's residual
  pivot_longer(cols = -residual,                       # stack all columns except residual
               names_to = "variable", values_to = "value")

p_resid_pred <- ggplot(resid_long, aes(x = value, y = residual)) +
  geom_point(alpha = 0.1) +
  geom_smooth(colour = "blue", se = FALSE) +           # should be flat near 0
  geom_hline(yintercept = 0, colour = "red") +
  facet_wrap(~ variable, scales = "free_x") +          # one panel per predictor
  labs(title = "Residuals versus each predictor",
       x = NULL, y = "Residual (mmHg)") +
  theme_minimal()

p_resid_pred

# --- 6. Multicollinearity (VIF) ------------------------------
# VIF measures how much a predictor's variance is inflated by overlap
# with the others. Below about 5 is fine; above 10 is a serious problem.
vif(model_final)

# --- 7. Influential points (Cook's distance) -----------------
# Cook's distance measures how much the fit would change if one patient
# were removed. A common warning level is 4 divided by the sample size.
cooks <- cooks.distance(model_final)
threshold <- 4 / nrow(heart)

sum(cooks > threshold)         # how many patients exceed the warning level
max(cooks)                     # the largest value; above 1 would be a real concern

# Look at the most influential patients
# order() sorts from smallest to largest; decreasing = TRUE reverses it
# head(..., 5) keeps the top five row numbers
top5 <- order(cooks, decreasing = TRUE)[1:5]
heart[top5, c("patient_id", "age", "bmi", "cholesterol_total",
              "stress_score", "exercise_minutes_per_week", "resting_bp_systolic")]

# --- 8. Save everything --------------------------------------
ggsave("outputs/plots/diag_resid_fitted.png", p_resid, width = 7, height = 5)
ggsave("outputs/plots/diag_resid_hist.png", p_hist, width = 6, height = 5)
ggsave("outputs/plots/diag_resid_qq.png", p_qq, width = 6, height = 5)
ggsave("outputs/plots/diag_resid_predictors.png", p_resid_pred, width = 10, height = 7)

get_regression_table(model_final, digits = 3) |>
  write_csv("outputs/tables/final_model.csv")

saveRDS(model_final, "outputs/models/final.rds")
