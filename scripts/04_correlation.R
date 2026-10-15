# Script 04: Correlations and relationships with the outcome
# Outcome: resting_bp_systolic
# Package: moderndive (get_correlation)

# --- Load packages and clean data ----------------------------
library(tidyverse)    # dplyr, ggplot2, purrr, readr
library(moderndive)   # get_correlation() and, later, get_regression_table()

#Load data
heart <- readRDS("data/clean/heart_clean.rds")

# --- Variables for the analysis ------------------------------
predictors <- c("age", "bmi", "cholesterol_total", "stress_score",
                "daily_steps", "sleep_hours", "exercise_minutes_per_week")

# --- 1. Correlation of each predictor with the outcome -------
# map_dbl() runs a function once per predictor and returns a numeric vector.
cor_results <- tibble(
  variable = predictors,                       # first column: predictor names
  correlation = map_dbl(predictors, function(x) {
    heart |>
      get_correlation(formula = as.formula(paste("resting_bp_systolic ~", x))) |>
      pull(cor)                                # pull() extracts the "cor" value as a number
  })
) |>
  mutate(correlation = round(correlation, 3), 
         strength = abs(correlation)) |>       
  arrange(desc(strength))                      

cor_results

# --- 2. Check predictors against each other ------------------
heart |>
  select(all_of(predictors)) |>
  cor() |>
  round(2)

# --- 3. Scatterplots of outcome vs each predictor ------------
# pivot_longer() stacks the predictors into "variable" and "value"
# columns, keeping the outcome fixed, so each row pairs one predictor
# value with that patient's blood pressure
scatter_data <- heart |>
  select(resting_bp_systolic, all_of(predictors)) |>
  pivot_longer(cols = -resting_bp_systolic,    # every column except the outcome
               names_to = "variable",
               values_to = "value")

p_scatter <- ggplot(scatter_data, aes(x = value, y = resting_bp_systolic)) +
  geom_point(alpha = 0.15, size = 0.8) +            # see-through points reveal density
  geom_smooth(method = "lm", colour = "red") +      # fitted straight line with confidence band
  facet_wrap(~ variable, scales = "free_x") +       # one panel per predictor, own x range
  labs(title = "Systolic BP versus each predictor",
       x = NULL, y = "Systolic BP (mmHg)") +
  theme_minimal()

p_scatter

# --- 4. Outcome by category (boxplots) -----------------------
# A boxplot shows the median (middle line), the middle 50% (the box),
# and outliers (dots) for each group
p_sex <- ggplot(heart, aes(x = sex, y = resting_bp_systolic)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Systolic BP by sex", x = NULL, y = "Systolic BP (mmHg)") +
  theme_minimal()

p_smoker <- ggplot(heart, aes(x = smoker_status, y = resting_bp_systolic)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Systolic BP by smoking status", x = NULL, y = "Systolic BP (mmHg)") +
  theme_minimal()

p_chest <- ggplot(heart, aes(x = chest_pain_type, y = resting_bp_systolic)) +
  geom_boxplot(fill = "lightyellow") +
  labs(title = "Systolic BP by chest pain type", x = NULL, y = "Systolic BP (mmHg)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))   # tilt labels so they fit

p_sex
p_smoker
p_chest

# --- 5. Group means for the categories -----------------------
# group_by() splits the data by category; summarise() computes
# the count and mean blood pressure within each group
heart |> group_by(sex) |>
  summarise(n = n(), mean_sbp = round(mean(resting_bp_systolic), 1))

heart |> group_by(smoker_status) |>
  summarise(n = n(), mean_sbp = round(mean(resting_bp_systolic), 1))

heart |> group_by(chest_pain_type) |>
  summarise(n = n(), mean_sbp = round(mean(resting_bp_systolic), 1))

# --- 6. Save plots and tables --------------------------------
# ggsave() writes a plot to a file; width and height are in inches
ggsave("outputs/plots/scatter_outcome_predictors.png", p_scatter, width = 10, height = 7)
ggsave("outputs/plots/box_sex.png", p_sex, width = 5, height = 5)
ggsave("outputs/plots/box_smoker.png", p_smoker, width = 5, height = 5)
ggsave("outputs/plots/box_chest_pain.png", p_chest, width = 6, height = 5)

# write_csv() saves the correlation results as a CSV
write_csv(cor_results, "outputs/tables/correlations_with_outcome.csv")
