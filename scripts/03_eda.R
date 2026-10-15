# Script 03: Exploratory data analysis (EDA)
# Outcome: resting_bp_systolic

#Load packages 
library(tidyverse)

# readRDS() loads the cleaned data with factor levels intact
heart <- readRDS("data/clean/heart_clean.rds")

# --- Choose the variables for the analysis -------------------
predictors <- c("age", "bmi", "cholesterol_total", "stress_score",
                "daily_steps", "sleep_hours", "exercise_minutes_per_week")

# --- 1. Summarise the outcome --------------------------------
# summarise() collapses the whole table into one row of statistics
# If the mean and median are close, the distribution is roughly symmetric
heart |>
  summarise(n      = n(),                          
            mean   = mean(resting_bp_systolic),
            median = median(resting_bp_systolic),
            sd     = sd(resting_bp_systolic),      
            min    = min(resting_bp_systolic),
            max    = max(resting_bp_systolic))

# --- 2. Histogram of the outcome -----------------------------
# Linear regression works best when the residuals are roughly normal,
# and a roughly normal outcome is a good early sign
p_outcome <- ggplot(heart, aes(x = resting_bp_systolic)) +   
  geom_histogram(binwidth = 5,                               # each bar covers 5 mmHg
                 fill = "steelblue", colour = "white") +     
  geom_vline(aes(xintercept = mean(resting_bp_systolic)),    
             colour = "red", linetype = "dashed") +
  labs(title = "Distribution of resting systolic blood pressure",
       x = "Systolic BP (mmHg)", y = "Number of patients") +
  theme_minimal()                                            # clean background

p_outcome   

# --- 3. Normal Q-Q plot of the outcome -----------------------
# Points sitting on the line mean the data follow a normal distribution
# Curving away at the ends means skewness or heavy tails
p_qq <- ggplot(heart, aes(sample = resting_bp_systolic)) +
  geom_qq(alpha = 0.3) +        
  geom_qq_line(colour = "red") +  
  labs(title = "Normal Q-Q plot: systolic BP",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_minimal()

p_qq

# --- 4. Summary table for the numeric predictors -------------
# pivot_longer() stacks the predictor columns into two columns:
# "variable" (the name) and "value" (the number).
# This lets us summarise every predictor in a single step.
heart_long <- heart |>
  select(all_of(predictors)) |>                 # keep only the predictors
  pivot_longer(cols = everything(),             # stack all selected columns
               names_to = "variable",
               values_to = "value")

heart_long |>
  group_by(variable) |>                         # one row per predictor
  summarise(mean   = round(mean(value), 1),
            median = round(median(value), 1),
            sd     = round(sd(value), 1),
            min    = min(value),
            max    = max(value))

# --- 5. Histograms of all predictors -------------------------
# facet_wrap() makes one panel per variable
# scales = "free" lets each panel use its own x and y range,
# because the variables are on very different scales
p_predictors <- ggplot(heart_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "darkgreen", colour = "white") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distributions of the numeric predictors",
       x = NULL, y = "Count") +
  theme_minimal()

p_predictors

# --- 6. Counts for the categorical variables -----------------
# count() tallies each category; mutate() adds the percentage
heart |>
  count(sex) |>
  mutate(percent = round(100 * n / sum(n), 1))

heart |>
  count(smoker_status) |>
  mutate(percent = round(100 * n / sum(n), 1))

heart |>
  count(chest_pain_type) |>
  mutate(percent = round(100 * n / sum(n), 1))

# --- 7. Save the plots ---------------------------------------
# ggsave() writes a plot to a file; width and height are in inches
ggsave("outputs/plots/outcome_histogram.png", p_outcome, width = 7, height = 5)
ggsave("outputs/plots/outcome_qq.png", p_qq, width = 6, height = 5)
ggsave("outputs/plots/predictor_histograms.png", p_predictors, width = 10, height = 7)
