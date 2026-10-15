library(tidyverse)
library(janitor)

heart <- read_csv("data/raw/heart_disease_risk_2026.csv") |>
  clean_names()

#mutate() creates or changes columns. factor() turns text into a categorical variable, 
#which regression needs in order to make dummy variables.

heart <- heart |>
  mutate(
    sex = factor(sex),
    smoker_status = factor(smoker_status, levels = c("Never", "Former", "Current")),
    chest_pain_type = factor(chest_pain_type)
  )

# --- Convert TRUE/FALSE columns to 1/0 -----------------------
heart <- heart |>
  mutate(across(c(exercise_induced_angina, family_history, wearable_owner),
                as.integer))


#Check factor levels -----
# levels() lists the categories; look for typos or duplicates
levels(heart$sex)
levels(heart$smoker_status)
levels(heart$chest_pain_type)

#Logic checks
# Diastolic BP must always be lower than systolic BP
sum(heart$resting_bp_diastolic >= heart$resting_bp_systolic)

heart |>
  mutate(chol_gap = cholesterol_total - (hdl + ldl + triglycerides / 5)) |>
  summarise(mean_gap = mean(chol_gap), sd_gap = sd(chol_gap),
            min_gap = min(chol_gap), max_gap = max(chol_gap))

#This counts ages outside a sensible range
sum(heart$age < 18 | heart$age > 100)

#Save the clean data
write_csv(heart, "data/clean/heart_clean.csv")
saveRDS(heart, "data/clean/heart_clean.rds")
