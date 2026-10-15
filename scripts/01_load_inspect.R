library(tidyverse)
library(janitor)
library(skimr)
library(broom)
library(car)
library(moderndive)

#Load the data
heart <- read_csv("data/raw/heart_disease_risk_2026.csv")


heart <- clean_names(heart)

#Inspect the data
dim(heart)

glimpse(heart)

skim(heart)

#Check for duplicates
sum(duplicated(heart$patient_id))

#Check for NA
colSums(is.na(heart))
