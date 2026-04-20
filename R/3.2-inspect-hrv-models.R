
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)
library(datawizard)
library(brms)
library(ggplot2)
library(tidybayes)

## Load custom functions
source("R/_functions.R")

## Load data
data("swimmers")

## Standardized the data for the models
swimmers_std <- standardize(swimmers)

## Load HRV models
time_fit <- readRDS("models/hrv_time_mod_fit.rds")
freq_fit <- readRDS("models/hrv_freq_mod_fit.rds")
comp_fit <- readRDS("models/hrv_comp_mod_fit.rds")

# -------------------------------------------------------------------------

results_time <- summary_model(time_fit, variable = "^b_|rescor_")[pd >= 0.8]

results_freq <- summary_model(freq_fit, variable = "^b_|rescor_")[pd >= 0.8]

results_comp <- summary_model(comp_fit, variable = "^b_|rescor_")[pd >= 0.8]

# -------------------------------------------------------------------------

results <- rbind(results_time, results_freq, results_comp)

results_label <- report_summary(results)

fwrite(
  x = results,
  file = "output/hrv_results.csv"
)

fwrite(
  x = results_label,
  file = "output/hrv_results_labels.csv"
)


