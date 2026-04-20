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

pred_grid <- expand.grid(
  record_id = NA,
  time = unique(swimmers$time),
  sexo = unique(swimmers$sexo),
  pre_competencia = NA,
  traje = NA,
  edad = c(-1:1) * 2
)

time_pred_data <- add_epred_draws(
  newdata = pred_grid,
  object = time_fit,
  allow_new_levels = TRUE
) |> as.data.table()

freq_pred_data <- add_epred_draws(
  newdata = pred_grid,
  object = freq_fit,
  allow_new_levels = TRUE
) |> as.data.table()

comp_pred_data <- add_epred_draws(
  newdata = pred_grid,
  object = comp_fit,
  allow_new_levels = TRUE
) |> as.data.table()

pred_data <- rbind(
  time_pred_data,
  freq_pred_data,
  comp_pred_data
)

pred_data[, `:=`(
  edad = as.ordered(edad),
  pre_competencia = NULL,
  traje = NULL,
  record_id = NULL,
  .chain = NULL,
  .iteration = NULL
)]

levels(pred_data$.category) <-
  c("Mean R-R", "RMSSD", "SDNN",
    "VLF", "LF", "HF",
    "PNS index", "SNS index", "Stress index")

# -------------------------------------------------------------------------

fig_1 <-
  ggplot(pred_data, aes(time, .epred)) +
  facet_grid(cols = vars(.category), rows = vars(sexo),
             scales = "free_y") +
  stat_summary(aes(group = edad, fill = edad), fun.data = median_hdi,
               position = position_dodge2(0.5),
               geom = "ribbon", alpha = 0.1) +
  stat_summary(aes(group = edad, col = edad), fun.data = median_hdi,
               position = position_dodge2(0.5),
               geom = "pointrange") +
  stat_summary(aes(group = edad, col = edad), fun = median,
               position = position_dodge2(0.5),
               geom = "line", linetype = 1, linewidth = 1/3) +
  ggsci::scale_color_observable(aesthetics = c("fill", "color")) +
  labs(x = "Timepoint", y = "Standardized units",
       color = "Age (z-score)", fill = "Age (z-score)") +
  theme_classic() +
  theme(legend.position = "top")

ggsave(filename = "output/fig_1.png", plot = fig_1,
       device = "png", scale = 7, width = 700, height = 500,
       units = "px", dpi = 400)
