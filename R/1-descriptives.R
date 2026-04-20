
# Prepare workspace -------------------------------------------------------

## Import libraries
library(data.table)
library(gtsummary)

## Load data
data("swimmers")

# -------------------------------------------------------------------------

tbl_data <- swimmers[time == "Pre", -c("record_id","time")]

names(tbl_data) <-
  c("Age (years old)", "Sex", "Distance (m)", "Swim suit", "Tympanic temperature (Cº)",
    "Forehead temperature (Cº)", "Chest temperature (Cº)", "Shoulder temperature (Cº)",
    "Hand temperature (Cº)", "Mean R-R (ms)", "RMSSD (ms)", "SDNN (ms)",
    "HF (ms²)", "LF (ms²)", "VLF (ms²)", "PNS index", "SNS index", "Stress index",
    "Systolic blood pressure (mmHg)", "Diastolic blood pressure (mmHg)", "SDMT correct answers")

tbl_descriptives <- tbl_summary(
  data = tbl_data,
  by = Sex,
  missing = "no",
  statistic = all_continuous() ~ "{mean} ± {sd}",
  digits = all_continuous() ~ 1,
  type = list(
    `Swim suit` ~ "categorical"
  ),
  include = !c("SDMT correct answers")
) |>
  add_overall() |>
  add_difference(
    test = all_continuous() ~ "smd",
    include = !c("Swim suit", "Distance (m)")
  )

saveRDS(object = tbl_descriptives,
        file = "output/tbl_descriptives.rds")
