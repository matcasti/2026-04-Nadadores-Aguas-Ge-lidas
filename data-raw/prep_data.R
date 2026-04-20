
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)

## Import data
raw_data <-
  fread(
    input = "data-raw/raw_data.csv",
    select = readLines("data-raw/selected_cols"),
  )

# -------------------------------------------------------------------------

raw_data <- raw_data[redcap_event_name != "evaluacion_cadi_arm_1"]

raw_data[, edad := edad[!is.na(edad)], keyby = record_id]

raw_data[, pre_competencia := pre_competencia[!is.na(pre_competencia)], keyby = record_id]

raw_data[, sexo := sexo[!is.na(sexo)], keyby = record_id]

raw_data[, traje := traje[!is.na(traje)], keyby = record_id]

raw_data[, pre_competencia := factor(
  x = pre_competencia,
  levels = 1:4,
  labels = c("500m", "1000m", "2000m", "4000m"),
  ordered = TRUE
)]

raw_data[, traje := factor(
  x = traje,
  levels = 1:2,
  labels = c("Yes", "No")
)]

raw_data[, record_id := as.factor(x = rleid(record_id))]

raw_data[record_id == 15, sexo := 1]

raw_data[, sexo := factor(sexo, levels = 1:2, labels = c("Male", "Female"))]

raw_data[, time := factor(
  x = redcap_event_name,
  levels = c("pre_competencia_arm_1","post_competencia_arm_1"),
  labels = c("Pre", "Post")
)]

raw_data[, redcap_event_name := NULL]

raw_data[, hrv_pre_pns := trimws(hrv_pre_pns)]
raw_data[hrv_pre_pns == '', hrv_pre_pns := NA_character_]
raw_data[, hrv_pre_pns := as.numeric(hrv_pre_pns)]

# -------------------------------------------------------------------------

remove_ids <- raw_data[, sum(is.na(.SD)), record_id][V1 > 15, record_id]

raw_data <- raw_data[!record_id %in% remove_ids]

rm(remove_ids)

# -------------------------------------------------------------------------

skimr::skim(raw_data)

raw_data[temperatura_piel_frente == 293.0, temperatura_piel_frente := 29.3]
raw_data[hrv_pre_hf == 19.3, hrv_pre_hf := 0.193]

swimmers <- copy(raw_data)

rm(raw_data)

# -------------------------------------------------------------------------

names(swimmers) <- gsub("_pre_", "_", names(swimmers))

save(swimmers, file = "data/swimmers.RData")

