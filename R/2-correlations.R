
# Prepare workspace -------------------------------------------------------

## Import libraries
library(data.table)
library(correlation)

## Load data
data("swimmers")

# -------------------------------------------------------------------------

cor_results <-
  correlation(swimmers, method = "gaussian", p_adjust = "none") |>
  subset(p < 0.05)

#> Parameter1                 |                 Parameter2 |     r |         95% CI |      t | df |         p
#> ----------------------------------------------------------------------------------------------------------
#> edad                       |                 diastolica |  0.67 | [ 0.44,  0.82] |   5.33 | 35 | < .001***
#> temperatura_timpanica      |                   hrv_sdnn |  0.38 | [ 0.07,  0.63] |   2.50 | 36 | 0.017*
#> temperatura_timpanica      |                    hrv_pns |  0.35 | [ 0.03,  0.60] |   2.21 | 36 | 0.034*
#> temperatura_timpanica      |                    hrv_sns | -0.39 | [-0.63, -0.08] |  -2.53 | 36 | 0.016*
#> temperatura_piel_pecho     |                hrv_mean_rr |  0.43 | [ 0.11,  0.67] |   2.71 | 32 | 0.011*
#> temperatura_piel_pecho     |                   hrv_sdnn |  0.50 | [ 0.19,  0.71] |   3.23 | 32 | 0.003**
#> temperatura_piel_pecho     |                    hrv_pns |  0.41 | [ 0.08,  0.66] |   2.53 | 32 | 0.016*
#> temperatura_piel_pecho     |                    hrv_sns | -0.47 | [-0.70, -0.16] |  -3.01 | 32 | 0.005**
#> temperatura_piel_pecho     |                 hrv_stress | -0.37 | [-0.63, -0.04] |  -2.27 | 32 | 0.030*
#> temperatura_piel_deltoides |                hrv_mean_rr |  0.47 | [ 0.17,  0.69] |   3.13 | 34 | 0.004**
#> temperatura_piel_deltoides |                    hrv_pns |  0.34 | [ 0.02,  0.60] |   2.13 | 34 | 0.041*
#> temperatura_piel_mano      |                hrv_mean_rr |  0.39 | [ 0.08,  0.63] |   2.58 | 37 | 0.014*
#> temperatura_piel_mano      |                   hrv_sdnn |  0.33 | [ 0.02,  0.59] |   2.14 | 37 | 0.039*
#> temperatura_piel_mano      |                    hrv_sns | -0.41 | [-0.64, -0.11] |  -2.73 | 37 | 0.010**


# -------------------------------------------------------------------------

summary(cor_results, redundant = TRUE)
