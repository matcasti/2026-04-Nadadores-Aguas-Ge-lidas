
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)
library(datawizard)
library(brms)

## Load data
data("swimmers")

## Standardized the data for the models
swimmers_std <- standardize(swimmers)

# Specify models ----------------------------------------------------------

### HRV Time domain ----
hrv_time_model <-
  bf(
    ## Multivariate response
    mvbind(hrv_mean_rr, hrv_rmssd, hrv_sdnn) | mi() ~

      # Main effects
      (time) +

      # Covariates effects
      (sexo + edad + pre_competencia + traje) +

      # Main x Covariates interaction effects
      (time:sexo +
         time:edad +
         time:pre_competencia +
         time:traje +
         time:traje:pre_competencia) +

      # Random effects
      (1 | record_id)

    # Residual correlation
  ) + set_rescor(TRUE)

### HRV Frequency domain ----
hrv_freq_model <-
bf(
    ## Multivariate response
    mvbind(hrv_vlf, hrv_lf, hrv_hf) | mi() ~

      # Main effects
      (time) +

      # Covariates effects
      (sexo + edad + pre_competencia + traje) +

      # Main x Covariates interaction effects
      (time:sexo +
         time:edad +
         time:pre_competencia +
         time:traje +
         time:traje:pre_competencia) +

      # Random effects
      (1 | record_id)

    ## Residual correlation
  ) + set_rescor(TRUE)

### HRV Composite indices domain ----
hrv_comp_model <-
bf(
    ## Multivariate response
    mvbind(hrv_pns, hrv_sns, hrv_stress) | mi() ~

      # Main effects
      (time) +

      # Covariates effects
      (sexo + edad + pre_competencia + traje) +

      # Main x Covariates interaction effects
      (time:sexo +
         time:edad +
         time:pre_competencia +
         time:traje +
         time:traje:pre_competencia) +

      # Random effects
      (1 | record_id)

    ## Residual correlation
  ) + set_rescor(TRUE)


# Specify model priors ----------------------------------------------------

## Custom prior function
custom_prior <- function(response) {
  c(
    ## N(μ = 0, σ = 3) for standardized effects
    set_prior("normal(0,3)", class = "b", resp = response),

    ## N(μ = 1, σ = 3) ∈ ℝ⁺ for standard deviation of the responses
    set_prior("normal(1,3)", class = "sigma", resp = response, lb = 0),

    ## N(μ = 1, σ = 3) ∈ ℝ⁺ for standard deviation of random coefficients
    set_prior("normal(1,3)", class = "sd", resp = response, lb = 0),

    ## LKJ(1) for residual correlation
    set_prior("lkj(1)", class = "rescor")
  )
}

time_model_prior <- custom_prior(response = c("hrvmeanrr", "hrvrmssd", "hrvsdnn"))
freq_model_prior <- custom_prior(response = c("hrvvlf", "hrvlf", "hrvhf"))
comp_model_prior <- custom_prior(response = c("hrvpns", "hrvsns", "hrvstress"))


# Fit models --------------------------------------------------------------

hrv_time_mod_fit <- brm(
  formula = hrv_time_model,
  data = swimmers_std,
  family = gaussian(),
  prior = time_model_prior,
  iter = 5000, warmup = 2500, chains = 4, cores = 4,
  seed = 1234, file = "models/hrv_time_mod_fit.rds",
  control = list(
    adapt_delta = 0.999,
    max_treedepth = 50
  )
)

hrv_freq_mod_fit <- brm(
  formula = hrv_freq_model,
  data = swimmers_std,
  family = gaussian(),
  prior = freq_model_prior,
  iter = 5000, warmup = 2500, chains = 4, cores = 4,
  seed = 1234, file = "models/hrv_freq_mod_fit.rds",
  control = list(
    adapt_delta = 0.999,
    max_treedepth = 50
  )
)

hrv_comp_mod_fit <- brm(
  formula = hrv_comp_model,
  data = swimmers_std,
  family = gaussian(),
  prior = comp_model_prior,
  iter = 5000, warmup = 2500, chains = 4, cores = 4,
  seed = 1234, file = "models/hrv_comp_mod_fit.rds",
  control = list(
    adapt_delta = 0.999,
    max_treedepth = 50
  )
)
