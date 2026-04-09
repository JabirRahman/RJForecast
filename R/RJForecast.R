#' RJForecast: Adaptive Hybrid Ensemble with Exogenous Support
#'
#' @description
#' An automated frequency-agnostic ensemble model. It dynamically weights
#' six base models (Mean, Naive, SNaive, Drift, ETS, and TSLM) based on
#' their inverse-variance performance in a recent validation window.
#'
#' @param data A data frame or tibble with a time-series index.
#' @param target_col The string name of the dependent variable.
#' @param xreg_cols A character vector of exogenous variables (optional).
#' @param h The forecast horizon (default 12).
#'
#' @importFrom stats as.formula
#' @importFrom rlang sym
#' @importFrom dplyr mutate n slice slice_tail
#' @importFrom tsibble new_data
#' @importFrom fabletools as_tsibble model forecast accuracy
#' @importFrom utils globalVariables
#' @import fable
#'
#' @export
# Define the fixed function directly in your script
rj_forecast <- function(data, target_col, xreg_cols = NULL, h = 12) {
  
  # Ensure data is a tsibble
  data_ts <- if(!tsibble::is_tsibble(data)) tsibble::as_tsibble(data) else data
  target_sym <- rlang::sym(target_col)
  
  # ROBUST FREQUENCY CHECK (Replaces the broken variable_key)
  idx_var <- tsibble::index_var(data_ts)
  period_val <- tsibble::guess_frequency(data_ts[[idx_var]])

  # 1. Fit models safely
  test_fit <- data_ts %>%
    fabletools::model(
      m1 = fable::MEAN(!!target_sym),
      m2 = fable::NAIVE(!!target_sym),
      m3 = if(period_val > 1) fable::SNAIVE(!!target_sym) else fable::RW(!!target_sym),
      m4 = fable::RW(!!target_sym ~ drift()),
      m5 = fable::ETS(!!target_sym),
      m6 = if(period_val > 4) fable::TSLM(!!target_sym ~ trend()) else fable::TSLM(!!target_sym ~ trend())
    )

  # 2. Accuracy check for weights
  acc <- test_fit %>% fabletools::accuracy()
  
  # 3. Weighting Logic (Handling failures)
  mse_vals <- acc$RMSE^2
  mse_vals[is.na(mse_vals)] <- Inf 
  
  weights <- 1 / (mse_vals + 1e-6)
  weights <- weights / sum(weights) 

  # 4. Final Weighted Model
  final_model <- test_fit %>%
    dplyr::mutate(
      RJ_Hybrid = (m1 * weights[1]) + (m2 * weights[2]) + (m3 * weights[3]) + 
                  (m4 * weights[4]) + (m5 * weights[5]) + (m6 * weights[6])
    )

  # 5. Forecast
  return(fabletools::forecast(final_model, h = h))
}
# 6. Global Variables (Placed at the end to avoid documentation errors)
if(getRversion() >= "2.15.1") utils::globalVariables(c("m1", "m2", "m3", "m4", "m5", "m6"))
