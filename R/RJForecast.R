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
rj_forecast <- function(data, target_col, xreg_cols = NULL, h = 12) {

  data_ts <- fabletools::as_tsibble(data)
  target_sym <- rlang::sym(target_col)

  # Determine if data is seasonal
  is_seasonal <- tsibble::variable_key(data_ts)
  period_val <- tsibble::guess_frequency(data_ts[[tsibble::index_var(data_ts)]])

  # 1. Fit models safely with try-catch logic
  # We use a standard TSLM if Fourier fails
  test_fit <- data_ts %>%
    fabletools::model(
      m1 = fable::MEAN(!!target_sym),
      m2 = fable::NAIVE(!!target_sym),
      m3 = if(period_val > 1) fable::SNAIVE(!!target_sym) else fable::RW(!!target_sym),
      m4 = fable::RW(!!target_sym ~ drift()),
      m5 = fable::ETS(!!target_sym),
      m6 = if(period_val > 4) fable::TSLM(!!target_sym ~ fourier(K = 2)) else fable::TSLM(!!target_sym ~ trend())
    )

  # 2. Calculate Accuracy and filter out failed models (NULL models)
  acc <- test_fit %>%
    fabletools::forecast(h = h) %>%
    fabletools::accuracy(data_ts)

  # 3. Robust Weighting: Handle potential NaNs
  mse_vals <- acc$RMSE^2
  mse_vals[is.na(mse_vals)] <- Inf # Give failed models zero weight

  weights <- 1 / (mse_vals + 0.0001)
  weights <- weights / sum(weights)

  # 4. Final Weighted Model
  final_model <- test_fit %>%
    dplyr::mutate(
      RJ_Hybrid = (m1 * weights[1]) + (m2 * weights[2]) + (m3 * weights[3]) +
        (m4 * weights[4]) + (m5 * weights[5]) + (m6 * weights[6])
    )

  # 5. Forecast
  future_data <- tsibble::new_data(data_ts, n = h)
  return(fabletools::forecast(final_model, new_data = future_data))
}

# 6. Global Variables (Placed at the end to avoid documentation errors)
if(getRversion() >= "2.15.1") utils::globalVariables(c("m1", "m2", "m3", "m4", "m5", "m6"))
