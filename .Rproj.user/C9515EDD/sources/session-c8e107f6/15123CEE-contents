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

  # 1. Initialization: Convert to tsibble & handle symbols
  data_ts <- fabletools::as_tsibble(data)
  target_sym <- rlang::sym(target_col)

  # 2. Dynamic Regression Formula
  if(!is.null(xreg_cols)) {
    reg_formula <- stats::as.formula(
      paste(target_col, "~ fourier(K = 2) +", paste(xreg_cols, collapse = " + "))
    )
  } else {
    reg_formula <- stats::as.formula(paste(target_col, "~ fourier(K = 2)"))
  }

  # 3. Dynamic Weighting Calculation
  n_rows <- nrow(data_ts)
  split_point <- floor(n_rows * 0.8)
  train_slice <- data_ts[1:split_point, ]
  test_slice  <- data_ts[(split_point + 1):n_rows, ]

  test_fit <- train_slice %>%
    fabletools::model(
      m1 = fable::MEAN(!!target_sym),
      m2 = fable::NAIVE(!!target_sym),
      m3 = fable::SNAIVE(!!target_sym),
      m4 = fable::RW(!!target_sym ~ drift()),
      m5 = fable::ETS(!!target_sym),
      m6 = fable::TSLM(reg_formula)
    )

  acc <- test_fit %>%
    fabletools::forecast(h = nrow(test_slice)) %>%
    fabletools::accuracy(data_ts)

  weights <- 1 / (acc$RMSE^2 + 0.0001)
  weights <- weights / sum(weights)

  # 4. Final Model on Full Dataset
  final_model <- data_ts %>%
    fabletools::model(
      m1 = fable::MEAN(!!target_sym),
      m2 = fable::NAIVE(!!target_sym),
      m3 = fable::SNAIVE(!!target_sym),
      m4 = fable::RW(!!target_sym ~ drift()),
      m5 = fable::ETS(!!target_sym),
      m6 = fable::TSLM(reg_formula)
    ) %>%
    dplyr::mutate(
      RJ_Hybrid = (m1 * weights[1]) + (m2 * weights[2]) + (m3 * weights[3]) +
        (m4 * weights[4]) + (m5 * weights[5]) + (m6 * weights[6])
    )

  # 5. Handle Future Scenarios
  future_data <- tsibble::new_data(data_ts, n = h)

  if (!is.null(xreg_cols)) {
    for (col in xreg_cols) {
      future_data[[col]] <- base::mean(data_ts[[col]], na.rm = TRUE)
    }
  }

  return(fabletools::forecast(final_model, new_data = future_data))
}

# 6. Global Variables (Placed at the end to avoid documentation errors)
if(getRversion() >= "2.15.1") utils::globalVariables(c("m1", "m2", "m3", "m4", "m5", "m6"))
