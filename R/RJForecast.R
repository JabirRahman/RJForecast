rj_forecast <- function(data, target_col, xreg_cols = NULL, h = 12, val_h = NULL) {
  library(dplyr)
  library(tsibble)
  library(fable)
  library(fabletools)
  library(rlang)

  # Ensure tsibble
  data_ts <- if (!tsibble::is_tsibble(data)) tsibble::as_tsibble(data) else data
  target_sym <- rlang::sym(target_col)

  # Validation horizon
  if (is.null(val_h)) val_h <- max(2, min(h, floor(nrow(data_ts) / 5)))

  if (nrow(data_ts) <= val_h + 5) {
    stop("Not enough observations to create a validation window.")
  }

  # Frequency check
  idx_var <- tsibble::index_var(data_ts)
  period_val <- tsibble::guess_frequency(data_ts[[idx_var]])

  # Split train / validation
  train_ts <- data_ts %>% slice(1:(n() - val_h))
  valid_ts <- data_ts %>% slice_tail(n = val_h)

  # Model formula
  if (is.null(xreg_cols)) {
    tslm_formula <- stats::as.formula(
      paste(target_col, "~ trend()")
    )
  } else {
    tslm_formula <- stats::as.formula(
      paste(target_col, "~ trend() +", paste(xreg_cols, collapse = " + "))
    )
  }

  # 1. Fit base models on training set
  fit_train <- train_ts %>%
    model(
      Mean   = MEAN(!!target_sym),
      Naive  = NAIVE(!!target_sym),
      SNaive = if (period_val > 1) SNAIVE(!!target_sym) else RW(!!target_sym),
      Drift  = RW(!!target_sym ~ drift()),
      ETS    = ETS(!!target_sym),
      TSLM   = TSLM(!!tslm_formula)
    )

  # 2. Forecast validation period
  fc_valid <- fit_train %>% forecast(h = val_h)

  # 3. Validation accuracy
  acc <- fc_valid %>%
    accuracy(valid_ts) %>%
    as_tibble() %>%
    select(.model, RMSE)

  mse_vals <- acc$RMSE^2
  mse_vals[is.na(mse_vals) | is.infinite(mse_vals)] <- Inf

  weights <- 1 / (mse_vals + 1e-6)
  weights <- weights / sum(weights)

  weight_tbl <- tibble(
    .model = acc$.model,
    weight = weights
  )

  # 4. Refit models on full data
  fit_full <- data_ts %>%
    model(
      Mean   = MEAN(!!target_sym),
      Naive  = NAIVE(!!target_sym),
      SNaive = if (period_val > 1) SNAIVE(!!target_sym) else RW(!!target_sym),
      Drift  = RW(!!target_sym ~ drift()),
      ETS    = ETS(!!target_sym),
      TSLM   = TSLM(!!tslm_formula)
    )

  # 5. Forecast future horizon
  fc_full <- fit_full %>%
    forecast(h = h) %>%
    as_tibble() %>%
    select(.model, !!sym(idx_var), .mean)

  # 6. Weighted ensemble forecast
  hybrid_fc <- fc_full %>%
    left_join(weight_tbl, by = ".model") %>%
    mutate(weighted_mean = .mean * weight) %>%
    group_by(!!sym(idx_var)) %>%
    summarise(.mean = sum(weighted_mean, na.rm = TRUE), .groups = "drop") %>%
    mutate(.model = "RJForecast")

  # 7. Return plain tibble with forecast means
  hybrid_fc
}
