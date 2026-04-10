RJForecast: Adaptive Hybrid Ensemble with Exogenous Support
Developer: Md Jabir Rahman
Affiliation: Meinders School of Business, Oklahoma City University
Overview: RJForecast is an R package designed for automated, frequency-agnostic time-series forecasting. The framework dynamically weights six base models based on their inverse-variance performance in a recent validation window, ensuring the forecast adapts to the specific characteristics of the input data. 
Key Features
Adaptive Weighting: Automatically audits model performance (Mean, Naive, SNaive, Drift, ETS, and TSLM) and weights them by accuracy.
Frequency Agnostic: Utilizes Fourier terms to handle various seasonalities without manual tuning.
Exogenous Support: Easily incorporate external regressors (xregs) to improve predictive power.
Tidy Integration: Built on the fable and tsibble ecosystem for seamless integration into modern data science workflows.

Installation
You can install the development version of RJForecast from GitHub:R# install.packages("remotes")
remotes::install_github("JabirRahman/RJForecast")

Quick StartRlibrary(RJForecast)
library(fpp3)

# Use quarterly beer production data
data <- aus_production %>% select(Quarter, Beer)

# Generate a 2-year (8 quarter) forecast
fc <- rj_forecast(data, target_col = "Beer", h = 8)

# Visualize the hybrid ensemble
library(ggplot2)
autoplot(fc, data) + theme_minimal()

Citation
If you use this framework in your research, please cite:
Rahman, M. J. (2026). RJForecast: An Adaptive Hybrid Ensemble Framework for Transparent AI Forecasting. Meinders School of Business, Oklahoma City University.

License
This project is licensed under the MIT License - see the LICENSE file for details.
