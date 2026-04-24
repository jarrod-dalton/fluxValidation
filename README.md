# fluxValidation
[![Release](https://img.shields.io/github/v/release/jarrod-dalton/fluxValidation?display_name=tag)](https://github.com/jarrod-dalton/fluxValidation/releases)
[![Downloads](https://img.shields.io/github/downloads/jarrod-dalton/fluxValidation/total)](https://github.com/jarrod-dalton/fluxValidation/releases)
[![License: LGPL-3](https://img.shields.io/badge/license-LGPL--3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Language: R](https://img.shields.io/badge/language-R-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)

Validation utilities for the **flux** ecosystem.

This package focuses on one job: **make observed/test-set entity data plug-compatible with `fluxForecast` outputs** so you can compute validation metrics on an apples-to-apples basis.

## Core ideas

- **Forecast grid is the truth**: validation evaluates only at the `times=` grid used for forecasting.
- **Denominators come from masks, not `NA`**:
  - `alive_mask` (TRUE/FALSE/NA)
  - `followup_defined` (TRUE/FALSE)
  - `model_defined` (TRUE/FALSE) for variable/event in-scope periods
  - `measured` (TRUE/FALSE) for EHR cadence
- **Observed state values are gridded by default with LOCF-within-window** (configurable).

## Installation (development)

```r
# install.packages("remotes")
remotes::install_github("jarrod-dalton/fluxValidation")
```

## Quick start

### 1) Build an observed grid

```r
library(fluxValidation)

# Example: static + longitudinal panels
df_static <- data.frame(
  entity_id = c(1, 2),
  sex = c("F", "M"),
  height_cm = c(165, 175)
)

df_bp <- data.frame(
  entity_id = c(1, 1, 2),
  time = c(0, 6, 3),
  sbp = c(120, 128, 140),
  dbp = c(80,  82,  90)
)

# Optional events table
df_events <- data.frame(
  entity_id = c(1, 2),
  event_time = c(5, 7),
  event_type = c("mi", "mi")
)

obs <- build_obs_grid(
  vars = list(df_static, df_bp),
  events = df_events,
  times = c(0, 3, 6, 9),
  t0 = 0,
  start_time = 0,
  default_window = 3
)
```

### 2) Event risk validation (apples-to-apples with `fluxForecast::event_prob()`)

```r
# pred can be a flux_event_prob object (preferred) or a flux_forecast object
# result <- validate_event_risk(pred, obs, event = "mi", obs_mode = "policy")
```

### 3) State validation

- Use `validate_state_prob()` for binary/categorical/ordinal variables when you have predicted level probabilities.
- Use `validate_state_point()` for numeric variables when you have predicted means (or point summaries).

## License

LGPL-3.
