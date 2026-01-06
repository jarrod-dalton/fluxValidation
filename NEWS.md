## 1.2.0

- Version bump to align with patientSim ecosystem v1.2.0. No functional changes.

# 1.1.1 (2026-01-06)

- Documentation and formatting cleanup in build_obs_grid (removed legacy version references).

# patientSimValidation NEWS

## 1.1.0

- Promoted to v1.1.0 to align with the stable patientSim ecosystem releases (patientSimCore/patientSimForecast/patientSimASCVD/patientSimModelTemplate v1.1.0).
- Validation APIs are now schema- and mask-driven, and support both forecast summary objects (preferred) and full `ps_forecast` inputs via adapters.
- Added vignette rendering helper script under `tools/` to compile `vignettes/*.Rmd` into `docs/*.html`.

## 0.1.2

- Added vignettes explaining observed-data gridding, mask-driven denominators, and "apples-to-apples" event-risk estimands.
- Added lightweight unit tests for forecast-adapter helpers (`as_ps_risk()`, `as_ps_state_prob()`, `as_ps_state_point()`).
- Added `NEWS.md`.

## 0.1.1

- Added forecast-to-summary adapters (`as_ps_risk()`, `as_ps_state_prob()`, `as_ps_state_point()`) so validation can prefer small prediction objects.
- Expanded validation outputs with basic comparison tables/metrics while keeping denominators mask-driven.

## 0.1.0

- Added initial state validation helpers for probability-valued (categorical/binary/ordinal) and point-valued (continuous) predictions.

## 0.0.9

- Fixed schema block window expansion for LOCF gridding.

## 0.0.8

- Implemented LOCF-within-window gridding as the default observed-data alignment strategy.

## 0.0.7

- Fixed matrix-shape preservation in mask/denominator logic.

## 0.0.6

- Added mask-first denominators and initial observed event-risk estimators.
