## 1.5.0

- Validation adapters and docs fully aligned to event-probability forecast APIs (as_event_prob and related references).

- build_obs_grid() documentation signatures/arguments synchronized with implementation, including schema/window/model_defined/at_risk controls.

- check() polish: namespaced setNames usage and Rd usage-width cleanup.

- Licensing update: switched package license to LGPL-3.

## 1.4.0

- API naming: removed `flux_` prefixes from adapter/check helpers. Public adapters are now `as_event_prob()`, `as_state_prob()`, `as_state_point()`, and `is_obs_grid()`.
- Documentation: added/updated manual `.Rd` pages to match the renamed helpers.
- Packaging hygiene: removed roxygen-style blocks from `R/` and standardized filenames to underscore style.

## 1.3.0

- Coordinated ecosystem release v1.3.0.
- Schema validation and schema helper workflows are consolidated to `fluxCore`.

## 1.2.5

## 1.2.6

- Add LICENSE file to align repository structure with ecosystem standards.

- Tests: align minimal test schemas with fluxCore's authoritative variable type set (e.g., use `type = "continuous"` instead of legacy `"numeric"`).

## 1.2.3

- Tests: schema block expansion test schema now includes required `default` fields for variables, consistent with fluxCore schema validation rules.

## 1.2.2

- Refactor: removed Validation's duplicated schema-handshake helpers and now relies on fluxCore's authoritative schema utilities (`schema_validate`, `schema_var_info`) for schema block expansion.

## 1.2.1

- Backend: build_obs_grid now supports Date/POSIXct calendar times for `t0` and input tables via fluxCore time helpers (`time_spec`, `time_to_model`). Supported units include days, weeks, months (30.4375 days), and years (365.25 days), with time zone handling via `ctx$time$zone`.

## 1.2.0

- Version bump to align with flux ecosystem v1.2.0. No functional changes.

# 1.1.1 (2026-01-06)

- Documentation and formatting cleanup in build_obs_grid (removed legacy version references).

# fluxValidation NEWS

## 1.1.0

- Promoted to v1.1.0 to align with the stable flux ecosystem releases (fluxCore/fluxForecast/fluxASCVD/fluxModelTemplate v1.1.0).
- Validation APIs are now schema- and mask-driven, and support both forecast summary objects (preferred) and full `flux_forecast` inputs via adapters.
- Added vignette rendering helper script under `tools/` to compile `vignettes/*.Rmd` into `docs/*.html`.

## 0.1.2

- Added vignettes explaining observed-data gridding, mask-driven denominators, and "apples-to-apples" event-risk estimands.
- Added lightweight unit tests for forecast-adapter helpers (`as_event_prob()`, `as_state_prob()`, `as_state_point()`).
- Added `NEWS.md`.

## 0.1.1

- Added forecast-to-summary adapters (`as_event_prob()`, `as_state_prob()`, `as_state_point()`) so validation can prefer small prediction objects.
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
