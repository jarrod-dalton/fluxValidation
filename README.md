# patientSimValidation

**Validation utilities for the patientSim ecosystem.**

`patientSimValidation` provides mask-aware, grid-aligned validation tools for comparing
**simulated outputs** from `patientSimForecast` to **observed / test-set patient data**.
It is designed to work alongside:

- patientSimCore (state, schema, semantics)
- patientSimForecast (summaries and estimands)
- disease models (e.g., patientSimASCVD)
- orchestration layers (multi-model timelines)

This package is intentionally **read-only** with respect to simulation outputs:
it never mutates objects created by `patientSimForecast`.

---

## Design principles

### 1. Apples-to-apples validation
Validation is only meaningful when **observed quantities and simulated estimates are
computed under the same estimand**.

Key implications:
- Validation operates **only on the forecast evaluation grid** (`times=`).
- Event risks can be evaluated under:
  - **policy / fixed-cohort** estimands (default, matches `forecast::risk()`), or
  - **interval risk-set** estimands (optional).
- Denominators are defined by **explicit masks**, not by the presence or absence of data.

---

### 2. Masks define denominators (never NA values)

All validation denominators are functions of explicit masks:

- `alive_mask[k,i]`  
  `TRUE`, `FALSE`, or `NA` (biological death vs follow-up stop)

- `followup_defined[k,i]`  
  Whether observation is possible at time `k`

- `model_defined[v,k,i]`  
  Whether variable `v` is structurally meaningful at time `k`
  (e.g., model active, episode in scope)

- `measured[v,k,i]`  
  Whether the variable was observable / measured in that interval

NA values in state matrices **never** define denominators.

---

### 3. Schema-driven typing
Validation logic branches on **schema types declared in patientSimCore**:
- binary
- categorical
- ordinal
- numeric / integer / count

This allows validation to:
- respect variable levels and ordering
- apply appropriate scoring rules
- avoid ad-hoc coercion

---

### 4. Lightweight summary inputs preferred
Whenever possible, validation should operate on **summary objects** rather than full
simulation outputs:

- `ps_risk` for event probabilities
- probability summaries for categorical states
- point / quantile summaries for conti
