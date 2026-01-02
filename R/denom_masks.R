# ------------------------------------------------------------------------------
# Denominator masks
#
# Validation denominators must be defined by explicit masks, not by NA values in
# observed or simulated state.
# ------------------------------------------------------------------------------

make_denom_mask <- function(
  alive_mask,
  followup_defined,
  model_defined = NULL,
  measured = NULL,
  measured_only = FALSE
) {
  .assert(is.matrix(alive_mask), "alive_mask must be a matrix")
  .assert(is.matrix(followup_defined), "followup_defined must be a matrix")
  .assert(all(dim(alive_mask) == dim(followup_defined)), "alive_mask and followup_defined must have same dim")

  # Core invariant from the locked spec.
  # NOTE: do not use %in% here because it drops dimensions for matrices.
  if (any(is.na(alive_mask) & (followup_defined == TRUE), na.rm = TRUE)) {
    stop("Invalid masks: followup_defined must be FALSE whenever alive_mask is NA.", call. = FALSE)
  }

  # Keep matrix shape: use elementwise comparisons, not %in%.
  denom <- (alive_mask == TRUE) & (followup_defined == TRUE)

  if (!is.null(model_defined)) {
    .assert(is.matrix(model_defined), "model_defined must be a matrix when provided")
    .assert(all(dim(model_defined) == dim(denom)), "model_defined must match dims")
    denom <- denom & (model_defined == TRUE)
  }

  if (isTRUE(measured_only)) {
    .assert(!is.null(measured), "measured_only=TRUE requires measured mask")
    .assert(is.matrix(measured), "measured must be a matrix when provided")
    .assert(all(dim(measured) == dim(denom)), "measured must match dims")
    denom <- denom & (measured == TRUE)
  }

  denom
}
