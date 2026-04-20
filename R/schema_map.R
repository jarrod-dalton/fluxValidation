#+#+#+#+-----------------------------------------------------------------------
# Schema helpers
#
# fluxCore defines (and validates) the model schema and provides
# authoritative schema-handshake helpers. Validation should not carry its own
# duplicated schema logic.
#
# This file keeps only the small helper required for expanding window groups
# (schema blocks) into per-variable windows.
#+#+#+#+-----------------------------------------------------------------------

expand_window_groups <- function(schema, window_by_group_or_var) {
  # Helper for Stage 2: expand named windows declared by schema blocks into
  # per-variable windows. Kept internal for now.
  fluxCore::schema_validate(schema)
  .assert(is.numeric(window_by_group_or_var), "window mapping must be numeric")
  nm <- names(window_by_group_or_var)
  .assert(!is.null(nm) && all(nzchar(nm)), "window mapping must be a named numeric vector")

  info <- fluxCore::schema_var_info(schema, names(schema))
  blocks_by_var <- stats::setNames(info$blocks, info$var)

  # Named numeric vector of per-variable windows.
  # IMPORTANT: use [<- / name membership checks when growing named vectors;
  # out[[newname]] would throw "subscript out of bounds".
  out <- stats::setNames(numeric(0), character(0))
  for (name in nm) {
    w <- window_by_group_or_var[[name]]
    if (name %in% names(schema)) {
      out[name] <- w
      next
    }
    # Treat as block name.
    vars_in_block <- names(Filter(function(b) is.character(b) && name %in% b, blocks_by_var))
    if (length(vars_in_block) == 0) {
      stop(sprintf("Window group '%s' matches neither a schema variable nor a schema block.", name), call. = FALSE)
    }
    for (v in vars_in_block) {
      # First assignment wins; var-specific overrides should be applied later.
      if (!(v %in% names(out))) out[v] <- w
    }
  }
  out
}
