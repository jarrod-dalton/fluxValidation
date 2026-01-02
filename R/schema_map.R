# ------------------------------------------------------------------------------
# Schema helpers
#
# patientSimCore defines a schema as a named list. Each entry must include a
# `type` field and may include `levels` and `blocks`.
#
# Validation uses schema typing to select gridding/scoring strategies.
# ------------------------------------------------------------------------------

schema_var_info <- function(schema, vars = NULL) {
  .assert(is.list(schema) && length(schema) > 0, "schema must be a non-empty list")
  nms <- names(schema)
  .assert(!is.null(nms) && all(nzchar(nms)), "schema must be a named list")

  if (!is.null(vars)) {
    .assert(is.character(vars), "vars must be character when provided")
    missing <- setdiff(vars, nms)
    if (length(missing) > 0) {
      stop(sprintf("Unknown variable(s) not present in schema: %s", paste(missing, collapse = ", ")), call. = FALSE)
    }
    nms <- vars
  }

  type <- setNames(character(length(nms)), nms)
  levels <- setNames(vector("list", length(nms)), nms)
  blocks <- setNames(vector("list", length(nms)), nms)

  for (v in nms) {
    entry <- schema[[v]]
    .assert(is.list(entry), sprintf("schema[[%s]] must be a list", v))
    .assert(!is.null(entry$type) && is.character(entry$type) && length(entry$type) == 1L,
            sprintf("schema[[%s]] must define a scalar 'type'", v))
    type[[v]] <- entry$type
    if (!is.null(entry$levels)) levels[[v]] <- entry$levels
    if (!is.null(entry$blocks)) blocks[[v]] <- entry$blocks
  }

  list(type = type, levels = levels, blocks = blocks)
}

expand_window_groups <- function(schema, window_by_group_or_var) {
  # Helper for Stage 2: expand named windows declared by schema blocks into
  # per-variable windows. Kept internal for now.
  .assert(is.list(schema), "schema must be a list")
  .assert(is.numeric(window_by_group_or_var), "window mapping must be numeric")
  nm <- names(window_by_group_or_var)
  .assert(!is.null(nm) && all(nzchar(nm)), "window mapping must be a named numeric vector")

  info <- schema_var_info(schema)

  # Named numeric vector of per-variable windows.
  # IMPORTANT: use [<- / name membership checks when growing named vectors;
  # out[[newname]] would throw "subscript out of bounds".
  out <- setNames(numeric(0), character(0))
  for (name in nm) {
    w <- window_by_group_or_var[[name]]
    if (name %in% names(schema)) {
      out[name] <- w
      next
    }
    # Treat as block name.
    vars_in_block <- names(Filter(function(b) is.character(b) && name %in% b, info$blocks))
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
