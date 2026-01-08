
# Hand-written docs (no roxygen).

build_obs_grid <- function(
  vars,
  events = NULL,
  times,
  t0,
  start_time = 0,
  ctx = NULL,
  time_unit = NULL,
  schema = NULL,
  default_window = 0,
  window = NULL,
  id_col = "patient_id",
  time_col = "time",
  event_time_col = "event_time",
  event_type_col = "event_type",
  measure_lookback = 0,
  model_defined = NULL,
  at_risk = NULL
)
{
  .assert(is.list(vars) && length(vars) >= 1, "`vars` must be a non-empty list of data.frames.")
  times <- as.numeric(times)
  if (any(!is.finite(times))) stop("`times` must be finite numeric.", call. = FALSE)
  if (is.unsorted(times, strictly = FALSE)) stop("`times` must be non-decreasing.", call. = FALSE)
  if (!is.numeric(start_time) || length(start_time) != 1L) stop("`start_time` must be scalar numeric.", call. = FALSE)

  # Compile time mapping spec once (fast path for repeated conversions).
  # If calendar times are used (Date/POSIXct), you must supply either:
  #   - ctx with ctx$time$unit (and optional ctx$time$origin/zone), or
  #   - time_unit (legacy arg) which will be wrapped into ctx$time$unit.
  time_spec <- NULL
  if (!is.null(ctx)) {
    time_spec <- patientSimCore::ps_time_spec(ctx)
  } else if (!is.null(time_unit) && nzchar(time_unit)) {
    time_spec <- patientSimCore::ps_time_spec(list(time = list(unit = time_unit)))
  }

  parsed <- .parse_vars_tables(vars, id_col = id_col, time_col = time_col, time_spec = time_spec)
  patient_ids <- parsed$patient_ids
  if (length(patient_ids) == 0) stop("No patients found in `vars`.", call. = FALSE)

  t0_vec <- .compute_t0(patient_ids, parsed, t0 = t0)

  grid_kind <- .time_kind(t0_vec[[1]])
  if (grid_kind %in% c("Date", "POSIXct")) {
    if (is.null(time_spec)) {
      stop("For Date/POSIXct `t0`, supply `ctx` with ctx$time$unit (and optional ctx$time$origin/zone) or provide `time_unit`.", call. = FALSE)
    }
  }

  grid_time <- .build_grid_time(t0_vec, times = times, start_time = start_time, time_spec = time_spec)
  # Stage 2: LOCF-within-window default for gridding observed state.
  # Backward compat: measure_lookback is retained but overridden by default_window/window.
  if (!is.null(window) || !missing(default_window)) {
    if (!is.numeric(default_window) || length(default_window) != 1L) {
      stop("`default_window` must be scalar numeric.", call. = FALSE)
    }
  }

  lookback_by_var <- .resolve_lookback(
    schema = schema,
    vars = names(parsed$vars),
    default_window = if (!missing(default_window)) default_window else measure_lookback,
    window = window
  )


  grid_state <- .grid_state_locf(parsed, grid_time, times, lookback_by_var = lookback_by_var)
  measured <- grid_state$measured

  masks <- .compute_basic_followup_masks(parsed, grid_time)

  ev <- NULL
  if (!is.null(events)) {
    ev <- .summarize_events(events, patient_ids, grid_time,
                            event_time_col = event_time_col,
                            event_type_col = event_type_col,
                            id_col = id_col,
                            time_spec = time_spec)
  }

  
# model_defined: optional support masks for variables over the grid.
# Can be:
# - NULL (default all TRUE),
# - a named list of logical matrices [n_patients x n_times] keyed by variable name.
model_defined_out <- model_defined
if (is.null(model_defined_out)) {
  model_defined_out <- lapply(names(parsed$vars), function(v) {
    matrix(TRUE, nrow = length(patient_ids), ncol = length(times),
           dimnames = list(patient_ids, as.character(times)))
  })
  names(model_defined_out) <- names(parsed$vars)
}

# at_risk: optional risk-set masks for events over intervals.
# Can be:
# - NULL (default all TRUE),
# - a named list of logical matrices [n_patients x (n_times-1)] keyed by event type.
at_risk_out <- at_risk
if (is.null(at_risk_out) && !is.null(ev)) {
  at_risk_out <- lapply(ev$event_types, function(et) {
    matrix(TRUE, nrow = length(patient_ids), ncol = max(0, length(times) - 1L),
           dimnames = list(patient_ids, paste0(as.character(times[-length(times)]), "->", as.character(times[-1L]))))
  })
  names(at_risk_out) <- ev$event_types
}
obj <- list(
    patient_ids = patient_ids,
    times = times,
    start_time = start_time,
    time_unit = if (!is.null(time_spec)) time_spec$unit else time_unit,
    t0 = t0_vec,
    grid_time = grid_time,
    # vars: sparse per-patient series (raw input after parsing)
    vars = parsed$vars,
    # state: gridded values on the forecast grid (LOCF-within-window)
    state = grid_state$state,
    measured = measured,
    alive_mask = masks$alive_mask,
    followup_defined = masks$followup_defined,
    events = ev,
    model_defined = model_defined_out,
    at_risk = at_risk_out
  )
  class(obj) <- "ps_obs_grid"
  obj
}

is_ps_obs_grid <- function(x) inherits(x, "ps_obs_grid")

print.ps_obs_grid <- function(x, ...) {
  cat("<ps_obs_grid>\n")
  cat("  Patients:", length(x$patient_ids), "\n")
  cat("  Times:", length(x$times), "offsets\n")
  cat("  Variables:", length(x$vars), "\n")
  if (!is.null(x$events)) cat("  Event types:", length(x$events$event_types), "\n")
  invisible(x)
}

.assert <- function(cond, msg) { if (!isTRUE(cond)) stop(msg, call. = FALSE) }

.time_kind <- function(t) {
  if (inherits(t, "Date")) return("Date")
  if (inherits(t, "POSIXct")) return("POSIXct")
  if (is.numeric(t)) return("numeric")
  stop("Unsupported t0 type. Use numeric, Date, or POSIXct.", call. = FALSE)
}

.parse_vars_tables <- function(vars, id_col, time_col, time_spec) {
  patient_ids <- character()
  var_names_seen <- character()
  out <- list()

  for (idx in seq_along(vars)) {
    df <- vars[[idx]]
    .assert(is.data.frame(df), sprintf("vars[[%d]] is not a data.frame.", idx))
    .assert(id_col %in% names(df), sprintf("vars[[%d]] must contain '%s'.", idx, id_col))

    ids <- as.character(df[[id_col]])
    patient_ids <- unique(c(patient_ids, ids))

    has_time <- time_col %in% names(df)
    value_cols <- setdiff(names(df), c(id_col, if (has_time) time_col else character()))
    if (length(value_cols) == 0) next

    dups <- intersect(var_names_seen, value_cols)
    if (length(dups) > 0) {
      stop(sprintf("Duplicate variable(s) across vars tables: %s", paste(dups, collapse = ", ")), call. = FALSE)
    }
    var_names_seen <- c(var_names_seen, value_cols)

    if (!has_time) {
      for (v in value_cols) {
        if (is.null(out[[v]])) out[[v]] <- list()
        for (i in seq_len(nrow(df))) {
          pid <- as.character(df[[id_col]][i])
          out[[v]][[pid]] <- list(time = NULL, value = df[[v]][i])
        }
      }
    } else {
      tt <- df[[time_col]]
      # Convert calendar time to numeric model time if needed.
      if (is.null(time_spec)) {
        kind_tt <- .time_kind(tt[which(!is.na(tt))[1]])
        if (kind_tt %in% c("Date", "POSIXct")) {
          stop("Calendar times in `vars` require a time spec. Supply `ctx` with ctx$time$unit (and optional ctx$time$origin/zone) or provide `time_unit`.", call. = FALSE)
        }
      } else {
        tt <- patientSimCore::ps_time_to_model(tt, time_spec)
      }
      tt <- as.numeric(tt)
      for (v in value_cols) {
        if (is.null(out[[v]])) out[[v]] <- list()
        for (pid in unique(ids)) {
          rows <- which(ids == pid)
          times_pid <- tt[rows]
          vals_pid <- df[[v]][rows]
          keep <- !is.na(times_pid) & !is.na(vals_pid)
          times_pid <- times_pid[keep]
          vals_pid <- vals_pid[keep]
          if (length(times_pid) == 0) next
          o <- order(times_pid)
          out[[v]][[pid]] <- list(time = times_pid[o], value = vals_pid[o])
        }
      }
    }
  }

  list(patient_ids = sort(unique(patient_ids)), vars = out)
}

.compute_t0 <- function(patient_ids, parsed, t0) {
  if (is.function(t0)) {
    res <- list()
    for (pid in patient_ids) {
      val <- t0(pid = pid, parsed = parsed)
      res[[pid]] <- val
    }
    return(res)
  }
  .assert(length(t0) == 1, "`t0` must be a scalar or a function.")
  res <- setNames(vector("list", length(patient_ids)), patient_ids)
  for (pid in patient_ids) res[[pid]] <- t0
  res
}

.build_grid_time <- function(t0_vec, times, start_time, time_spec) {
  offsets <- start_time + times
  res <- list()
  for (pid in names(t0_vec)) {
    t0 <- t0_vec[[pid]]
    kind <- .time_kind(t0)
    if (kind != "numeric" && is.null(time_spec)) {
      stop("Calendar times require a time spec. Supply `ctx` with ctx$time$unit (and optional ctx$time$origin/zone) or provide `time_unit`.", call. = FALSE)
    }
    t0_num <- if (kind == "numeric") {
      as.numeric(t0)
    } else {
      patientSimCore::ps_time_to_model(t0, time_spec)
    }
    .assert(is.numeric(t0_num) && length(t0_num) == 1L && is.finite(t0_num),
            "t0 must resolve to a finite scalar numeric time.")
    res[[pid]] <- t0_num + offsets
  }
  res
}

.resolve_lookback <- function(schema, vars, default_window, window) {
  .assert(is.character(vars) && length(vars) >= 1L, "vars must be a non-empty character vector")
  .assert(is.numeric(default_window) && length(default_window) == 1L && is.finite(default_window),
          "default_window must be a finite scalar numeric")
  out <- setNames(rep(default_window, length(vars)), vars)

  if (!is.null(window)) {
    .assert(is.numeric(window), "window must be numeric when provided")
    nm <- names(window)
    .assert(!is.null(nm) && all(nzchar(nm)), "window must be a named numeric vector")

    expanded <- NULL
    if (!is.null(schema)) {
      expanded <- expand_window_groups(schema, window)
    } else {
      # No schema: treat names as variable names only.
      expanded <- window
    }

    # Apply group-derived windows first (only for known vars).
    for (n in names(expanded)) {
      if (n %in% vars) {
        out[[n]] <- expanded[[n]]
      }
    }
    # Then apply explicit variable overrides (original window entries that match vars).
    for (n in names(window)) {
      if (n %in% vars) {
        out[[n]] <- window[[n]]
      }
    }
  }
  out
}

.grid_state_locf <- function(parsed, grid_time, times, lookback_by_var) {
  patient_ids <- names(grid_time)
  n <- length(patient_ids)
  m <- length(times)
  state <- list()
  measured <- list()

  # Note: lookback_by_var must be expressed in the same units as grid_time for
  # interval subtraction: numeric offsets for numeric grids, days for Date grids,
  # and seconds for POSIXct grids. build_obs_grid() handles unit conversion.

  for (v in names(parsed$vars)) {
    lb <- lookback_by_var[[v]]
    mat <- matrix(NA, nrow = n, ncol = m, dimnames = list(patient_ids, as.character(times)))
    meas <- matrix(FALSE, nrow = n, ncol = m, dimnames = list(patient_ids, as.character(times)))
    for (i in seq_along(patient_ids)) {
      pid <- patient_ids[[i]]
      series <- parsed$vars[[v]][[pid]]
      if (is.null(series)) next
      if (is.null(series$time)) {
        if (!is.na(series$value)) {
          mat[i, ] <- series$value
          meas[i, ] <- TRUE
        }
        next
      }
      tt <- series$time
      vv <- series$value
      if (length(tt) == 0) next
      gt <- grid_time[[pid]]
      for (k in seq_len(m)) {
        t_k <- gt[[k]]
        if (lb == 0) {
          idx <- which(tt == t_k)
          if (length(idx) > 0) {
            j <- idx[[length(idx)]]
            mat[i, k] <- vv[[j]]
            meas[i, k] <- TRUE
          }
        } else {
          idx <- which(tt <= t_k & tt >= (t_k - lb))
          if (length(idx) > 0) {
            j <- idx[[length(idx)]]
            mat[i, k] <- vv[[j]]
            meas[i, k] <- TRUE
          }
        }
      }
    }
    state[[v]] <- mat
    measured[[v]] <- meas
  }

  list(state = state, measured = measured)
}

.compute_measured <- function(parsed, grid_time, times, lookback) {
  patient_ids <- names(grid_time)
  n <- length(patient_ids)
  m <- length(times)
  out <- list()

  for (v in names(parsed$vars)) {
    mat <- matrix(FALSE, nrow = n, ncol = m, dimnames = list(patient_ids, NULL))
    for (i in seq_along(patient_ids)) {
      pid <- patient_ids[i]
      series <- parsed$vars[[v]][[pid]]
      if (is.null(series)) next
      if (is.null(series$time)) {
        mat[i, ] <- !is.na(series$value)
        next
      }
      tt <- series$time
      if (length(tt) == 0) next
      gt <- grid_time[[pid]]
      for (k in seq_len(m)) {
        t_k <- gt[k]
        if (lookback == 0) {
          mat[i, k] <- any(tt == t_k)
        } else {
          mat[i, k] <- any(tt <= t_k & tt >= (t_k - lookback))
        }
      }
    }
    out[[v]] <- mat
  }
  out
}

.compute_basic_followup_masks <- function(parsed, grid_time) {
  patient_ids <- names(grid_time)
  n <- length(patient_ids)
  m <- length(grid_time[[patient_ids[1]]])
  # alive_mask: TRUE/FALSE/NA (NA = vital status unknown after follow-up stop)
  alive <- matrix(TRUE, nrow = n, ncol = m, dimnames = list(patient_ids, NULL))
  followup <- matrix(TRUE, nrow = n, ncol = m, dimnames = list(patient_ids, NULL))

  death_var <- if ("death_date" %in% names(parsed$vars)) "death_date" else NULL
  last_contact_var <- if ("last_contact_date" %in% names(parsed$vars)) "last_contact_date" else NULL

  for (i in seq_along(patient_ids)) {
    pid <- patient_ids[i]
    gt <- grid_time[[pid]]

    death_date <- NULL
    if (!is.null(death_var) && !is.null(parsed$vars[[death_var]][[pid]]) && is.null(parsed$vars[[death_var]][[pid]]$time)) {
      death_date <- parsed$vars[[death_var]][[pid]]$value
    }
    last_contact_date <- NULL
    if (!is.null(last_contact_var) && !is.null(parsed$vars[[last_contact_var]][[pid]]) && is.null(parsed$vars[[last_contact_var]][[pid]]$time)) {
      last_contact_date <- parsed$vars[[last_contact_var]][[pid]]$value
    }

    # follow-up ends at the earliest of last_contact_date and death_date, when known.
    followup_end <- NULL
    if (!is.null(last_contact_date) && !is.na(last_contact_date)) followup_end <- last_contact_date
    if (!is.null(death_date) && !is.na(death_date)) {
      if (is.null(followup_end) || death_date < followup_end) followup_end <- death_date
    }
    if (!is.null(followup_end)) {
      followup[i, ] <- gt <= followup_end
    }

    # Alive semantics:
    # - If death_date known: alive becomes FALSE at/after death_date.
    # - If death_date unknown: alive is TRUE within follow-up; NA after follow-up end.
    if (!is.null(death_date) && !is.na(death_date)) {
      alive[i, ] <- ifelse(gt >= death_date, FALSE, TRUE)
    } else if (!is.null(followup_end)) {
      alive[i, ] <- ifelse(gt <= followup_end, TRUE, NA)
      # Spec: followup_defined must be FALSE whenever alive_mask is NA
      followup[i, is.na(alive[i, ])] <- FALSE
    }
  }

  list(alive_mask = alive, followup_defined = followup)
}

.summarize_events <- function(events, patient_ids, grid_time, event_time_col, event_type_col, id_col, time_spec) {
  .assert(is.data.frame(events), "`events` must be a data.frame.")
  .assert(all(c(id_col, event_time_col, event_type_col) %in% names(events)),
          sprintf("`events` must contain %s, %s, %s.", id_col, event_time_col, event_type_col))
  events[[id_col]] <- as.character(events[[id_col]])
  ev_types <- sort(unique(as.character(events[[event_type_col]])))
  n <- length(patient_ids)
  m <- length(grid_time[[patient_ids[1]]])

  any_list <- list()
  count_list <- list()

  for (et in ev_types) {
    any_mat <- matrix(0L, nrow = n, ncol = max(m - 1, 1), dimnames = list(patient_ids, NULL))
    count_mat <- matrix(0L, nrow = n, ncol = max(m - 1, 1), dimnames = list(patient_ids, NULL))
    sub <- events[as.character(events[[event_type_col]]) == et, , drop = FALSE]
    for (i in seq_along(patient_ids)) {
      pid <- patient_ids[i]
      gt <- grid_time[[pid]]
      tt <- sub[sub[[id_col]] == pid, event_time_col]
      if (length(tt) == 0) next
      # Convert calendar time to numeric model time if needed.
      if (is.null(time_spec)) {
        kind_tt <- .time_kind(tt[which(!is.na(tt))[1]])
        if (kind_tt %in% c("Date", "POSIXct")) {
          stop("Calendar times in `events` require a time spec. Supply `ctx` with ctx$time$unit (and optional ctx$time$origin/zone) or provide `time_unit`.", call. = FALSE)
        }
      } else {
        tt <- patientSimCore::ps_time_to_model(tt, time_spec)
      }
      tt <- as.numeric(tt)
      for (k in seq_len(m - 1)) {
        lo <- gt[k]; hi <- gt[k + 1]
        in_int <- (tt > lo) & (tt <= hi)
        c <- sum(in_int, na.rm = TRUE)
        count_mat[i, k] <- as.integer(c)
        any_mat[i, k] <- as.integer(c > 0)
      }
    }
    any_list[[et]] <- any_mat
    count_list[[et]] <- count_mat
  }

  list(event_types = ev_types, any = any_list, count = count_list)
}