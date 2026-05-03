
test_that("build_obs_grid accepts vars-only and errors on duplicate vars", {
  vars <- list(
    data.frame(entity_id = c("p1","p2"), sex = c("F","M"), stringsAsFactors = FALSE),
    data.frame(entity_id = c("p1","p1","p2"),
               time = c(0, 1, 0),
               sbp = c(120, 130, 110),
               stringsAsFactors = FALSE)
  )
  obj <- build_obs_grid(vars = vars, times = c(0,1,2), t0 = 0, default_window = 0)
  expect_true(is_obs_grid(obj))
  expect_equal(length(obj$entity_ids), 2)
  expect_true("sbp" %in% names(obj$vars))
  expect_true(all(dim(obj$measured$sbp) == c(2,3)))

  # With default_window=0, only exact matches are measured.
  expect_equal(unname(obj$measured$sbp["p1", ]), c(TRUE, TRUE, FALSE))
  expect_true(is.na(obj$state$sbp["p1", 3]))

  # LOCF within a window includes the most recent observation in (t_k - w, t_k].
  obj2 <- build_obs_grid(vars = vars, times = c(0,1,2), t0 = 0, default_window = 1)
  expect_equal(unname(obj2$measured$sbp["p1", ]), c(TRUE, TRUE, TRUE))
  expect_equal(unname(obj2$state$sbp["p1", ]), c(120, 130, 130))

  vars_dup <- list(
    data.frame(entity_id = c("p1"), sbp = 120),
    data.frame(entity_id = c("p1"), sbp = 121)
  )
  expect_error(build_obs_grid(vars_dup, times = c(0,1), t0 = 0), "Duplicate variable")
})

test_that("window groups from schema blocks expand to variables", {
  vars <- list(
    data.frame(entity_id = c("p1"), sex = "F", stringsAsFactors = FALSE),
    data.frame(entity_id = c("p1","p1"), time = c(0, 1), sbp = c(120, 130), dbp = c(80, 85), stringsAsFactors = FALSE)
  )
  schema <- list(
    sbp = list(type = "continuous", default = NA_real_, blocks = c("bp")),
    dbp = list(type = "continuous", default = NA_real_, blocks = c("bp"))
  )
  obj <- build_obs_grid(vars, times = c(0,1,2), t0 = 0, schema = schema, window = c(bp = 1))
  expect_equal(unname(obj$state$sbp["p1", ]), c(120, 130, 130))
  expect_equal(unname(obj$state$dbp["p1", ]), c(80, 85, 85))
})

test_that("build_obs_grid requires time_spec for calendar t0", {
  vars <- list(data.frame(entity_id="p1", sex="F"))
  expect_error(build_obs_grid(vars, times=c(0,1), t0=as.Date("2020-01-01")), "time_spec")
  obj <- build_obs_grid(vars, times=c(0,1), t0=as.Date("2020-01-01"), time_spec = fluxCore::time_spec(unit = "days"))
  expect_true(is_obs_grid(obj))
})

test_that("build_obs_grid errors for invalid time_spec input type", {
  vars <- list(
    data.frame(
      entity_id = c("p1"),
      time = as.Date("2020-01-01"),
      sbp = 120,
      stringsAsFactors = FALSE
    )
  )

  expect_error(
    build_obs_grid(
      vars = vars,
      times = c(0, 1),
      t0 = as.Date("2020-01-01"),
      time_spec = list(unit = "days")
    ),
    "time_spec"
  )
})

test_that("events summarization works", {
  vars <- list(data.frame(entity_id=c("p1","p2"), sex=c("F","M")))
  events <- data.frame(
    entity_id=c("p1","p1","p2"),
    event_time=c(0.5, 1.2, 0.7),
    event_type=c("T2DM","T2DM","T2DM"),
    stringsAsFactors = FALSE
  )
  obj <- build_obs_grid(vars, events=events, times=c(0,1,2), t0=0)
  expect_true(!is.null(obj$events))
  any <- obj$events$any[["T2DM"]]
  # Matrix scalar indexing can carry a names attribute via dimnames.
  expect_equal(unname(any["p1",1]), 1L)
  expect_equal(unname(any["p1",2]), 1L)
  expect_equal(unname(any["p2",1]), 1L)
})

test_that("death_date and last_contact_date create alive_mask NA after follow-up stop", {
  vars <- list(
    data.frame(
      entity_id = "p1",
      sex = "F",
      last_contact_date = as.Date("2020-01-02"),
      stringsAsFactors = FALSE
    )
  )
  obj <- build_obs_grid(vars, times = c(0, 1, 2), t0 = as.Date("2020-01-01"), time_spec = fluxCore::time_spec(unit = "days"))
  am <- obj$alive_mask["p1", ]
  fu <- obj$followup_defined["p1", ]
  expect_identical(as.logical(am[1]), TRUE)
  expect_identical(as.logical(am[2]), TRUE)
  expect_true(is.na(am[3]))
  expect_identical(as.logical(fu[1]), TRUE)
  expect_identical(as.logical(fu[2]), TRUE)
  expect_identical(as.logical(fu[3]), FALSE)
})
