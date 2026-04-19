test_that("build_obs_grid supports Date t0 and Date state times using ctx$time units (weeks/months/years)", {
  ctx_weeks <- list(time = list(unit = "weeks", origin = as.Date("1970-01-01"), zone = "UTC"))

  vars <- list(
    data.frame(patient_id = c("p1","p1","p2"),
               time = as.Date(c("2020-01-01","2020-01-08","2020-01-01")),
               sbp = c(120, 130, 110),
               stringsAsFactors = FALSE)
  )

  g <- build_obs_grid(vars = vars, times = c(0, 1, 2), t0 = as.Date("2020-01-01"),
                      start_time = 0, ctx = ctx_weeks)

  expect_true(is.numeric(g$grid_time[["p1"]]))
  expect_equal(as.numeric(g$grid_time[["p1"]][2] - g$grid_time[["p1"]][1]), 1)
  expect_equal(as.numeric(g$grid_time[["p1"]][3] - g$grid_time[["p1"]][2]), 1)

  ctx_months <- list(time = list(unit = "months", origin = as.Date("1970-01-01"), zone = "UTC"))
  g2 <- build_obs_grid(vars = vars, times = c(0, 1, 2), t0 = as.Date("2020-01-01"),
                       start_time = 0, ctx = ctx_months)
  expect_equal(as.numeric(g2$grid_time[["p1"]][2] - g2$grid_time[["p1"]][1]), 1)

  ctx_years <- list(time = list(unit = "years", origin = as.Date("1970-01-01"), zone = "UTC"))
  g3 <- build_obs_grid(vars = vars, times = c(0, 1, 2), t0 = as.Date("2020-01-01"),
                       start_time = 0, ctx = ctx_years)
  expect_equal(as.numeric(g3$grid_time[["p1"]][2] - g3$grid_time[["p1"]][1]), 1)
})

test_that("build_obs_grid supports POSIXct t0 and POSIXct times via ctx$time (hours)", {
  ctx <- list(time = list(unit = "hours",
                          origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"),
                          zone = "UTC"))

  vars <- list(
    data.frame(patient_id = c("p1","p1"),
               time = as.POSIXct(c("2020-01-01 00:00:00","2020-01-01 01:00:00"), tz = "UTC"),
               sbp = c(120, 130),
               stringsAsFactors = FALSE)
  )

  g <- build_obs_grid(vars = vars, times = c(0, 1, 2), t0 = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
                      start_time = 0, ctx = ctx)

  expect_equal(as.numeric(g$grid_time[["p1"]][2] - g$grid_time[["p1"]][1]), 1)
})

test_that("build_obs_grid errors for calendar t0 without ctx/time_unit and errors for invalid zone", {
  vars <- list(
    data.frame(patient_id = c("p1"),
               time = as.Date("2020-01-01"),
               sbp = 120,
               stringsAsFactors = FALSE)
  )

  expect_error(
    build_obs_grid(vars = vars, times = c(0, 1), t0 = as.Date("2020-01-01")),
    "ctx$time$unit",
    fixed = TRUE
  )

  bad_ctx <- list(time = list(unit = "days", origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"), zone = "NOT_A_ZONE"))
  expect_error(
    build_obs_grid(vars = vars, times = c(0, 1), t0 = as.Date("2020-01-01"), ctx = bad_ctx),
    "time zone",
    ignore.case = TRUE
  )
})
