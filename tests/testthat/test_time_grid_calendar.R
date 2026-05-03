test_that("build_obs_grid supports Date t0 and Date state times using time_spec units (weeks/months/years)", {
  ts_weeks <- fluxCore::time_spec(unit = "weeks", origin = as.Date("1970-01-01"), zone = "UTC")

  vars <- list(
    data.frame(entity_id = c("p1","p1","p2"),
               time = as.Date(c("2020-01-01","2020-01-08","2020-01-01")),
               sbp = c(120, 130, 110),
               stringsAsFactors = FALSE)
  )

  g <- build_obs_grid(vars = vars, times = c(0, 1, 2), t0 = as.Date("2020-01-01"),
                      start_time = 0, time_spec = ts_weeks)

  expect_true(is.numeric(g$grid_time[["p1"]]))
  expect_equal(as.numeric(g$grid_time[["p1"]][2] - g$grid_time[["p1"]][1]), 1)
  expect_equal(as.numeric(g$grid_time[["p1"]][3] - g$grid_time[["p1"]][2]), 1)

  ts_months <- fluxCore::time_spec(unit = "months", origin = as.Date("1970-01-01"), zone = "UTC")
  g2 <- build_obs_grid(vars = vars, times = c(0, 1, 2), t0 = as.Date("2020-01-01"),
                       start_time = 0, time_spec = ts_months)
  expect_equal(as.numeric(g2$grid_time[["p1"]][2] - g2$grid_time[["p1"]][1]), 1)

  ts_years <- fluxCore::time_spec(unit = "years", origin = as.Date("1970-01-01"), zone = "UTC")
  g3 <- build_obs_grid(vars = vars, times = c(0, 1, 2), t0 = as.Date("2020-01-01"),
                       start_time = 0, time_spec = ts_years)
  expect_equal(as.numeric(g3$grid_time[["p1"]][2] - g3$grid_time[["p1"]][1]), 1)
})

test_that("build_obs_grid supports POSIXct t0 and POSIXct times via time_spec (hours)", {
  ts <- fluxCore::time_spec(unit = "hours",
                            origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"),
                            zone = "UTC")

  vars <- list(
    data.frame(entity_id = c("p1","p1"),
               time = as.POSIXct(c("2020-01-01 00:00:00","2020-01-01 01:00:00"), tz = "UTC"),
               sbp = c(120, 130),
               stringsAsFactors = FALSE)
  )

  g <- build_obs_grid(vars = vars, times = c(0, 1, 2), t0 = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
                      start_time = 0, time_spec = ts)

  expect_equal(as.numeric(g$grid_time[["p1"]][2] - g$grid_time[["p1"]][1]), 1)
})

test_that("build_obs_grid errors for calendar t0 without time_spec and invalid time_spec type", {
  vars <- list(
    data.frame(entity_id = c("p1"),
               time = as.Date("2020-01-01"),
               sbp = 120,
               stringsAsFactors = FALSE)
  )

  expect_error(
    build_obs_grid(vars = vars, times = c(0, 1), t0 = as.Date("2020-01-01")),
    "time_spec",
    fixed = TRUE
  )

  expect_error(
    build_obs_grid(vars = vars, times = c(0, 1), t0 = as.Date("2020-01-01"), time_spec = list(unit = "days")),
    "time_spec"
  )
})
