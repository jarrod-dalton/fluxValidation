
test_that("make_denom_mask enforces spec invariants and measured_only", {
  alive <- matrix(c(TRUE, NA), nrow = 1)
  fup <- matrix(c(TRUE, TRUE), nrow = 1)
  expect_error(make_denom_mask(alive, fup), "followup_defined")

  alive <- matrix(c(TRUE, TRUE), nrow = 1)
  fup <- matrix(c(TRUE, TRUE), nrow = 1)
  md <- matrix(c(TRUE, FALSE), nrow = 1)
  meas <- matrix(c(TRUE, FALSE), nrow = 1)
  d1 <- make_denom_mask(alive, fup, model_defined = md)
  expect_identical(as.logical(d1[1, ]), c(TRUE, FALSE))

  d2 <- make_denom_mask(alive, fup, model_defined = md, measured = meas, measured_only = TRUE)
  expect_identical(as.logical(d2[1, ]), c(TRUE, FALSE))
})

test_that("compute_obs_risk policy vs interval behave as expected", {
  vars <- list(data.frame(entity_id=c("p1","p2"), sex=c("F","M")))
  events <- data.frame(
    entity_id=c("p1","p2"),
    event_time=c(0.5, 1.2),
    event_type=c("E","E"),
    stringsAsFactors = FALSE
  )
  obs <- build_obs_grid(vars, events = events, times = c(0,1,2), t0 = 0)

  pol <- compute_obs_risk(obs, event = "E", times = c(0,1,2), start_time = 0, mode = "policy")
  expect_equal(pol$risk[pol$time==1], 0.5)
  expect_equal(pol$risk[pol$time==2], 1.0)

  # Interval mode: risk at time 1 uses interval (0,1], at time 2 uses interval (1,2]
  int <- compute_obs_risk(obs, event = "E", times = c(0,1,2), start_time = 0, mode = "interval")
  expect_true(is.na(int$risk[int$time==0]))
  expect_equal(int$risk[int$time==1], 0.5)
  expect_equal(int$risk[int$time==2], 0.5)
})
