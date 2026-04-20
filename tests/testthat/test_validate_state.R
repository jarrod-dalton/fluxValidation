test_that("validate_state_prob returns observed level proportions", {
  obs <- list(
    entity_ids = c("p1", "p2", "p3"),
    times = c(0, 1, 2),
    alive_mask = matrix(TRUE, nrow = 3, ncol = 3),
    followup_defined = matrix(TRUE, nrow = 3, ncol = 3),
    model_defined = list(sex = matrix(TRUE, nrow = 3, ncol = 3)),
    measured = list(sex = matrix(TRUE, nrow = 3, ncol = 3)),
    state = list(sex = matrix(c("M", "F", "M",
                               "M", "F", "F",
                               "M", NA, "F"),
                             nrow = 3, byrow = FALSE))
  )
  class(obs) <- "flux_obs_grid"

  pred <- list(
    spec = list(var = "sex", times = c(0, 1, 2), start_time = 0, levels = c("F", "M")),
    result = data.frame(time = rep(c(0, 1, 2), each = 2), level = rep(c("F", "M"), times = 3), prob = 0.5)
  )
  class(pred) <- "flux_state_prob"

  res <- validate_state_prob(pred, obs, var = "sex", measured_only = TRUE)
  expect_true(all(c("predicted", "observed", "meta") %in% names(res)))
  expect_true(all(c("time", "level", "n", "prop") %in% names(res$observed)))
  expect_true(all(res$observed$time %in% c(0, 1, 2)))
})

test_that("validate_state_point returns observed means", {
  obs <- list(
    entity_ids = c("p1", "p2"),
    times = c(0, 1),
    alive_mask = matrix(TRUE, nrow = 2, ncol = 2),
    followup_defined = matrix(TRUE, nrow = 2, ncol = 2),
    measured = list(hba1c = matrix(TRUE, nrow = 2, ncol = 2)),
    state = list(hba1c = matrix(c(7.0, 8.0,
                                 7.5, NA), nrow = 2, byrow = FALSE))
  )
  class(obs) <- "flux_obs_grid"

  pred <- list(
    spec = list(var = "hba1c", times = c(0, 1), start_time = 0),
    result = data.frame(time = c(0, 1), mean = c(7.2, 7.4))
  )
  class(pred) <- "flux_state_point"

  res <- validate_state_point(pred, obs, var = "hba1c", measured_only = TRUE)
  expect_true(all(c("time", "n", "mean", "sd") %in% names(res$observed)))
  expect_equal(nrow(res$predicted), 2)
})
