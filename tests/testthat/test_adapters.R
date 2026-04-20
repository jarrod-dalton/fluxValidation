
test_that("flux adapters pass through already-constructed summary objects", {
  r <- list(spec = list(event = "E"), result = data.frame(time = 0, event_prob = 0))
  class(r) <- "flux_event_prob"
  out_r <- as_event_prob(r)
  expect_s3_class(out_r, "flux_event_prob")

  p <- list(spec = list(var = "sex", times = c(0,1), start_time = 0, levels = c("F","M")),
            result = data.frame(time = c(0,0,1,1), level = rep(c("F","M"),2), prob = 0.5))
  class(p) <- "flux_state_prob"
  out_p <- as_state_prob(p, var = "sex")
  expect_s3_class(out_p, "flux_state_prob")

  m <- list(spec = list(var = "hba1c", times = c(0,1), start_time = 0),
            result = data.frame(time = c(0,1), mean = c(7,7.1)))
  class(m) <- "flux_state_point"
  out_m <- as_state_point(m, var = "hba1c")
  expect_s3_class(out_m, "flux_state_point")
})
