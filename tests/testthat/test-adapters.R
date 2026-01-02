
test_that("as_ps_* adapters pass through already-constructed summary objects", {
  r <- list(spec = list(event = "E"), result = data.frame(time = 0, risk = 0))
  class(r) <- "ps_risk"
  out_r <- as_ps_risk(r)
  expect_s3_class(out_r, "ps_risk")

  p <- list(spec = list(var = "sex", times = c(0,1), start_time = 0, levels = c("F","M")),
            result = data.frame(time = c(0,0,1,1), level = rep(c("F","M"),2), prob = 0.5))
  class(p) <- "ps_state_prob"
  out_p <- as_ps_state_prob(p, var = "sex")
  expect_s3_class(out_p, "ps_state_prob")

  m <- list(spec = list(var = "hba1c", times = c(0,1), start_time = 0),
            result = data.frame(time = c(0,1), mean = c(7,7.1)))
  class(m) <- "ps_state_point"
  out_m <- as_ps_state_point(m, var = "hba1c")
  expect_s3_class(out_m, "ps_state_point")
})

