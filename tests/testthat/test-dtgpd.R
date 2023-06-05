test_that("examples work", {
  evaluation_points <- c(-1, 0, 0.5, 1, 1.9, 2.1, 5)
  expect_no_condition(dtgpd(x = evaluation_points, scale = 1, shape = 0, scale_taper = 2))
  expect_no_condition(dtgpd(x = evaluation_points, scale = 1, shape = 0.2, scale_taper = 2))
  expect_no_condition(dtgpd(x = evaluation_points, scale = 1, shape = -0.2, scale_taper = 3))
})

test_that("exponential case works",{
  evaluation_points <- seq(-1, 4)
  dtgpd_values <- dtgpd(x = evaluation_points, scale = 2, shape = 0, scale_taper = 4)
  empirical_values <- (0.5+0.25) * exp(-(0.5+0.25) * evaluation_points) * (evaluation_points >= 0)
  expect_equal(dtgpd_values, empirical_values)
})

test_that("linear case works", {
  evaluation_points <- seq(-1, 4)
  dtgpd_values <- dtgpd(x = evaluation_points, scale = 2, shape = -0.5, scale_taper = 4)
  empirical_values <- (0.25 + 0.5 * pmax(1 - 0.25 * evaluation_points, 0)^(-1)) *exp(-evaluation_points/4) * pmax(1 - 0.25 * evaluation_points, 0)^2 * (evaluation_points >= 0 & evaluation_points < 4)
  for (i in 1:length(evaluation_points)){
    expect_equal(dtgpd_values[i], empirical_values[i], tolerance=1e-4)
  }
})

