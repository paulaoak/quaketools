test_that("examples work", {
  evaluation_points <- c(-1, 0, 0.5, 1, 1.9, 2.1, 5)
  expect_no_condition(dgpd_mmax_mean(x = evaluation_points, mmax = 5, mean = 3))
  expect_no_condition(dgpd_mmax_mean(x = evaluation_points, mmax = 10, mean = 4))
})
