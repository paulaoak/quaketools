test_that("error when p not a valid probability", {
  expect_error(qgpd_mmax_mean(p = -0.1))
  expect_error(qgpd_mmax_mean(p = 1.1))
})
