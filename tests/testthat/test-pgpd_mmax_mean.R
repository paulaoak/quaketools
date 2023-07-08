test_that("examples work", {
  expect_no_condition(pgpd_mmax_mean(q = seq(-1, 2), mmax = 10, mean = 5))
  expect_no_condition(pgpd_mmax_mean(q = 1, mmax = c(20,10), mean = c(5,6)))
})
