test_that("examples work", {
  v <- rep(c(1.6,1.2, 1.1), each = 50)
  x <- rgpd_mmax_mean(n = 150,mmax = 10, mean = 5, shift = v)

  expect_no_condition(llh_gpd_mmax_mean_varu(mmax_mean = c(11, 1), u = 1, v= v, x=x))
  expect_no_condition(llh_gpd_mmax_mean_varu(mmax_mean = c(11, 5), u = 1, v= v, x=x))
})
