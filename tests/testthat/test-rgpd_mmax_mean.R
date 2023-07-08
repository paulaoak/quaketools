test_that("examples work", {
  expect_no_condition(rgpd_mmax_mean(n = 5, mmax = 3, mean = 1, shift = 0))
  expect_no_condition(rgpd_mmax_mean(n = 5, mmax = 3:5, mean = 1, shift = 0))
  expect_no_condition(rgpd_mmax_mean(n = 5, mmax = 10, shape = 0.5 * 1:5, shift = 0))
  expect_no_condition(rgpd_mmax_mean(n = 5, scale = 10, shape = 5, shift = 1:4))
})

test_that("shift greater than mean or mean greater than mmax scale parameters are flagged", {
  expect_error(rgpd_mmax_mean(n = 5, mmax = 0.5, mean = 1, shift = 0))
  expect_error(rgpd_mmax_mean(n = 5, mmax = 3, mean = 1, shift = 2))
  expect_error(rgpd_mmax_mean(n = 5, mmax = c(1,0), mean = 1, shift = 0))
})

test_that("setting the seed works", {
  set.seed(1234)
  gpd_sample_2 <-rgpd_mmax_mean(n = 5, mmax = 3, mean = 1, shift = 0)
  set.seed(1234)
  gpd_sample_1 <- rgpd_mmax_mean(n = 5, mmax = 3, mean = 1, shift = 0)
  expect_equal(gpd_sample_1, gpd_sample_2)
})
