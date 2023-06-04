test_that("examples work", {
  v <- rep(c(1.6,1.2, 1.1), each = 50)
  x <- rgpd_rd(n = 150,scale = 1.1, shape = 0.2, shift = v, to_nearest = 0.1)

  expect_no_condition(llh_gpd_rd_varu(sigxi = c(0.1, -0.01), u = 1, v= v, x=x))
  expect_no_condition(llh_gpd_rd_varu(sigxi = c(0.1, -0.5), u = 1, v= v, x=x))
})

test_that("low latent threshold u of length greater than 1 is flagged", {
  expect_error(llh_gpd_rd_varu(sigxi = c(0.1, -0.01), u = 1:5, v= v, x=x))
})

test_that("lower endpoint failure is flagged", {
  v1 <- rep(c(1.6,1.2, 1.1), each = 50)
  x1 <- rgpd_rd(n = 150,scale = 1.1, shape = 0.2, shift = v1-1, to_nearest = 0.1)

  expect_error(llh_gpd_rd_varu(sigxi = c(0.1, 0.01), u = 1, v= v1, x=x1))
})

test_that("negative initial scale parameter leads to a value of -10e6 for likelihood",{
  v2 <- rep(c(1.6,1.2), each = 50)
  x2 <- rgpd_rd(n = 100, scale = 1.1, shape = 0.2, shift = v2, to_nearest = 0.1)

  expect_equal(
    object = llh_gpd_rd_varu(sigxi = c(-0.1, 0.01), u = 1, v= v2, x=x2),
    expected = -10e6)
})

test_that("to_nearest of length > 1 and sigxi (initial condition) of length > 2 are flagged", {
  v2 <- rep(c(1.6,1.2), each = 50)
  x2 <- rgpd_rd(n = 100, scale = 1.1, shape = 0.2, shift = v2, to_nearest = 0.1)

  expect_error(llh_gpd_rd_varu(sigxi = c(0.1, 0.01), u = 1, v= v2, x=x2, to_nearest = 0.1*1:3))
  expect_error(llh_gpd_rd_varu(sigxi = c(0.1, 0.01, 0.3), u = 1, v= v2, x=x2))
})
