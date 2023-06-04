test_that("example works", {
  v <- rep(c(1.6,1.2, 1.1), each = 50)
  x <- rgpd_rd(n = 150,scale = 1.1, shape = 0.2, shift = v, to_nearest = 0.1)

  expect_no_condition(mle_gpd_rd_varu(sigxi = c(1, 0.01), u = 1, v= v, x=x))
})

test_that("low latent threshold u of length greater than 1 is flagged", {
  v <- rep(c(1.6,1.2, 1.1), each = 50)
  x <- rgpd_rd(n = 150,scale = 1.1, shape = 0.2, shift = v, to_nearest = 0.1)

  expect_error(mle_gpd_rd_varu(sigxi = c(1, 0.01), u = 1:5, v= v, x=x))
})

test_that("non valid starting conditions are flagged", {
  set.seed(1234)
  v <- rep(c(1.6,1.2, 1.1), each = 50)
  x <- rgpd_rd(n = 150,scale = 1.1, shape = 0.2, shift = v, to_nearest = 0.1)

  expect_error(mle_gpd_rd_varu(sigxi = c(0.1, -1), u = 1, v= v1, x=x1))
})

test_that("non logical type for hessian and llh_val are flagged", {
  v2 <- rep(c(1.6,1.2), each = 50)
  x2 <- rgpd_rd(n = 100, scale = 1.1, shape = 0.2, shift = v2, to_nearest = 0.1)

  expect_error(mle_gpd_rd_varu(sigxi = c(1, 0.01), u = 1, v= v2, x=x2, hessian = 'a'))
  expect_error(mle_gpd_rd_varu(sigxi = c(1, 0.01), u = 1, v= v2, x=x2, llh_val = 5))
})
