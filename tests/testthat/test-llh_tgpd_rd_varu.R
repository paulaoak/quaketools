test_that("examples work", {
  v <- rep(c(1.6,1.2, 1.1), each = 50)
  x <- rtgpd_rd(n = 150,scale = 1.1, shape = 0.2, shift = v, scale_taper = 2, to_nearest = 0.1)

  expect_no_condition(llh_tgpd_rd_varu(sigxitheta = c(0.1, -0.01, 0.2), u = 1, v= v, x=x))
  expect_no_condition(llh_tgpd_rd_varu(sigxitheta = c(0.1, 0.01, 0.2), u = 1, v= v, x=x))
})

test_that("low latent threshold u of length greater than 1 is flagged", {
  v <- rep(c(1.6,1.2, 1.1), each = 50)
  x <- rtgpd_rd(n = 150,scale = 1.1, shape = 0.2, shift = v, scale_taper = 2, to_nearest = 0.1)

  expect_error(llh_tgpd_rd_varu(sigxitheta = c(0.1, -0.01, 0.2), u = 1:5, v= v, x=x))
})

test_that("low latent threshold u greater than min(v) is flagged", {
  v <- rep(c(1.6,1.2, 1.1), each = 50)
  x <- rtgpd_rd(n = 150,scale = 1.1, shape = 0.2, shift = v, scale_taper = 2, to_nearest = 0.1)

  expect_error(llh_tgpd_rd_varu(sigxitheta = c(0.1, 0.01, 0.2), u = 1.3, v= v, x=x))
})

test_that("lower endpoint failure is flagged", {
  v1 <- rep(c(1.6,1.2, 1.1), each = 50)
  x1 <- rtgpd_rd(n = 150,scale = 1.1, shape = 0.2, shift = v1-1, scale_taper = 2, to_nearest = 0.1)

  expect_error(llh_tgpd_rd_varu(sigxitheta = c(0.1, 0.01, 0.2), u = 1, v= v1, x=x1))
})

test_that("negative initial scale parameter leads to a value of -10e6 for likelihood",{
  v2 <- rep(c(1.6,1.2), each = 50)
  x2 <- rtgpd_rd(n = 100, scale = 1.1, shape = 0.2, shift = v2, scale_taper = 2, to_nearest = 0.1)

  expect_equal(
    object = llh_tgpd_rd_varu(sigxitheta = c(-0.1, 0.01, 0.2), u = 1, v= v2, x=x2),
    expected = -10e6)

  expect_equal(
    object = llh_tgpd_rd_varu(sigxitheta = c(0.1, 0.01, -0.2), u = 1, v= v2, x=x2),
    expected = -10e6)
})

test_that("to_nearest of length > 1 and sigxi (initial condition) of length > 2 are flagged", {
  v2 <- rep(c(1.6,1.2), each = 50)
  x2 <- rtgpd_rd(n = 100, scale = 1.1, shape = 0.2, shift = v2, scale_taper = 2, to_nearest = 0.1)

  expect_error(llh_tgpd_rd_varu(sigxitheta = c(0.1, 0.01), u = 1, v= v2, x=x2, to_nearest = 0.1*1:3))
  expect_error(llh_tgpd_rd_varu(sigxitheta = c(0.1, 0.01, 0.3, 0.4), u = 1, v= v2, x=x2))
})
