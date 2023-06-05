test_that("examples work", {
  expect_no_condition(rtgpd(n = 5, scale = 1, shape = 0, shift = 0, scale_taper = 2))
  expect_no_condition(rtgpd(n = 5, scale = 1:5, shape = 0.1, shift = 0, scale_taper = 10))
  expect_no_condition(rtgpd(n = 5, scale = 1, shape = 0.1 * 1:5, shift = 0, scale_taper = 3))
  expect_no_condition(rtgpd(n = 5, scale = 1, shape = 0, shift = 1:5, scale_taper = 10))
  expect_no_condition(rtgpd(n = 5, scale = 1, shape = 0, shift = 0, scale_taper = 5:10))
})

test_that("non-positive scale parameters are flagged", {
  expect_error(rtgpd(n = 1, scale = -1, shape = 0, shift = 0))
  expect_error(rtgpd(n = 1, scale = 0, shape = 0, shift = 0))
  expect_error(rtgpd(n = 1, scale = c(1,-1), shape = 0, shift = 0))
  expect_error(rtgpd(n = 1, scale = c(1,0), shape = 0, shift = 0))
  expect_error(rtgpd(n = 1, scale = 1, shape = 0, shift = 0, scale_taper = -1))
  expect_error(rtgpd(n = 1, scale = 1, shape = 0, shift = 0, scale_taper = c(0,-1)))
})

test_that("negative shape_tolerance is flagged",{
  expect_error(rtgpd(n = 5, scale = 1,shape = 0,shift = 0, shape_tolerance = -1))
})

test_that("shape_tolerance of length > 1 is flagged", {
  expect_error(rtgpd(n = 5, scale = ,shape = 0,shift = 0, shape_tolerance = 1:5 * 1e-5))
})

test_that("setting the seed works", {
  set.seed(1234)
  tgpd_sample_2 <- rtgpd(n = 5, scale = 1, shape = 0, shift = 0, scale_taper = 2)
  set.seed(1234)
  tgpd_sample_1 <- rtgpd(n = 5, scale = 1, shape = 0, shift = 0, scale_taper = 2)
  expect_equal(tgpd_sample_1, tgpd_sample_2)
})
