test_that("examples work", {
  expect_no_condition(ptgpd(q = seq(-1, 2), shape = 0, scale = 1))
  expect_no_condition(ptgpd(q = seq(-1, 2), shape = 1e-11, scale = 1))
  expect_no_condition(ptgpd(q = 1, shape = c(0,-1), scale = c(0.1,1), scale_taper = c(2, 2.1)))
})


test_that("exponential example matches theory", {
  intended_output <- c(0, 0, 1 - exp(-1*1.5), 1 - exp(-2*1.5))
  ptgpd_output_0 <- ptgpd(q = seq(-1, 2), scale = 1, shape = 0, scale_taper = 2)
  ptgpd_output_1 <- ptgpd(q = seq(-1, 2), scale = 1, shape = 1e-11, scale_taper = 2)

  expect_equal(intended_output, ptgpd_output_0)
  expect_equal(intended_output, ptgpd_output_1)
})

test_that("UEP correction works",{
  expect_equal(ptgpd(q = 5,scale = 2,shape = -0.5, scale_taper = 5), 1)
})

