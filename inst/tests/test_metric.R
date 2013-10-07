# :vim set filetype=R
context("Kullback-Leibler")
test_that("Distance is greater than or equal to 0", {
  x <- dnorm(dnorm(seq(-10,10, 0.2)), 1, 2)
  y <- dnorm(dnorm(seq(-10,10, 0.2)), 0, 3)
  d <- kl.dist(x,y)
  expect_that(d >= 0, is_true())
})
