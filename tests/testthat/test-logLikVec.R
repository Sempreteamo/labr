# Check that loglikelihood values agree

if (requireNamespace("betareg", quietly = TRUE)) {
  library(betareg)

  # An example from the betarge::betareg documentation.
  data("GasolineYield", package = "betareg")
  gy <- betareg(yield ~ batch + temp, data = GasolineYield)
  temp <- gy
  adj_gy <- alogLik(gy)

  # Note: we use ignore_attr = TRUE because logLikVec.betareg() includes the
  # nobs attribute but logLik.betareg() does not.

  # Check that summing the vector returned by logLikVec.betareg() gives the
  # same value as logLik.betareg()
  test_that("logLik() vs. logLik(logLikVec)", {
    testthat::expect_equal(logLik(gy), logLik(logLikVec(gy)),
                           ignore_attr = TRUE)
  })

  # Check that alogLik.betareg() also returned the correct maximised
  # loglikelihood
  test_that("logLik() vs. logLik(logLikVec)", {
    testthat::expect_equal(logLik(gy), logLik(adj_gy),
                           ignore_attr = TRUE)
  })

}
