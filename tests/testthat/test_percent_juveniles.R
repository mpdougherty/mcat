library(mcat)
context("percent_juveniles")

# Create the individual mussel data frame
individuals <- mcat::individuals

# Calculate percent tolerant for the individuals data frame
pj <- percent_juveniles(individuals)

test_that("Check percent juveniles for site 201009221465 = 100", {
  pj_201009221465 <- pj[pj$SampleID == "201009221465", ]
  expect_equal(pj_201009221465$percent_juveniles, 100.0, tolerance = 1e-2)
})

test_that("Check percent juveniles for site 201009221404 = 50", {
  pj_201009221404 <- pj[pj$SampleID == "201009221404", ]
  expect_equal(pj_201009221404$percent_juveniles, 50.0, tolerance = 1e-2)
})

test_that("Check percent juveniles for site 201009211401 = 0", {
  pj_201009211401 <- pj[pj$SampleID == "201009211401", ]
  expect_equal(pj_201009211401$percent_juveniles, 0.0, tolerance = 1e-2)
})

test_that("Check that missing Age still results in percent_juveniles = 0", {
  pj_201009221448 <- pj[pj$SampleID == "201009221448", ]
  expect_equal(pj_201009221448$percent_juveniles, 0.0, tolerance = 1e-2)
})

test_that("Check when SUM_NumberLive and SUM_Juveniles = 0, percent_juveniles = 0", {
  pj_0_0 <- pj[pj$SUM_NumberLive == 0 & pj$SUM_Juveniles == 0, ]
  zero_vector <- rep(0, times = length(pj_0_0$percent_juveniles))
  expect_equal(pj_0_0$percent_juveniles, zero_vector, tolerance = 1e-2)
})

test_that("Check when SUM_NumberLive = 0, percent_juveniles = 0", {
  pj_0 <- pj[pj$SUM_NumberLive == 0, ]
  zero_vector <- rep(0, times = length(pj_0$percent_juveniles))
  expect_equal(pj_0$percent_juveniles, zero_vector, tolerance = 1e-2)
})

test_that("Check percent juveniles not greater than 100", {
  expect_lte(max(pj$percent_juveniles), 100.0)
})

201009221448
