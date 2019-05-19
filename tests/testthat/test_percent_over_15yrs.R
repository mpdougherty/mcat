library(mcat)
context("percent_over_15yrs")

# Create the individual mussel data frame
individuals <- mcat::individuals

# Calculate percent tolerant for the individuals data frame
po <- percent_over_15yrs(individuals)

test_that("Check percent over_15yrs for site 201009211401 = 100", {
  po_201009211401 <- po[po$SampleID == "201009211401", ]
  expect_equal(po_201009211401$percent_over_15yrs, 100.0, tolerance = 1e-2)
})

test_that("Check percent over_15yrs for site 201009221420 = 40", {
  po_201009221420 <- po[po$SampleID == "201009221420", ]
  expect_equal(po_201009221420$percent_over_15yrs, 40.0, tolerance = 1e-2)
})

test_that("Check percent over_15yrs for site 201009211404 = 0", {
  po_201009211404 <- po[po$SampleID == "201009211404", ]
  expect_equal(po_201009211404$percent_over_15yrs, 0.0, tolerance = 1e-2)
})

test_that("Check that missing Age still results in percent_over_15yrs = 0", {
  po_201009221448 <- po[po$SampleID == "201009221448", ]
  expect_equal(po_201009221448$percent_over_15yrs, 0.0, tolerance = 1e-2)
})

test_that("Check when SUM_NumberLive and SUM_Over_15yrs = 0, percent_over_15yrs = 0", {
  po_0_0 <- po[po$SUM_NumberLive == 0 & po$SUM_Over_15yrs == 0, ]
  zero_vector <- rep(0, times = length(po_0_0$percent_over_15yrs))
  expect_equal(po_0_0$percent_over_15yrs, zero_vector, tolerance = 1e-2)
})

test_that("Check when SUM_NumberLive = 0, percent_over_15yrs = 0", {
  po_0 <- po[po$SUM_NumberLive == 0, ]
  zero_vector <- rep(0, times = length(po_0$percent_over_15yrs))
  expect_equal(po_0$percent_over_15yrs, zero_vector, tolerance = 1e-2)
})

test_that("Check percent over_15yrs not greater than 100", {
  expect_lte(max(po$percent_over_15yrs), 100.0)
})
