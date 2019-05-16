library(mcat)
context("percent_tolerant")

# Create the individual mussel data frame
individuals <- mcat::individuals

# Calculate percent tolerant for the individuals data frame
pt <- percent_tolerant(individuals)

test_that("Check percent tolerant for site 201009211401 = 100", {
  pt_201009211401 <- pt[pt$SampleID == "201009211401", ]
  expect_equal(pt_201009211401$percent_tolerant, 100.0, tolerance = 1e-2)
})

test_that("Check percent tolerant for site 201009211404 = 75", {
  pt_201009211404 <- pt[pt$SampleID == "201009211404", ]
  expect_equal(pt_201009211404$percent_tolerant, 75.0, tolerance = 1e-2)
})

test_that("Check percent tolerant for site 201009211406 = 0", {
  pt_201009211406 <- pt[pt$SampleID == "201009211406", ]
  expect_equal(pt_201009211406$percent_tolerant, 0.0, tolerance = 1e-2)
})

test_that("Check when SUM_NumberLive and SUM_Tolerant = 0, percent_tolerant = 0", {
  pt_0_0 <- pt[pt$SUM_NumberLive == 0 & pt$SUM_Tolerant == 0, ]
  zero_vector <- rep(0, times = length(pt_0_0$percent_tolerant))
  expect_equal(pt_0_0$percent_tolerant, zero_vector, tolerance = 1e-2)
})

test_that("Check when SUM_NumberLive = 0, percent_tolerant = 0", {
  pt_0 <- pt[pt$SUM_NumberLive == 0, ]
  zero_vector <- rep(0, times = length(pt_0$percent_tolerant))
  expect_equal(pt_0$percent_tolerant, zero_vector, tolerance = 1e-2)
})

test_that("Check percent tolerant not greater than 100", {
  expect_lte(max(pt$percent_tolerant), 100.0)
})
