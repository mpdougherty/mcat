library(mcat)
context("percent_listed")

# Create the individual mussel data frame
individuals <- mcat::individuals

# Calculate percent listed for the individuals data frame
pl <- percent_listed(individuals)

test_that("Check percent listed for site 201010261418 = 20", {
  pl_201010261418 <- pl[pl$SampleID == "201010261418", ]
  expect_equal(pl_201010261418$percent_listed, 20.0, tolerance = 1e-2)
})

test_that("Check percent listed for site 201010261401 = 0", {
  pl_201010261418 <- pl[pl$SampleID == "201010261401", ]
  expect_equal(pl_201010261418$percent_listed, 0.0, tolerance = 1e-2)
})

test_that("Check that division by zero is set to percent_listed = 0", {
  pl_201010261418 <- pl[pl$SampleID == "201809241509", ]
  expect_equal(pl_201010261418$percent_listed, 0.0, tolerance = 1e-2)
})
