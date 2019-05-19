context("percent_listed")

# Create the individual mussel data frame
individuals <- mcat::individuals

# Calculate percent listed for the individuals data frame
pl <- percent_listed(individuals)

test_that("Check percent listed for site 201009221404 = 0", {
  pl_201009221404 <- pl[pl$SampleID == "201009221404", ]
  expect_equal(pl_201009221404$percent_listed, 0.0, tolerance = 1e-2)
})

test_that("Check percent listed for site 201009221410 = 33.3", {
  pl_201009221410 <- pl[pl$SampleID == "201009221410", ]
  expect_equal(pl_201009221410$percent_listed, 33.3333333, tolerance = 1e-2)
})

test_that("Check percent listed for site 201009221423 = 100", {
  pl_201009221423 <- pl[pl$SampleID == "201009221423", ]
  expect_equal(pl_201009221423$percent_listed, 100.0, tolerance = 1e-2)
})

test_that("Check when SUM_NumberLive and SUM_Listed = 0, percent_listed = 0", {
  pl_0_0 <- pl[pl$SUM_NumberLive == 0 & pl$SUM_Listed == 0, ]
  zero_vector <- rep(0, times = length(pl_0_0$percent_listed))
  expect_equal(pl_0_0$percent_listed, zero_vector, tolerance = 1e-2)
})

test_that("Check when SUM_NumberLive = 0, percent_listed = 0", {
  pl_0 <- pl[pl$SUM_NumberLive == 0, ]
  zero_vector <- rep(0, times = length(pl_0$percent_listed))
  expect_equal(pl_0$percent_listed, zero_vector, tolerance = 1e-2)
})

test_that("Check percent listed not greater than 100", {
  expect_lte(max(pl$percent_listed), 100.0)
})
