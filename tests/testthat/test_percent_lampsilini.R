context("percent_lampsilini")

# Create the individual mussel data frame
individuals <- mcat::individuals

# Calculate percent tolerant for the individuals data frame
plm <- percent_lampsilini(individuals)

test_that("Check percent lampsilini for site 201009211406 = 100", {
  plm_201009211406 <- plm[plm$SampleID == "201009211406", ]
  expect_equal(plm_201009211406$percent_lampsilini, 100.0, tolerance = 1e-2)
})

test_that("Check percent lampsilini for site 201009221410 = 33.333", {
  plm_201009221410 <- plm[plm$SampleID == "201009221410", ]
  expect_equal(plm_201009221410$percent_lampsilini, 33.333333, tolerance = 1e-2)
})

test_that("Check percent lampsilini for site 201009211401 = 0", {
  plm_201009211401 <- plm[plm$SampleID == "201009211401", ]
  expect_equal(plm_201009211401$percent_lampsilini, 0.0, tolerance = 1e-2)
})

test_that("Check when SUM_number_live and SUM_lampsilini = 0, percent_lampsilini = 0", {
  plm_0_0 <- plm[plm$SUM_number_live == 0 & plm$SUM_lampsilini == 0, ]
  zero_vector <- rep(0, times = length(plm_0_0$percent_lampsilini))
  expect_equal(plm_0_0$percent_lampsilini, zero_vector, tolerance = 1e-2)
})

test_that("Check when SUM_number_live = 0, percent_lampsilini = 0", {
  plm_0 <- plm[plm$SUM_number_live == 0, ]
  zero_vector <- rep(0, times = length(plm_0$percent_lampsilini))
  expect_equal(plm_0$percent_lampsilini, zero_vector, tolerance = 1e-2)
})

test_that("Check percent lampsilini not greater than 100", {
  expect_lte(max(plm$percent_lampsilini), 100.0)
})
