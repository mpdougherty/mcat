context("percent_fresh_dead")

# Create the individual mussel data frame
individuals <- mcat::individuals

# Calculate percent fresh dead for the individuals data frame
pfd <- percent_fresh_dead(individuals)

test_that("Check percent fresh dead for site 201009211401 = 33.33333", {
  pfd_201009211401 <- pfd[pfd$SampleID == "201009211401", ]
  expect_equal(pfd_201009211401$percent_fresh_dead, 33.33333, tolerance = 1e-2)
})

test_that("Check percent fresh dead for site 201009211404 = 0", {
  pfd_201009211404 <- pfd[pfd$SampleID == "201009211404", ]
  expect_equal(pfd_201009211404$percent_fresh_dead, 0.0, tolerance = 1e-2)
})

test_that("Check percent fresh dead for site 201009221461 = 100", {
  pfd_201009221461 <- pfd[pfd$SampleID == "201009221461", ]
  expect_equal(pfd_201009221461$percent_fresh_dead, 100.0, tolerance = 1e-2)
})


test_that("Check that missing Status still results in percent_fresh_dead = 0", {
  pfd_201009221448 <- pfd[pfd$SampleID == "201009221448", ]
  expect_equal(pfd_201009221448$percent_fresh_dead, 0.0, tolerance = 1e-2)
})

test_that("Check when SUM_mussels_per_record and SUM_fresh_dead = 0, percent_fresh_dead = 0", {
  pfd_0_0 <- pfd[pfd$SUM_mussels_per_record == 0 & pfd$SUM_fresh_dead == 0, ]
  zero_vector <- rep(0, times = length(pfd_0_0$percent_fresh_dead))
  expect_equal(pfd_0_0$percent_fresh_dead, zero_vector, tolerance = 1e-2)
})

test_that("Check when SUM_mussels_per_record = 0, percent_fresh_dead = 0", {
  pfd_0 <- pfd[pfd$SUM_mussels_per_record == 0, ]
  zero_vector <- rep(0, times = length(pfd_0$percent_fresh_dead))
  expect_equal(pfd_0$percent_fresh_dead, zero_vector, tolerance = 1e-2)
})

test_that("Check percent fresh_dead not greater than 100", {
  expect_lte(max(pfd$percent_fresh_dead), 100.0)
})
