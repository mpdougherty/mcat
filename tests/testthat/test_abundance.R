context("abundance")

# Create the individual mussel data frame
individuals <- mcat::individuals

# Calculate abundance for the individuals data frame
a <- abundance(individuals)

test_that("Check abundance for site 201009211401 = 1", {
  a_201009211401 <- a[a$SampleID == "201009211401", ]
  expect_equal(a_201009211401$abundance, 1, tolerance = 1e-2)
})

test_that("Check abundance for site 201009211404 = 4", {
  a_201009211404 <- a[a$SampleID == "201009211404", ]
  expect_equal(a_201009211404$abundance, 4, tolerance = 1e-2)
})

test_that("Check abundance for site 201009211408 = 0", {
  a_201009211408 <- a[a$SampleID == "201009211408", ]
  expect_equal(a_201009211408$abundance, 0, tolerance = 1e-2)
})
