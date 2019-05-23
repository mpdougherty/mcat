context("tribe_evenness")

# Create the individual mussel data frame
individuals <- mcat::individuals

# Calculate abundance for the individuals data frame
te <- tribe_evenness(individuals)

test_that("Check tribe_evenness for site 201009211401 = 0", {
  te_201009211401 <- te[te$SampleID == "201009211401", ]
  expect_equal(te_201009211401$tribe_pielou_evenness, 0, tolerance = 1e-2)
})

test_that("Check tribe_evenness for site 201009211408 = 0", {
  te_201009211408 <- te[te$SampleID == "201009211408", ]
  expect_equal(te_201009211408$tribe_pielou_evenness, 0, tolerance = 1e-2)
})

test_that("Check tribe_evenness for site 201009221431 = 1", {
  te_201009221431 <- te[te$SampleID == "201009221431", ]
  expect_equal(te_201009221431$tribe_pielou_evenness, 1, tolerance = 1e-2)
})

test_that("Check tribe_number for site 201009211401 = 1", {
  te_201009211401 <- te[te$SampleID == "201009211401", ]
  expect_equal(te_201009211401$tribe_number, 1, tolerance = 1e-2)
})

test_that("Check tribe_number for site 201009211408 = 0", {
  te_201009211408 <- te[te$SampleID == "201009211408", ]
  expect_equal(te_201009211408$tribe_number, 0, tolerance = 1e-2)
})

test_that("Check tribe_number for site 201009221431 = 2", {
  te_201009221431 <- te[te$SampleID == "201009221431", ]
  expect_equal(te_201009221431$tribe_number, 2, tolerance = 1e-2)
})
