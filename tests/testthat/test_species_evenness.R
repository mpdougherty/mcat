context("species_evenness")

# Create the individual mussel data frame
individuals <- mcat::individuals

# Calculate abundance for the individuals data frame
se <- species_evenness(individuals)

test_that("Check pielou_evenness for site 201009211401 = 0", {
  se_201009211401 <- se[se$SampleID == "201009211401", ]
  expect_equal(se_201009211401$species_pielou_evenness, 0, tolerance = 1e-2)
})

test_that("Check pielou_evenness for site 201009211404 = 0.9463946", {
  se_201009211404 <- se[se$SampleID == "201009211404", ]
  expect_equal(se_201009211404$species_pielou_evenness, 0.9463946, tolerance = 1e-2)
})

test_that("Check pielou_evenness for site 201009211408 = 0", {
  se_201009211408 <- se[se$SampleID == "201009211408", ]
  expect_equal(se_201009211408$species_pielou_evenness, 0, tolerance = 1e-2)
})

test_that("Check pielou_evenness for site 201009221409 = 1", {
  se_201009221409 <- se[se$SampleID == "201009221409", ]
  expect_equal(se_201009221409$species_pielou_evenness, 1, tolerance = 1e-2)
})

test_that("Check species_number for site 201009211401 = 1", {
  se_201009211401 <- se[se$SampleID == "201009211401", ]
  expect_equal(se_201009211401$species_number, 1, tolerance = 1e-2)
})

test_that("Check species_number for site 201009211404 = 3", {
  se_201009211404 <- se[se$SampleID == "201009211404", ]
  expect_equal(se_201009211404$species_number, 3, tolerance = 1e-2)
})

test_that("Check species_number for site 201009211408 = 0", {
  se_201009211408 <- se[se$SampleID == "201009211408", ]
  expect_equal(se_201009211408$species_number, 0, tolerance = 1e-2)
})

test_that("Check species_number for site 201009221409 = 2", {
  se_201009221409 <- se[se$SampleID == "201009221409", ]
  expect_equal(se_201009221409$species_number, 2, tolerance = 1e-2)
})
