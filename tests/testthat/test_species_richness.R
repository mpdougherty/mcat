context("species_richness")

# Create the individual mussel data frame
individuals <- mcat::individuals

# Calculate species richness for the individuals data frame
sr <- species_richness(individuals)

test_that("Check rarefy_es_100 for site 201009211401 = 1", {
  sr_201009211401 <- sr[sr$SampleID == "201009211401", ]
  expect_equal(sr_201009211401$rarefy_es_100, 1, tolerance = 1e-2)
})

test_that("Check rarefy_es_100 for site 201009211404 = 3", {
  sr_201009211404 <- sr[sr$SampleID == "201009211404", ]
  expect_equal(sr_201009211404$rarefy_es_100, 3, tolerance = 1e-2)
})

test_that("Check rarefy_es_100 for site 201009211408 = 0", {
  sr_201009211408 <- sr[sr$SampleID == "201009211408", ]
  expect_equal(sr_201009211408$rarefy_es_100, 0, tolerance = 1e-2)
})

test_that("Check species_number for site 201009211401 = 1", {
  sr_201009211401 <- sr[sr$SampleID == "201009211401", ]
  expect_equal(sr_201009211401$species_number, 1, tolerance = 1e-2)
})

test_that("Check species_number for site 201009211404 = 3", {
  sr_201009211404 <- sr[sr$SampleID == "201009211404", ]
  expect_equal(sr_201009211404$species_number, 3, tolerance = 1e-2)
})

test_that("Check species_number for site 201009211408 = 0", {
  sr_201009211408 <- sr[sr$SampleID == "201009211408", ]
  expect_equal(sr_201009211408$species_number, 0, tolerance = 1e-2)
})
