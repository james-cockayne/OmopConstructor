test_that("Test collapse records", {
  cdm <- omopgenerics::emptyCdmReference(cdmName = "test") |>
    omopgenerics::insertCdmTo(to = src)
})
