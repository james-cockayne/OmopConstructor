test_that("Test collapse records", {
  cdm <- omopgenerics::emptyCdmReference(cdmName = "test") |>
    copyCdm()

  # simple example
  x <- dplyr::tibble(
    id = c(1L, 1L, 1L, 2L, 2L, 3L),
    start_date = as.Date("2000-01-01") + c(0L, 20L, 31L, 10L, 40L, 0L),
    end_date = as.Date("2000-01-01") + c(20L, 30L, 33L, 33L, 45L, 47L)
  )
  cdm <- omopgenerics::insertTable(cdm = cdm, name = "test", table = x)

  # gap 0
  startTables <- omopgenerics::listSourceTables(cdm = cdm)
  res0 <- cdm$test |>
    collapseRecords(
      startDate = "start_date",
      endDate = "end_date",
      by = "id",
      gap = 0L,
      name = "test2"
    )
  expect_true(inherits(res0, "cdm_table"))
  expect_equal(
    res0 |>
      dplyr::collect() |>
      dplyr::arrange(.data$id, .data$start_date),
    dplyr::tibble(
      id = c(1L, 1L, 2L, 2L, 3L),
      start_date = as.Date("2000-01-01") + c(0L, 31L, 10L, 40L, 0L),
      end_date = as.Date("2000-01-01") + c(30L, 33L, 33L, 45L, 47L)
    )
  )
  expect_identical(omopgenerics::tableName(res0), "test2")
  endTables <- omopgenerics::listSourceTables(cdm = cdm)
  expect_identical(setdiff(startTables, endTables), character())
  expect_identical(sort(endTables), sort(unique(c(startTables, "test2"))))

  # gap 1
  expect_equal(
    cdm$test |>
      collapseRecords(
        startDate = "start_date",
        endDate = "end_date",
        by = "id",
        gap = 1L,
        name = "test2"
      ) |>
      dplyr::collect() |>
      dplyr::arrange(.data$id, .data$start_date),
    dplyr::tibble(
      id = c(1L, 2L, 2L, 3L),
      start_date = as.Date("2000-01-01") + c(0L, 10L, 40L, 0L),
      end_date = as.Date("2000-01-01") + c(33L, 33L, 45L, 47L)
    )
  )

  # gap 6
  expect_equal(
    cdm$test |>
      collapseRecords(
        startDate = "start_date",
        endDate = "end_date",
        by = "id",
        gap = 6L,
        name = "test2"
      ) |>
      dplyr::collect() |>
      dplyr::arrange(.data$id, .data$start_date),
    dplyr::tibble(
      id = c(1L, 2L, 2L, 3L),
      start_date = as.Date("2000-01-01") + c(0L, 10L, 40L, 0L),
      end_date = as.Date("2000-01-01") + c(33L, 33L, 45L, 47L)
    )
  )

  # gap 7
  expect_equal(
    cdm$test |>
      collapseRecords(
        startDate = "start_date",
        endDate = "end_date",
        by = "id",
        gap = 7L,
        name = "test2"
      ) |>
      dplyr::collect() |>
      dplyr::arrange(.data$id, .data$start_date),
    dplyr::tibble(
      id = c(1L, 2L, 3L),
      start_date = as.Date("2000-01-01") + c(0L, 10L, 0L),
      end_date = as.Date("2000-01-01") + c(33L, 45L, 47L)
    )
  )

  # gap Inf
  expect_equal(
    cdm$test |>
      collapseRecords(
        startDate = "start_date",
        endDate = "end_date",
        by = "id",
        gap = Inf,
        name = "test2"
      ) |>
      dplyr::collect() |>
      dplyr::arrange(.data$id, .data$start_date),
    dplyr::tibble(
      id = c(1L, 2L, 3L),
      start_date = as.Date("2000-01-01") + c(0L, 10L, 0L),
      end_date = as.Date("2000-01-01") + c(33L, 45L, 47L)
    )
  )

  # no by
  expect_equal(
    cdm$test |>
      collapseRecords(
        startDate = "start_date",
        endDate = "end_date",
        by = character(),
        gap = Inf,
        name = "test2"
      ) |>
      dplyr::collect(),
    dplyr::tibble(
      start_date = as.Date("2000-01-01"), end_date = as.Date("2000-02-17")
    )
  )
  expect_equal(
    cdm$test |>
      collapseRecords(
        startDate = "start_date",
        endDate = "end_date",
        by = character(),
        gap = 5L,
        name = "test2"
      ) |>
      dplyr::collect(),
    dplyr::tibble(
      start_date = as.Date("2000-01-01"), end_date = as.Date("2000-02-17")
    )
  )

  # multiple by
  x <- dplyr::tibble(
    person_id = c(1L, 1L, 1L, 1L, 1L, 3L),
    group = c("A", "A", "A", "B", "B", "B"),
    start_date = as.Date("2000-01-01") + c(0L, 20L, 31L, 10L, 40L, 0L),
    end_date = as.Date("2000-01-01") + c(20L, 30L, 33L, 33L, 45L, 47L)
  )
  cdm <- omopgenerics::insertTable(cdm = cdm, name = "test", table = x)
  expect_equal(
    cdm$test |>
      collapseRecords(
        startDate = "start_date",
        endDate = "end_date",
        by = c("person_id", "group"),
        gap = 15L,
        name = "test2"
      ) |>
      dplyr::collect() |>
      dplyr::arrange(.data$person_id, .data$start_date),
    dplyr::tibble(
      person_id = c(1L, 1L, 3L),
      group = c("A", "B", "B"),
      start_date = as.Date("2000-01-01") + c(0L, 10L, 0L),
      end_date = as.Date("2000-01-01") + c(33L, 45L, 47L)
    )
  )

  # empty table
  expect_equal(
    cdm$test |>
      dplyr::filter(.data$person_id == 0L) |>
      collapseRecords(
        startDate = "start_date",
        endDate = "end_date",
        by = c("person_id", "group"),
        gap = 15L,
        name = "test2"
      ) |>
      dplyr::collect() |>
      dplyr::arrange(.data$person_id, .data$start_date),
    dplyr::tibble(
      person_id = integer(),
      group = character(),
      start_date = as.Date(character()),
      end_date = as.Date(character())
    )
  )

  dropCreatedTables(cdm = cdm)
})
