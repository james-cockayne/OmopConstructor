
#' Generate `observation_period` table from data recorded in the
#' `cdm_reference`.
#'
#' @param cdm A `cdm_reference` object.
#' @param collapseEra Distance between records to be collapsed.
#' @param persistenceWindow Number of days added at the end of an observation
#' period as persistence window.
#' @param censorDate Date to censor all followup for individuals.
#' @param censorAge Age to censor individuals if they reach a certain age.
#' @param recordsFrom Tables to retrieve observation records from.
#'
#' @return The `cdm_reference` object with a new `observation_period`.
#' @export
#'
generateObservationPeriod <- function(cdm,
                                      collapseEra = Inf,
                                      persistenceWindow = Inf,
                                      censorDate = Sys.time(),
                                      censorAge = 150L,
                                      recordsFrom = c(
                                        "drug_exposure", "condition_occurrence",
                                        "procedure_occurrence",
                                        "visit_occurrence", "device_exposure",
                                        "measurement", "observation", "death"
                                      )) {
  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertNumeric(collapseEra, length = 1)
  omopgenerics::assertNumeric(persistenceWindow, length = 1)
  censorDate <- as.Date(censorDate)
  omopgenerics::assertDate(censorDate, length = 1)
  omopgenerics::assertNumeric(censorAge, length = 1)
  omopgenerics::assertChoice(recordsFrom, choices = c(
    "drug_exposure", "condition_occurrence", "procedure_occurrence",
    "visit_occurrence", "device_exposure", "measurement", "observation", "death"
  ))
  recordsFrom <- unique(recordsFrom)
  if (!is.infinite(censorAge)) censorAge <- as.integer(censorAge)
  if (persistenceWindow > collapseEra) {
    cli::cli_abort(c(
      x = "persistenceWindow ({persistenceWindow}) must be <= collapseEra ({collapseEra})"
    ))
  }

  if (length(recordsFrom) == 0) {
    # return empty
  }

  name <- omopgenerics::uniqueTableName()
  x <- getTemptativeDates(
    cdm = cdm, tables = recordsFrom, collapse = collapseEra,
    window = persistenceWindow, name = name
  )

  # censor at censorDate
  if (is.infinite(persistenceWindow)) {
    x <- x |>
      dplyr::mutate(end_date = .env$censorDate)
  } else {
    x <- x |>
      dplyr::mutate(end_date = dplyr::if_else(
        .data$end_date <= .env$censorDate, .data$end_date, .env$censorDate
      ))
  }

  # censor at censorAge
  x <- x |>
    PatientProfiles::addDateOfBirthQuery(dateOfBirthName = "date_of_birth") |>
    dplyr::mutate(
      age_date = clock::add_days(
        clock::add_years(.data$date_of_birth, .env$censorAge), -1L
      ),
      end_date = dplyr::if_else(
        .data$age_date <= .data$end_date, .data$age_date, .data$end_date
      )
    )

  # filter censor < start_date
  cdm$observation_period <- x |>
    dplyr::filter(.data$start_date >= .data$end_date) |>
    dplyr::mutate(
      observation_period_id = dplyr::row_number(),
      period_type_concept_id = 0L
    ) |>
    dplyr::select(
      "observation_period_id", "person_id",
      "observation_period_start_date" = "start_date",
      "observation_period_end_date" = "end_date", "period_type_concept_id"
    ) |>
    dplyr::compute(name = "observation_period")

  # drop intermediate table
  omopgenerics::dropSourceTable(cdm = cdm, name = name)

  return(cdm)
}
getTemptativeDates <- function(cdm, tables, collapse, window, name) {
  if (is.infinite(collapseEra)) {
    if (is.infinite(persistenceWindow)) {
      end <- FALSE
    } else {
      end <- TRUE
    }
    q <- c(
      "min(as.Date(.data[['{startDate}']]), na.rm = TRUE)",
      "min(dplyr::coalesce(as.Date(.data[['{endDate}']]), as.Date(.data[['{startDate}']])), na.rm = TRUE)"
    ) |>
      rlang::set_names(c("start_date", "end_date"))
    q <- q[c(TRUE, end)]
    if (length(tables) == 1) {
      nm <- name
    } else {
      nm <- omopgenerics::uniqueTableName()
    }
    for (k in seq_along(tables)) {
      table <- tables[k]
      startDate <- omopgenerics::omopColumns(table = table, field = "start_date")
      endDate <- omopgenerics::omopColumns(table = table, field = "end_date")
      qs <- glue::glue(q, startDate = startDate, endDate = endDate) |>
        rlang::parse_exprs()
      xk <- cdm[[table]] |>
        dplyr::group_by(.data$person_id) |>
        dplyr::summarise(!!!qs, .groups = "drop") |>
        dplyr::compute(name = nm)
      if (k == 1) {
        x <- xk
      } else {
        qq <- glue::glue(q, startDate = "start_date", endDate = "end_date") |>
          rlang::parse_exprs()
        x <- x |>
          dplyr::union_all(xk) |>
          dplyr::group_by(.data$person_id) |>
          dplyr::summarise(!!!qs, .groups = "drop") |>
          dplyr::compute(name = name)
      }
    }
    omopgenerics::dropSourceTable(cdm = cdm, name = nm)
  } else {
    for (k in seq_along(tables)) {
      table <- tables[k]
      startDate <- omopgenerics::omopColumns(table = table, field = "start_date")
      endDate <- omopgenerics::omopColumns(table = table, field = "end_date")
      sel <- c("person_id", startDate, endDate) |>
        rlang::set_names(c("person_id", "start_date", "end_date"))
      xk <- cdm[[table]] |>
        dplyr::select(dplyr::all_of(sel))
      if (k == 1) {
        x <- xk
      } else {
        x <- x |>
          dplyr::union_all(xk)
      }
    }
    x <- x |>
      collapseRecords(
        startDate = "start_date", endDate = "end_date", by = "person_id",
        gap = 0L, name = name
      )
  }
  x
}


