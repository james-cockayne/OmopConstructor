
#' Generate `observation_period` table from data recorded in the
#' `cdm_reference`.
#'
#' @param cdm A `cdm_reference` object.
#' @param collapseDays Distance between records to be collapsed.
#' @param persistenceDays Number of days added at the end of an observation
#' period as persistence window.
#' @param censorDate Date to censor all followup for individuals.
#' @param censorAge Age to censor individuals if they reach a certain age.
#' The last day in observation of the individual will be the day prior to their
#' birthday.
#' @param recordsFrom Tables to retrieve observation records from.
#' @param periodTypeConceptId Choose the observation_period_type_concept_id that
#' best represents how the period was determined.
#' [Accepted Concepts](https://athena.ohdsi.org/search-terms/terms?domain=Type+Concept&standardConcept=Standard&page=1&pageSize=15&query=).
#'
#' @return The `cdm_reference` object with a new `observation_period`.
#' @export
#'
generateObservationPeriod <- function(cdm,
                                      collapseDays = Inf,
                                      persistenceDays = Inf,
                                      censorDate = NULL,
                                      censorAge = 120L,
                                      recordsFrom = c("drug_exposure", "visit_occurrence"),
                                      periodTypeConceptId = 32817L) {
  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertNumeric(collapseDays, length = 1)
  omopgenerics::assertNumeric(persistenceDays, length = 1)
  censorDate <- validateCensorDate(censorDate = censorDate, cdm = cdm)
  omopgenerics::assertNumeric(censorAge, length = 1)
  omopgenerics::assertNumeric(periodTypeConceptId, integerish = TRUE, length = 1)
  periodTypeConceptId <- as.integer(periodTypeConceptId)
  omopgenerics::assertChoice(recordsFrom, choices = c(
    "drug_exposure", "condition_occurrence", "procedure_occurrence",
    "visit_occurrence", "device_exposure", "measurement", "observation", "death"
  ), null = TRUE)
  recordsFrom <- unique(recordsFrom)
  if (!is.infinite(censorAge)) censorAge <- as.integer(censorAge)
  if (!is.infinite(collapseDays)) collapseDays <- as.integer(collapseDays)
  if (!is.infinite(persistenceDays)) persistenceDays <- as.integer(persistenceDays)
  if (persistenceDays > collapseDays) {
    cli::cli_abort(c(
      x = "persistenceDays ({persistenceDays}) must be < collapseDays ({collapseDays})"
    ))
  }
  if (persistenceDays == collapseDays & !is.infinite(collapseDays)) {
    cli::cli_inform(c(i = "`persistenceDays` ({.pkg {persistenceDays}}) can not be equal to `collapseDays` ({.pkg {collapseDays}}) as back to back observation periods are not allowed, setting `collapseDays = {.pkg {collapseDays+1}}`."))
    collapseDays <- collapseDays + 1
  }

  if (length(recordsFrom) == 0) {
    cli::cli_warn(c("!" = "`recordsFrom` is empty, empty {.pkg observation_period} created."))
    # return empty
    pid <- cdm$person |>
      utils::head(1) |>
      dplyr::pull("person_id")
    op <- dplyr::tibble(
      observation_period_id = integer(),
      person_id = pid,
      observation_period_start_date = as.Date(character()),
      observation_period_end_date = as.Date(character()),
      period_type_concept_id = integer()
    )
    cdm <- omopgenerics::insertTable(
      cdm = cdm, name = "observation_period", table = op
    )
    return(cdm)
  }

  name <- omopgenerics::uniqueTableName()
  x <- getTemptativeDates(
    cdm = cdm, tables = recordsFrom, collapse = collapseDays,
    window = persistenceDays, name = name
  )

  # censor at censorDate
  if (is.infinite(persistenceDays)) {
    x <- x |>
      dplyr::mutate(end_date = .env$censorDate)
  } else {
    x <- x |>
      dplyr::mutate(end_date = dplyr::if_else(
        .data$end_date <= .env$censorDate, .data$end_date, .env$censorDate
      ))
  }

  # censor at death
  if ("death" %in% names(cdm)) {
    x <- x |>
      dplyr::left_join(
        cdm$death |>
          dplyr::select("person_id", "death_date"),
        by = "person_id"
      ) |>
      dplyr::mutate(end_date = dplyr::case_when(
        is.na(.data$death_date) ~ .data$end_date,
        .data$death_date < .data$end_date ~ .data$death_date,
        .default = .data$end_date
      )) |>
      dplyr::select(!"death_date")
  }

  # censor at censorAge
  if (!is.infinite(censorAge)) {
    x <- x |>
      PatientProfiles::addDateOfBirthQuery(dateOfBirthName = "date_of_birth") |>
      dplyr::mutate(
        age_date = as.Date(clock::add_days(
          clock::add_years(.data$date_of_birth, .env$censorAge), -1L
        )),
        end_date = dplyr::if_else(
          .data$age_date <= .data$end_date, .data$age_date, .data$end_date
        )
      )
  }

  # filter censor < start_date
  cdm$observation_period <- x |>
    dplyr::filter(.data$start_date <= .data$end_date) |>
    dplyr::arrange(.data$person_id, .data$start_date) |>
    dplyr::mutate(
      observation_period_id = as.integer(dplyr::row_number()),
      period_type_concept_id = .env$periodTypeConceptId
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
  if (is.infinite(collapse)) {
    end <- !is.infinite(window)
    q <- c(
      "min(.data$start_date, na.rm = TRUE)",
      "max(dplyr::case_when(
        is.na(.data$end_date) ~ .data$start_date,
        .data$end_date < .data$start_date ~ .data$start_date,
        .default = .data$end_date
      ), na.rm = TRUE)"
    ) |>
      rlang::parse_exprs() |>
      rlang::set_names(c("start_date", "end_date"))
    q <- q[c(TRUE, end)]
    nm <- omopgenerics::uniqueTableName()
    for (k in seq_along(tables)) {
      xk <- selectColumns(cdm, tables[k])
      if (end) {
        xk <- xk |>
          correctEndDate()
      }
      xk <- xk |>
        dplyr::group_by(.data$person_id) |>
        dplyr::summarise(!!!q, .groups = "drop")
      if (k == 1) {
        x <- xk |>
          dplyr::compute(name = name)
      } else {
        x <- x |>
          dplyr::union_all(
            xk |>
              dplyr::compute(name = nm)
          ) |>
          dplyr::group_by(.data$person_id) |>
          dplyr::summarise(!!!q, .groups = "drop") |>
          dplyr::compute(name = name)
      }
    }
    omopgenerics::dropSourceTable(cdm = cdm, name = nm)
  } else {
    for (k in seq_along(tables)) {
      xk <- selectColumns(cdm, tables[k])
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
        gap = collapse, name = name
      )
  }
  if (!is.infinite(window) & window > 0) {
    x <- x |>
      dplyr::mutate(end_date = as.Date(clock::add_days(
        .data$end_date, .env$window
      )))
  }
  x
}
selectColumns <- function(cdm, table) {
  startDate <- omopgenerics::omopColumns(table = table, field = "start_date")
  endDate <- omopgenerics::omopColumns(table = table, field = "end_date")
  sel <- c("person_id", startDate, endDate) |>
    rlang::set_names(c("person_id", "start_date", "end_date"))
  x <- cdm[[table]] |>
    dplyr::select(dplyr::all_of(sel))

  # check if casting is needed
  q <- c()
  if (!isColDate(x, "start_date")) {
    q <- c(q, "start_date" = "as.Date(.data$start_date)")
  }
  if (!isColDate(x, "end_date")) {
    q <- c(q, "end_date" = "as.Date(.data$end_date)")
  }
  if (length(q) > 0) {
    q <- rlang::parse_exprs(q)
    x <- x |>
      dplyr::mutate(!!!q)
  }

  return(x)
}
correctEndDate <- function(x) {
  x |>
    dplyr::mutate(end_date = dplyr::case_when(
      is.na(.data$end_date) ~ .data$start_date,
      .data$end_date < .data$start_date ~ .data$start_date,
      .default = .data$end_date
    ))
}
isColDate <- function(x, col) {
  x |>
    dplyr::select(dplyr::all_of(col)) |>
    utils::head(1L) |>
    dplyr::pull() |>
    dplyr::type_sum() == "date"
}
validateCensorDate <- function(censorDate, cdm, call = parent.frame()) {
  if (is.null(censorDate)) {
    if ("cdm_source" %in% names(cdm)) {
      cdmSource <- cdm$cdm_source |>
        dplyr::collect()
      if (nrow(cdmSource) == 1) {
        candidate1 <- as.Date(cdmSource$source_release_date)
        if (checkDate(candidate1)) {
          censorDate <- candidate1
          cli::cli_inform(c(i = "Using censorDate {.pkg {censorDate}} from {.emph source_release_date}."))
        } else {
          candidate2 <- as.Date(cdmSource$cdm_release_date)
          if (checkDate(candidate2)) {
            censorDate <- candidate2
            cli::cli_inform(c(i = "Using censorDate {.pkg {censorDate}} from {.emph cdm_release_date}."))
          }
        }
      }
    }
    if (is.null(censorDate)) {
      censorDate <- as.Date(Sys.Date())
      cli::cli_inform(c(i = "Using current date as censorDate: {.pkg {censorDate}}."))
    }
  } else {
    censorDate <- as.Date(censorDate)
    omopgenerics::assertDate(censorDate, length = 1, call = call)
  }

  if (censorDate > Sys.Date()) {
    cli::cli_warn(c("!" = "Unrealistic censorDate ({censorDate}) in the future provided."))
  }

  return(censorDate)
}
checkDate <- function(date) {
  check <- FALSE
  if (inherits(date, "Date")) {
    check <- isTRUE(date >= as.Date("1950-01-01") && date <= Sys.Date())
  }
  return(check)
}
