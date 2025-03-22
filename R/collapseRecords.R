
#' Collapse records of a `cdm_table` into episodes.
#'
#' @param x A `cdm_table` object.
#' @param startDate Column in `x` that points to the start date of the record.
#' @param endDate Column in `x` that point to the end date of the record.
#' @param by Columns in `x` that aggregate the records.
#' @param gap Integer; distance allowed between two consecutive records to be
#' collapsed.
#' @param name Name of the new `cdm_table` created.
#'
#' @return The `x` `cdm_table` with the records collapsed.
#' @export
#'
collapseRecords <- function(x,
                            startDate,
                            endDate,
                            by,
                            gap = 0L,
                            name = NULL) {
  # input check
  omopgenerics::validateCdmTable(table = x)
  cdm <- omopgenerics::cdmReference(x)
  omopgenerics::validateColumn(column = startDate, x = x)
  omopgenerics::validateColumn(column = endDate, x = x)
  omopgenerics::assertCharacter(by)
  omopgenerics::assertChoice(by, colnames(x))
  omopgenerics::assertNumeric(gap, integerish = TRUE, min = 0, length = 1L)
  if (!is.infinite(gap)) {
    gap <- as.integer(gap)
  }
  name <- omopgenerics::validateNameArgument(name, cdm = cdm, null = TRUE)
  extraColumns <- colnames(x) |>
    purrr::keep(\(x) !x %in% c(startDate, endDate, by))
  if (length(extraColumns) > 0) {
    cli::cli_inform(c("!" = "Columns {.var {extraColumns}} will be dropped from the cdm_table."))
  }

  if (omopgenerics::isTableEmpty(x)) {
    return(
      x |>
        dplyr::select(dplyr::all_of(c(by, startDate, endDate))) |>
        dplyr::compute(name = name)
    )
  }

  # get unique ids
  ids <- omopgenerics::uniqueId(n = 5, exclude = c(by, startDate, endDate))
  # this is so any name of column can be used in by, startdate or endDate
  # id[1] -> date
  # id[2] -> date_id (-1 start; 1 end)
  # id[3] -> cum_id (cumulative id, if 0 end of episode if -1 and date_id = -1 start of episode)
  # id[4] -> name (start or end date)
  # id[5] -> era_id (number of era)

  # start dates
  start <- x |>
    dplyr::select(dplyr::all_of(c(by, !!id[1] := startDate))) |>
    dplyr::mutate(!!id[2] := -1L)
  # end dates
  end <- x |>
    dplyr::select(dplyr::all_of(c(by, !!id[1] := endDate))) |>
    dplyr::mutate(!!id[2] := 1L)

  # add gap
  if (gap > 0L) {
    end <- end |>
      dplyr::mutate(!!id[1] := as.Date(clock::add_days(x = .data[[id[1]]], n = .env$gap)))
  }

  # join dates
  x <- start |>
    dplyr::union_all(end) |>
    dplyr::compute(name = name)

  x <- x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::arrange(.data[[id[1]]], .data[[id[2]]]) |>
    dplyr::mutate(!!id[3] := cumsum(.data[[id[2]]])) |>
    dplyr::filter(
      .data[[id[3]]] == 0L | (.data[[id[3]]] == -1L & .data[[id[2]]] == -1L)
    ) |>
    dplyr::mutate(
      !!id[4] := dplyr::if_else(.data[[id[2]]] == -1L, .env$startDate, .env$endDate),
      !!id[5] := cumsum(dplyr::if_else(.data[[id[3]]] == -1L, 1L, 0L))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(c(by, id[5], id[4], id[1]))) |>
    dplyr::compute(name = name) |>
    tidyr::pivot_wider(names_from = id[4], values_from = id[1]) |>
    dplyr::select(dplyr::all_of(c(by, startDate, endDate)))

  if (gap > 0) {
    x <- x |>
      dplyr::mutate(!!endDate := as.Date(clock::add_days(x = .data[[endDate]], n = -gap)))
  }

  x |>
    dplyr::compute(name = name)
}
