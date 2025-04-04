## code to prepare `achillesAnalisisDetails` dataset goes here

achillesAnalisisDetails <- readr::read_csv(
  file = here::here("data-raw", "achilles_analysis_details.csv"),
  col_types = c(
    "ANALYSIS_ID" = "i", "DISTRIBUTION" = "i", "DISTRIBUTED_FIELD" = "c",
    "ANALYSIS_NAME" = "c", "STRATUM_1_NAME" = "c", "STRATUM_2_NAME" = "c",
    "STRATUM_3_NAME" = "c", "STRATUM_4_NAME" = "c", "STRATUM_5_NAME" = "c",
    "IS_DEFAULT" = "i", "CATEGORY" = "c"
  )
) |>
  dplyr::rename_all(tolower)

usethis::use_data(achillesAnalisisDetails, overwrite = TRUE)

achillesAnalisisInternal <- achillesAnalisisDetails |>
  dplyr::mutate(type = dplyr::case_when(
    .data$analysis_id %in% c(1:5, 10, 12) ~ "count",
    .data$analysis_id == 7L ~ "invalid:provider_id",
    .data$analysis_id == 8L ~ "invalid:provider_id",
    .data$analysis_id == 9L ~ "invalid:provider_id",
    .data$analysis_id == 11L ~ "not_deceased",
  ))

usethis::use_data(achillesAnalisisInternal, overwrite = TRUE, internal = TRUE)
