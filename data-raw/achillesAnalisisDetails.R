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
