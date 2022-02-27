## code to prepare `check_list` dataset goes here

check_list <- import_xlsx("data-raw/kobo_data_survey_choices.xlsx", sheet = "check_list")

usethis::use_data(check_list, overwrite = TRUE)
