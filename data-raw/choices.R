## code to prepare `data` dataset goes here

choices <- import_xlsx("data-raw/kobo_data_survey_choices.xlsx", sheet = "choices")

usethis::use_data(choices, overwrite = TRUE)
