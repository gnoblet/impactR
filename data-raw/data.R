## code to prepare `data` dataset goes here

data <- import_xlsx("data-raw/kobo_data_survey_choices.xlsx", sheet = "data")

usethis::use_data(data, overwrite = TRUE)
