## code to prepare `survey` dataset goes here

survey <- import_xlsx("data-raw/kobo_data_survey_choices.xlsx", sheet = "survey")

usethis::use_data(survey, overwrite = TRUE)
