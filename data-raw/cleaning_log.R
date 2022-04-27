## code to prepare `check_list` dataset goes here

cleaning_log <- import_xlsx("data-raw/cleaning_log.xlsx", sheet = "cleaning_log")

usethis::use_data(cleaning_log, overwrite = TRUE)
