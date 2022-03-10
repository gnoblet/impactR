## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----initiate-impactr---------------------------------------------------------
# Si vous n'avez pas encore installé le package
# devtools::install_github("impactR)
library(impactR)

## ----initiate-datas-----------------------------------------------------------
# Load dataset in environment
data(data)

# Show the first lines and types of airports' tibble
data <- data |> tibble::as_tibble()
data

## ----initiate-survey----------------------------------------------------------
data(survey)
survey <- survey |> tibble::as_tibble(survey)

survey

## ----split-survey-------------------------------------------------------------
# Miseà part l'argument 'col_to_split' (la colonne à séparer), les autres paramètres sont les paramètres par défaut
survey <- survey |>  
  split_survey(
    col_to_split = "type",
    into = c("type", "list_name"),
    sep = " ",
    fill = "right")

survey

## ----monitor-outliers1--------------------------------------------------------
# Utilitaire pour obtenir toutes les variables numériques
#numeric_cols(data) #  Obtenir toutes les colonnes numériques : cela inclut par exemple les identifiants numériques des énumérateurs ou les points GPS.
#numeric_cols(data, survey) # Obtenir toutes les colonnes numériques en utilisant la feuille 'survey' de l'outil Kobo (uniquement les calculate, integer, etc.)

# Valeurs aberrantes interquartle (règle des 1,5 fois par défaut)
#outliers_iqr(data, col = i_enquete_age,times = 1.5,  id_col = uuid) # La colonne d'identifiant 'id_col' est habituellement "uuid" avec Kobo

# Valeurs aberrantes à la moyenne
#outliers_sd(data, i_enquete_age, times = 3, id_col = uuid)

# Fabriquer le journal de nettoyage entier pour toutes les variables numériques (en prenant en compte l'outil Kobo)
log_outliers <- make_log_outlier(data, survey, id_col = uuid, today, i_enum_id, i_zad)
log_outliers

## ----monitor-others-----------------------------------------------------------
# Obtenir les autres 
other_cols <- other_cols(data, "autre_", id_col = uuid)
other_cols

# Obtenir les autres parents
other_parent_cols(data, other_cols, "autre_", id_col = uuid)

# Fabriquer le journal de nettoyage des "autres"
log_others <- make_log_other(data, survey, "autre_", uuid, today, i_enum_id, i_zad) 
log_others


## ----monitor-check-list-------------------------------------------------------
data(check_list)
check_list <- check_list |> tibble::as_tibble()
check_list

## ----check-check-list, eval = F-----------------------------------------------
#  check_check_list(check_list, data)
#  # following column/s from `question_name` is/are missing in `.tbl`: survey_duration, survey_duration, survey_duration

## ----survey-duration----------------------------------------------------------
data <- data |> 
  svy_duration(start, end) #|>
  # NOT RUN!
  # svy_difftime(start, end, new_colname = "survey_difftime", i_enum_id)
data$survey_duration

## ----check-check-list-true----------------------------------------------------
check_check_list(check_list, data)

## ----log-check-list-----------------------------------------------------------
log_check_list <- make_log_from_check_list(data, survey, check_list, uuid, today, i_enum_id, i_zad)

log_check_list

## ----bind-export, message = F, warning = F------------------------------------
log <- list(log_outliers, log_check_list, log_others) |> 
  purrr::map(~ .x |> dplyr::mutate(dplyr::across(.fns = as.character))) |> 
  dplyr::bind_rows() |> 
  readr::type_convert()
log

## ----make-all-logs, message = F, warning = F----------------------------------
log <- make_all_logs(data, survey, check_list, "autre_", uuid, today, i_enum_id, i_zad)
log

## ----export-------------------------------------------------------------------
# Not run! La plus simple
# writexl::write_xlsx(log, "output/log.xlsx)

# On peut y ajouter la date du jour pour permettre le suivi
# writexl::write_xlsx(log, paste0("output/log_", Sys.Date(), ".xlsx))

