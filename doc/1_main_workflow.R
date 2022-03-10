## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----initiate-impactr---------------------------------------------------------
# If you haven't done so 
# devtools::install_github("impactR)
library(impactR)

## ----initiate-data------------------------------------------------------------
# Load dataset in environment
data(data)

# Show the first lines and types of airports' tibble
data <- data |> tibble::as_tibble()
data

## ----initiate-survey----------------------------------------------------------
data(survey)
survey <- survey |> tibble::as_tibble(survey)

## ----split-survey-------------------------------------------------------------
# All are already defaults apart from 'col_to_split'
survey <- survey |>  
  split_survey(
    col_to_split = "type",
    into = c("type", "list_name"),
    sep = " ",
    fill = "right")

## ----monitor-outliers1--------------------------------------------------------
# Helper to get numeric columns
numeric_cols(data) # Get all numeric columns : it includes for example numeric enumerator ids or gps locations
numeric_cols(data, survey) # Check for numeric columns using the survey sheet.

# Get IQR outliers (1.5 default rule)
outliers_iqr(data, col = i_enquete_age,times = 1.5,  id_col = uuid)  # id_col is usually "uuid" with Kobo

# Get standard deviation outliers
outliers_sd(data, i_enquete_age, times = 3, id_col = uuid)

# Create the full log of outliers of all numeri columns
log_outliers <- make_log_outlier(data, survey, id_col = uuid, today, i_enum_id, i_zad)
log_outliers

## ----monitor-others-----------------------------------------------------------
# Obtenir les autres 
other_cols <- other_cols(data, "autre_", id_col = uuid)
other_cols

# Obtenir les autres parents
other_parent_cols(data, other_cols, "autre_", id_col = uuid)

# Fabriquer le journal de nettoyage des "autres"
log_others <- make_log_other(data, survey, "autre_", uuid, i_enum_id, i_zad) 
log_others


