#' @ FCS - Food Consumption Score
#'
#' @param .tbl Data
#' @param fcs_cereal Cereals component column
#' @param fcs_legumes Legumes component column
#' @param fcs_dairy Dairy component column
#' @param fcs_meat Meat component column
#' @param fcs_veg Vegetables component column
#' @param fcs_fruit Fruit component column
#' @param fcs_oil Oil component column
#' @param fcs_sugar Sugar component column
#' @param cat Cut-offs categories, either "normal" (21.5/35) or "alternate" (28/42)
#'
#' @details All component columns must of type 'double'.
#'
#' @return Ten new columns: each component weight (fcs_w_), the score (fcs_score) and category (fcs_cat).
#'
#' @export
fcs <- function(.tbl, fcs_cereal, fcs_legumes, fcs_dairy, fcs_meat, fcs_veg, fcs_fruit, fcs_oil, fcs_sugar, cat = "normal"){

  if (!(cat %in% c("normal", "alternate"))) {rlang::abort("`cat` must be one of: 'normal' or 'alternate'.")}

  .tbl <- .tbl |>
    dplyr::mutate(
      fcs_w_cereal = {{ fcs_cereal }} * 2,
      fcs_w_legumes = {{ fcs_legumes }} * 3,
      fcs_w_dairy = {{ fcs_dairy }} * 4,
      fcs_w_meat = {{ fcs_meat }} * 4,
      fcs_w_veg = {{ fcs_veg }} * 1,
      fcs_w_fruit = {{ fcs_fruit }} * 1,
      fcs_w_oil = {{ fcs_oil }} * 0.5,
      fcs_w_sugar = {{ fcs_sugar }} * 0.5
    )

  .tbl <- .tbl |>
    dplyr::mutate(fcs_score = purrr::pmap_dbl(
      .tbl  |> dplyr::select(dplyr::starts_with("fcs_w_")),
      ~ sum(c(...), na.rm = T)))

  if (cat == "normal") {
    .tbl <- .tbl |>
      dplyr::mutate(fcs_cat = dplyr::case_when(
        fcs_score <= 21 ~ "Poor",
        fcs_score <= 35 ~ "Borderline",
        fcs_score <= 200 ~ "Acceptable",
        TRUE ~ NA_character_
      ))
  } else {
    .tbl <- .tbl |>
      dplyr::mutate(fcs_cat = dplyr::case_when(
        fcs_score <= 28 ~ "Poor",
        fcs_score <= 42 ~ "Borderline",
        fcs_score <= 200 ~ "Acceptable",
        TRUE ~ NA_character_))
  }

  return(.tbl)
}




#' @title HHS - Household Hunger Scale
#'
#' @param .tbl Data
#' @param hhs_lev1_nofoodhh Component column: No food of any kind in the house
#' @param hhs_lev2_nofoodhh Follow-up frequency column
#' @param hhs_lev1_sleephungry Component column: Go to sleep hungry because there was not enough food
#' @param hhs_lev2_sleephungry Follow-up frequency column
#' @param hhs_lev1_alldaynight Component column: Go a whole day and night without eating
#' @param hhs_lev2_alldaynight Follow-up frequency column
#' @param level1_codes Character vector of at least "Yes" and "No" codes (in this order), e.g. c("yes", "no")
#' @param level2_codes Character vector of at least frequencies codes, in the following order: "Rarely", "Sometimes", "Often", e.g. c("rarely", "sometimes", "often")
#'
#' @return Five new columns: each component recoded hhs_comp1, hhs_comp2, hhs_comp3, the overall score (hhs_score) and categories (hhs_cat).
#'
#' @export
hhs <- function(.tbl, hhs_lev1_nofoodhh, hhs_lev2_nofoodhh, hhs_lev1_sleephungry, hhs_lev2_sleephungry, hhs_lev1_alldaynight, hhs_lev2_alldaynight, level1_codes, level2_codes){

  hhs_recoding <- function(q_yes_no, q_freq, level1_codes, level2_codes){
    dplyr::case_when(
      {{ q_yes_no }} == level1_codes[2] ~ 0,
      {{ q_freq }} %in% level2_codes[1:2] ~ 1,
      {{ q_freq }} == level2_codes[3] ~ 2,
      TRUE ~ NA_real_
    )
  }

  .tbl <- .tbl |>
    dplyr::mutate(
      hhs_lev1_nofoodhh = {{ hhs_lev1_nofoodhh }},
      hhs_lev2_nofoodhh = {{ hhs_lev2_nofoodhh }},
      hhs_lev1_sleephungry = {{ hhs_lev1_sleephungry }},
      hhs_lev2_sleephungry = {{ hhs_lev2_sleephungry }},
      hhs_lev1_alldaynight = {{ hhs_lev1_alldaynight }},
      hhs_lev2_alldaynight = {{ hhs_lev2_alldaynight }}
    )

  .tbl <- .tbl |>
    dplyr::mutate(
      hhs_comp1 = hhs_recoding(.data$hhs_lev1_nofoodhh, .data$hhs_lev2_nofoodhh, level1_codes, level2_codes),
      hhs_comp2 = hhs_recoding(.data$hhs_lev1_sleephungry, .data$hhs_lev2_sleephungry, level1_codes, level2_codes),
      hhs_comp3 = hhs_recoding(.data$hhs_lev1_alldaynight, .data$hhs_lev2_alldaynight, level1_codes, level2_codes)
    )

  .tbl <- .tbl |>
    dplyr::mutate(hhs_score = purrr::pmap_dbl(
      .tbl  |> dplyr::select(dplyr::starts_with("hhs_comp")),
      ~ sum(c(...), na.rm = T)))

  .tbl <- .tbl |>
    dplyr::mutate(hhs_cat = dplyr::case_when(
      hhs_score == 0 ~ "None",
      hhs_score <= 1 ~ "Little",
      hhs_score <= 3 ~ "Moderate",
      hhs_score <= 4 ~ "Severe",
      hhs_score <= 6 ~ "Very Severe",
      TRUE ~ NA_character_))

  return(.tbl)
}



#' @title rCSI - reduced Coping Strategy Index
#'
#' @param .tbl Data
#' @param rcsi_lesspreferred Component column: Rely on less preferred and less expensive food
#' @param rcsi_borrowfood Component column: Borrow food or rely on help from friends or relatives
#' @param rcsi_limitportion Component column: Limit portion size at mealtime
#' @param rcsi_restrict Component column: Restrict consumption by adults in order for small children to eat
#' @param rcsi_reducemeals Component column: Reduce the number of meals eaten in a day
#'
#' @details All component columns must of type 'double'.
#'
#' @return Seven new columns: each component weight (rcsi_w_*), rCSI score (rcsi_score) and categories (rcsi_cat).
#'
#' @export
rcsi <- function(.tbl, rcsi_lesspreferred, rcsi_borrowfood, rcsi_limitportion, rcsi_restrict, rcsi_reducemeals){

  .tbl <- .tbl |>
    dplyr::mutate(
      rcsi_lesspreferred = {{ rcsi_lesspreferred }},
      rcsi_borrowfood = {{ rcsi_borrowfood }},
      rcsi_limitportion = {{ rcsi_limitportion }},
      rcsi_restrict = {{ rcsi_restrict }},
      rcsi_reducemeals = {{ rcsi_reducemeals }}
    )

  .tbl <- .tbl |>
    dplyr::mutate(
      rcsi_w_lesspreferred = .data$ rcsi_lesspreferred * 1,
      rcsi_w_borrowfood = .data$ rcsi_borrowfood * 2,
      rcsi_w_limitportion = .data$ rcsi_limitportion * 1,
      rcsi_w_restrict = .data$ rcsi_restrict * 3,
      rcsi_w_reducemeals = .data$ rcsi_reducemeals * 1
    )

  .tbl <- .tbl |>
    dplyr::mutate(rcsi_score = purrr::pmap_dbl(
      .tbl  |> dplyr::select(dplyr::starts_with("rcsi_w_")),
      ~ sum(c(...), na.rm = T)))

  .tbl <- .tbl |>
    dplyr::mutate(rcsi_cat = dplyr::case_when(
      rcsi_score <= 3 ~ "No to Low",
      rcsi_score <= 18 ~ "Medium",
      rcsi_score <= 1000 ~ "Severe",
      TRUE ~ NA_character_))

  return(.tbl)

}


#' @title LCSI - Livelihood Coping Strategy Index
#'
#' @param .tbl Data
#' @param lcs_stress_1 Component column: Stress strategy 1
#' @param lcs_stress_2 Component column: Stress strategy 2
#' @param lcs_stress_3 Component column: Stress strategy 3
#' @param lcs_stress_4 Component column: Stress strategy 4
#' @param lcs_crisis_1 Component column: Crisis strategy 1
#' @param lcs_crisis_2 Component column: Crisis strategy 2
#' @param lcs_crisis_3 Component column: Crisis strategy 3
#' @param lcs_emergency_1 Component column: Emergency strategy 1
#' @param lcs_emergency_2 Component column: Emergency strategy 2
#' @param lcs_emergency_3 Component column: Emergency strategy 3
#' @param level_codes Character vector of at responses codes, in the following order: ""Yes", "No, exhausted", "No, no need", "No, not applicable", e.g. c("yes", "exhausted", "no_need", "not_applicable")
#'
#' @return Fourteen new columns: each strategy recoded (lcs_stress_*, lcs_crisis_*, lcs_emergency_*), a dummy for each category (lcs_stress, lcs_crisis, lcs_emergency), and the category (lcs_cat).
#'
#' @export
lcsi <- function(.tbl, lcs_stress_1, lcs_stress_2, lcs_stress_3, lcs_stress_4, lcs_crisis_1, lcs_crisis_2, lcs_crisis_3, lcs_emergency_1, lcs_emergency_2, lcs_emergency_3, level_codes) {


  lcs_recoding <- function(var, level_codes){
    dplyr::case_when(
      {{ var }} %in% level_codes[1:2] ~ 1,
      {{ var }} %in% level_codes[3:4] ~ 0,
      TRUE ~ NA_real_)
  }


  .tbl <- .tbl |>
    dplyr::mutate(
      lcs_stress_1 = lcs_recoding({{ lcs_stress_1 }}, level_codes),
      lcs_stress_2 = lcs_recoding({{ lcs_stress_2 }}, level_codes),
      lcs_stress_3 = lcs_recoding({{ lcs_stress_3 }}, level_codes),
      lcs_stress_4 = lcs_recoding({{ lcs_stress_4 }}, level_codes),
      lcs_crisis_1 = lcs_recoding({{ lcs_crisis_1 }}, level_codes),
      lcs_crisis_2 = lcs_recoding({{ lcs_crisis_2 }}, level_codes),
      lcs_crisis_3 = lcs_recoding({{ lcs_crisis_3 }}, level_codes),
      lcs_emergency_1 = lcs_recoding({{ lcs_emergency_1 }}, level_codes),
      lcs_emergency_2 = lcs_recoding({{ lcs_emergency_2 }}, level_codes),
      lcs_emergency_3 = lcs_recoding({{ lcs_emergency_3 }}, level_codes)
    )

  .tbl <- .tbl |>
    dplyr::mutate(
      lcs_stress = purrr::pmap_dbl(
        .tbl  |> dplyr::select(dplyr::starts_with("lcs_stress_")),
        ~ ifelse(sum(c(...), na.rm = T) >= 1, 1, 0)),
      lcs_crisis = purrr::pmap_dbl(
        .tbl  |> dplyr::select(dplyr::starts_with("lcs_crisis_")),
        ~ ifelse(sum(c(...), na.rm = T) >= 1, 1, 0)),
      lcs_emergency = purrr::pmap_dbl(
        .tbl  |> dplyr::select(dplyr::starts_with("lcs_emergency_")),
        ~ ifelse(sum(c(...), na.rm = T) >= 1, 1, 0))
    )

  .tbl <- .tbl |>
    dplyr::mutate(lcs_cat =  dplyr::case_when(
      lcs_emergency == 1 ~ "Emergency",
      lcs_crisis == 1 ~ "Crisis",
      lcs_stress == 1 ~ "Stress",
      lcs_stress == 0 & lcs_crisis == 0 & lcs_emergency == 0 ~ "None",
      TRUE ~ NA_character_))

  return(.tbl)
}



#' @title FCM - FEWS NET Food Consumption Matrix
#'
#' @param .tbl Data
#' @param fcs_cat Column produced by `impactR::fcs()`
#' @param hhs_cat Column produced by `impactR::hhs()`
#' @param rcsi_cat Column produced by `impactR::rcsi()`
#'
#' @return The FEWS NET Food Consumption Cell number.
#'
#' @export
fcm <- function(.tbl, fcs_cat, hhs_cat, rcsi_cat){

  .tbl <- .tbl |>
    dplyr::mutate(
      fcs_cat = {{ fcs_cat }},
      hhs_cat = {{ hhs_cat }},
      rcsi_cat = {{ rcsi_cat }})

  .tbl <- .tbl |>
    dplyr::mutate(fcm_cell = dplyr::case_when(
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "None" & .data$rcsi_cat == "No to Low" ~ 1,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "Little" & .data$rcsi_cat == "No to Low" ~ 2,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "Moderate" & .data$rcsi_cat == "No to Low" ~ 3,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "Severe" & .data$rcsi_cat == "No to Low" ~ 4,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "Very Severe" & .data$rcsi_cat == "No to Low" ~ 5,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "None" & .data$rcsi_cat == "No to Low" ~ 6,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "Little" & .data$rcsi_cat == "No to Low" ~ 7,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "Moderate" & .data$rcsi_cat == "No to Low" ~ 8,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "Severe" & .data$rcsi_cat == "No to Low" ~ 9,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "Very Severe" & .data$rcsi_cat == "No to Low" ~ 10,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "None" & .data$rcsi_cat == "No to Low" ~ 11,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "Little" & .data$rcsi_cat == "No to Low" ~ 12,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "Moderate" & .data$rcsi_cat == "No to Low" ~ 13,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "Severe" & .data$rcsi_cat == "No to Low" ~ 14,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "Very Severe" & .data$rcsi_cat == "No to Low" ~ 15,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "None" & .data$rcsi_cat == "Medium" ~ 16,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "Little" & .data$rcsi_cat == "Medium" ~ 17,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "Moderate" & .data$rcsi_cat == "Medium" ~ 18,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "Severe" & .data$rcsi_cat == "Medium" ~ 19,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "Very Severe" & .data$rcsi_cat == "Medium" ~ 20,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "None" & .data$rcsi_cat == "Medium" ~ 21,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "Little" & .data$rcsi_cat == "Medium" ~ 22,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "Moderate" & .data$rcsi_cat == "Medium" ~ 23,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "Severe" & .data$rcsi_cat == "Medium" ~ 24,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "Very Severe" & .data$rcsi_cat == "Medium" ~ 25,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "None" & .data$rcsi_cat == "Medium" ~ 26,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "Little" & .data$rcsi_cat == "Medium" ~ 27,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "Moderate" & .data$rcsi_cat == "Medium" ~ 28,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "Severe" & .data$rcsi_cat == "Medium" ~ 29,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "Very Severe" & .data$rcsi_cat == "Medium" ~ 30,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "None" & .data$rcsi_cat == "Severe" ~ 31,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "Little" & .data$rcsi_cat == "Severe" ~ 32,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "Moderate" & .data$rcsi_cat == "Severe" ~ 33,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "Severe" & .data$rcsi_cat == "Severe" ~ 34,
      .data$fcs_cat == "Acceptable" & .data$hhs_cat == "Very Severe" & .data$rcsi_cat == "Severe" ~ 35,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "None" & .data$rcsi_cat == "Severe" ~ 36,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "Little" & .data$rcsi_cat == "Severe" ~ 37,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "Moderate" & .data$rcsi_cat == "Severe" ~ 38,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "Severe" & .data$rcsi_cat == "Severe" ~ 39,
      .data$fcs_cat == "Borderline" & .data$hhs_cat == "Very Severe" & .data$rcsi_cat == "Severe" ~ 40,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "None" & .data$rcsi_cat == "Severe" ~ 41,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "Little" & .data$rcsi_cat == "Severe" ~ 42,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "Moderate" & .data$rcsi_cat == "Severe" ~ 43,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "Severe" & .data$rcsi_cat == "Severe" ~ 44,
      .data$fcs_cat == "Poor" & .data$hhs_cat == "Very Severe" & .data$rcsi_cat == "Severe" ~ 45,
      TRUE ~ NA_real_
    ))

  return(.tbl)

}



#' @title FC phase - Food Consumption phase according to the FCM
#'
#' @param .tbl Data
#' @param fcm_cell Column produced by `impactR::fcm()`
#'
#' @return Two columns: the food consumption phase number (fcp) and the category (fcp_cat)
#'
#' @export
fcp <- function(.tbl, fcm_cell){

  .tbl <- .tbl |>
    dplyr::mutate(
      fcm_cell = {{ fcm_cell }})

  .tbl <- .tbl |>
    dplyr::mutate(fcp = dplyr::case_when(
      .data$fcm_cell %in% c(1,6) ~ 1,
      .data$fcm_cell %in% c(2, 3, 7, 11, 12, 16, 17, 18, 21, 22, 26, 31, 32, 36) ~ 2,
      .data$fcm_cell %in% c(4, 5, 8, 9, 13, 19, 20, 23, 24, 27, 28, 33, 34, 37, 38, 41, 42, 43) ~ 3,
      .data$fcm_cell %in% c(10, 14, 15, 25, 29, 35, 39, 40, 44) ~ 4,
      .data$fcm_cell %in% c(30, 45) ~ 5,
      TRUE ~ NA_real_))

  .tbl <- .tbl |>
    dplyr::mutate(fcp_cat = dplyr::case_when(
      .data$fcp == 1 ~ "Phase 1 FC",
      .data$fcp == 2 ~ "Phase 2 FC",
      .data$fcp == 3 ~ "Phase 3 FC",
      .data$fcp == 4 ~ "Phase 4 FC",
      .data$fcp == 5 ~ "Phase 5 FC",
      TRUE ~ NA_character_
    ))

  return(.tbl)
}



#' @title FEWS NET Food Consumption-Livelihood Coping Matrix
#'
#' @param .tbl Data
#' @param fcp The food consumption phase column produced by `impactR::fcp()`
#' @param lcs_cat The Livelihood Coping Strategies categories produced by `impactR::lcs()`
#'
#' @return Two new columns with the FEWS NET Food Consumption-Livelihood Coping matrix phases: numbers (fclp) and categories (fclp_cat).
#'
#' @export
fclcp <- function(.tbl, fcp, lcs_cat){

  .tbl <- .tbl |>
    dplyr::mutate(
      fcp = {{ fcp }},
      lcs_cat = {{ lcs_cat }}
    )

  .tbl <- .tbl |>
    dplyr::mutate(fclcp = dplyr::case_when(
      .data$fcp == 1 & .data$lcs_cat == "None" ~ 1,
      .data$fcp == 1 & .data$lcs_cat == "Stress" ~ 1,
      .data$fcp == 1 & .data$lcs_cat == "Crisis" ~ 2,
      .data$fcp == 1 & .data$lcs_cat == "Emergency" ~ 3,
      .data$fcp == 2 & .data$lcs_cat == "None" ~ 2,
      .data$fcp == 2 & .data$lcs_cat == "Stress" ~ 2,
      .data$fcp == 2 & .data$lcs_cat == "Crisis" ~ 3,
      .data$fcp == 2 & .data$lcs_cat == "Emergency" ~ 3,
      .data$fcp == 3 & .data$lcs_cat == "None" ~ 3,
      .data$fcp == 3 & .data$lcs_cat == "Stress" ~ 3,
      .data$fcp == 3 & .data$lcs_cat == "Crisis" ~ 3,
      .data$fcp == 3 & .data$lcs_cat == "Emergency" ~ 4,
      .data$fcp == 4 & .data$lcs_cat == "None" ~ 4,
      .data$fcp == 4 & .data$lcs_cat == "Stress" ~ 4,
      .data$fcp == 4 & .data$lcs_cat == "Crisis" ~ 4,
      .data$fcp == 4 & .data$lcs_cat == "Emergency" ~ 5,
      .data$fcp == 5 & .data$lcs_cat == "Emergency" ~ 5,
      TRUE ~ NA_real_
    ))

  .tbl <- .tbl |>
    dplyr::mutate(fclcp_cat = dplyr::case_when(
      .data$fclcp == 1 ~ "Phase 1 FCLC",
      .data$fclcp == 2 ~ "Phase 2 FCLC",
      .data$fclcp == 3 ~ "Phase 3 FClC",
      .data$fclcp == 4 ~ "Phase 4 FClC",
      .data$fclcp == 5 ~ "Phase 5 FClC",
      TRUE ~ NA_character_
    ))

  return(.tbl)
}




#' @title All foodsec indicators
#'
#' @param .tbl Data
#' @param fcs_cereal Cereals component column
#' @param fcs_legumes Legumes component column
#' @param fcs_dairy Dairy component column
#' @param fcs_meat Meat component column
#' @param fcs_veg Vegetables component column
#' @param fcs_fruit Fruit component column
#' @param fcs_oil Oil component column
#' @param fcs_sugar Sugar component column
#' @param fcs_cat Cut-offs categories, either "normal" (21.5/35) or "alternate" (28/42)
#' @param hhs_lev1_nofoodhh Component column: No food of any kind in the house
#' @param hhs_lev2_nofoodhh Follow-up frequency column
#' @param hhs_lev1_sleephungry Component column: Go to sleep hungry because there was not enough food
#' @param hhs_lev2_sleephungry Follow-up frequency column
#' @param hhs_lev1_alldaynight Component column: Go a whole day and night without eating
#' @param hhs_lev2_alldaynight Follow-up frequency column
#' @param hhs_level1_codes Character vector of at least "Yes" and "No" codes (in this order), e.g. c("yes", "no")
#' @param hhs_level2_codes Character vector of at least frequencies codes, in the following order: "Rarely", "Sometimes", "Often", e.g. c("rarely", "sometimes", "often")
#' @param rcsi_lesspreferred Component column: Rely on less preferred and less expensive food
#' @param rcsi_borrowfood Component column: Borrow food or rely on help from friends or relatives
#' @param rcsi_limitportion Component column: Limit portion size at mealtime
#' @param rcsi_restrict Component column: Restrict consumption by adults in order for small children to eat
#' @param rcsi_reducemeals Component column: Reduce the number of meals eaten in a day
#' @param lcs_stress_1 Component column: Stress strategy 1
#' @param lcs_stress_2 Component column: Stress strategy 2
#' @param lcs_stress_3 Component column: Stress strategy 3
#' @param lcs_stress_4 Component column: Stress strategy 4
#' @param lcs_crisis_1 Component column: Crisis strategy 1
#' @param lcs_crisis_2 Component column: Crisis strategy 2
#' @param lcs_crisis_3 Component column: Crisis strategy 3
#' @param lcs_emergency_1 Component column: Emergency strategy 1
#' @param lcs_emergency_2 Component column: Emergency strategy 2
#' @param lcs_emergency_3 Component column: Emergency strategy 3
#' @param lcs_level_codes Character vector of at responses codes, in the following order: ""Yes", "No, exhausted", "No, no need", "No, not applicable", e.g. c("yes", "exhausted", "no_need", "not_applicable")
#'
#'
#' @return All columns produced by the following `impactR` functions: `fcs()`, `hhs()`, `rcsi()`, `lcs()`, `fcm()`, `fcp()` and `fclp()`.
#'
#' @export
foodsec <- function(.tbl,
                    fcs_cereal,
                    fcs_legumes,
                    fcs_dairy,
                    fcs_meat,
                    fcs_veg,
                    fcs_fruit,
                    fcs_oil,
                    fcs_sugar,
                    fcs_cat = "normal",
                    hhs_lev1_nofoodhh,
                    hhs_lev2_nofoodhh,
                    hhs_lev1_sleephungry,
                    hhs_lev2_sleephungry,
                    hhs_lev1_alldaynight,
                    hhs_lev2_alldaynight,
                    hhs_level1_codes,
                    hhs_level2_codes,
                    rcsi_lesspreferred,
                    rcsi_borrowfood,
                    rcsi_limitportion,
                    rcsi_restrict,
                    rcsi_reducemeals,
                    lcs_stress_1,
                    lcs_stress_2,
                    lcs_stress_3,
                    lcs_stress_4,
                    lcs_crisis_1,
                    lcs_crisis_2,
                    lcs_crisis_3,
                    lcs_emergency_1,
                    lcs_emergency_2,
                    lcs_emergency_3,
                    lcs_level_codes){

  .tbl <- fcs(.tbl, {{ fcs_cereal }}, {{ fcs_legumes }}, {{ fcs_dairy }}, {{ fcs_meat }}, {{ fcs_veg }}, {{ fcs_fruit }}, {{ fcs_oil }}, {{ fcs_sugar }}, cat = fcs_cat)

  .tbl <- hhs(.tbl, {{ hhs_lev1_nofoodhh }}, {{ hhs_lev2_nofoodhh }}, {{ hhs_lev1_sleephungry }}, {{ hhs_lev2_sleephungry }}, {{ hhs_lev1_alldaynight }}, {{ hhs_lev2_alldaynight }}, level1_codes = hhs_level1_codes, level2_codes = hhs_level2_codes)

  .tbl <- rcsi(.tbl, {{ rcsi_lesspreferred }}, {{ rcsi_borrowfood }}, {{ rcsi_limitportion }}, {{ rcsi_restrict }}, {{ rcsi_reducemeals }})

  .tbl <- lcsi(.tbl, {{ lcs_stress_1 }}, {{ lcs_stress_2 }}, {{ lcs_stress_3 }}, {{ lcs_stress_4 }}, {{ lcs_crisis_1 }}, {{ lcs_crisis_2 }}, {{ lcs_crisis_3 }}, {{ lcs_emergency_1 }}, {{ lcs_emergency_2 }}, {{ lcs_emergency_3 }}, level_codes = lcs_level_codes)

  .tbl <- fcm(.tbl, .data$fcs_cat, .data$hhs_cat, .data$rcsi_cat)

  .tbl <- fcp(.tbl, .data$fcm_cell)

  .tbl <- fclcp(.tbl, .data$fcp, .data$lcs_cat)


  return(.tbl)
}
