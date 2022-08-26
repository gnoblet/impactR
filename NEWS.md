# impactR 0.8.7.9000

* Bux fix: bug fix to `make_analysis_from_dap()` when binding if choices for one analysis are numeric a,d for another one are character.

---

# impactR 0.8.6.9000

* Bux fix: bug fix to `check_cleaning_log()` for checking identifcal other old and new values.

---


# impactR 0.8.5.9000

* New feature: welcome to `filter2_equalt()` and `filter2_nequal()`.

---

# impactR 0.8.4.9000

* Rewrite: `make_log_from_check_list()` uses `purrr::pmap()` instead of `purrr::exec()`.

---

# impactR 0.8.3.9000

* New feature: `check_cleaning_log()` now checks whether the identification column for each survey `id_col` exists in the cleaning log. It also checks whether all ids from the cleaning log exists in data. (#69)
* New: `na_count()` adds a column with the counts of NAs row-wise over the character columns.

---


# impactR 0.8.2.9000

* Bug fix: for `check_cleaning_log()`.

---

# impactR 0.8.1.9000

* Update: `check_cleaning_log()` now throw a warning if there are missing 'other_new_value' but 'other_old_value' contained more than 2 values and if there are remaining strings from the template in 'feedback' and 'new_value'.

---

# impactR 0.8.9000

* Breaking: `date_to_day_month()` loses the useless `locale` parameter. It now follows the system env locale.
* New: `string_count()` adds a column with the counts of the string pattern per row over the character columns.

---

# impactR 0.7.12.9000

## New features

* `make_analysis()` and `make_analysis_from_dap()` gets a lot fresher look and are totally usable. There's still the need for a `check_dap()` function.

---

# impactR 0.7.11.9000

## New features

* `get_choices()` now returns a tibble when `label = TRUE`. Two columns, the name and the label. (#61)
* `make_analysis()` and `make_analysis_from_dap()` gets a fresh look and can now be used while still under total development. (#62)

---

# impactR 0.7.10.9000

## New features

* Four new foodsec indicators calculation: Food Consumption Score `fcs()`, reduce Coping Strategy Index `rcsi()`, Household Hunger Scale `hhs()`, Livelihood Coping Strategy Index `lcsi()`. (#59)
* Fews net matrix and scores: FEWS NET Food Consumption Matrix's cell `fcm()`, Food Consumption phase according to the FCM `fcp()` and FEWS NET Food Consumption-Livelihood Coping phase `fclcp()`. (#59)
* A general function that calculates all: `foodsec()`. (#59)
* Thanks to [SaeedR1987/healthyr](https://github.com/SaeedR1987/healthyr).

--- 

# impactR 0.7.9.9000

## New features

* Two new helpers to export nice excel workbooks using `openxlsx`: `clean_workbook_list()` to export a named list of dataframes and `clean_workbook_df()` to export one dataframe. (#57)
* Updated README.

--- 

# impactR 0.7.8.9000

## New features

* `check_cleaning_log()` gains many features (#51)
* `clean_all()` now internally uses `check_cleaning_log()` before cleaning (#52)
* `modify_from_log()` now internally uses `readr::type_convert()` to ensure type conversion of new values from cleaning log (#55)

--- 


# impactR 0.7.7.9000

## Minor bug fixes

* `modify_from_log()` gains a `other` argument, it now only modify values if the id_check is different than the `other` pattern

--- 

# impactR 0.7.6.9000

## Minor bug fixes

* `numeric_cols()` now do not throw an error when a numeric variable from param `survey` is not in `.tbl` colnames. It knows keep only those that are in both (#47)

--- 

# impactR 0.7.5.9000

## Minor bug fixes

* `make_all_logs()` now throws a warning when there is no column name that starts with the "other" pattern and skips using `make_log_other()` (#45)

--- 

# impactR 0.7.4.9000

* Removed all viz functions, now included in package `visualizeR` (#43)

--- 

# impactR 0.7.3

## Minor bug fixes

* `survey_difftime()` got a fix for reading the `end` parameter

--- 

# impactR 0.7.2 

## New features

* Redispatched R files (#35)

## Breaking changes

* `rm_cols()` becomes `deselect()` (#35)
* `rec_na()` and `rec_values()` become `recode_na()` and `recode_values()` (#35)
* `get_one()` becomes `get_select_one()`; `get_multiple()` becomes `get_select_multiple()`; and, `get_multiple_and_other_parent()`becomes `get_select_multiple_and_other_parent()` (#35)
* `svy_difftime()` and `svy_duration()` become `survey_difftime()` and `survey_duration()` to avoid confusion with survey analysis `svy_*` functions (#35)
* `survey_duration()` gains a `new_colname` parameter that allows for deciding the new column name of the survey duration (#35)

---

# impactR 0.7.1

## Minor bug fixes

* `clean_all()` and all internals functions now have tidy eval and take into account the "other" string (#30)

---

# impactR 0.7

## New features

* Added a `NEWS.md` file to track changes to the package.
* `label()` labels all simple and multiple choices column providing kobo data, survey, and choices; `label_all_select_one()` does it for simple choice columns only, and `label_all_select_multiple()` for multiple choice columns (#26)
* `label_select_one()` and `label_select_multiple()` are helpers, they label one column (#26)

## Breaking changes

* `get_choices()` now has a `label` parameter to get labelled choices, that takes precedence over the `conc` parameter (#28)


## Minor bug fixes and improvements

* `reach_cols()` gains a new parameter `unnamed = T` that allows and has for default an unnamed output vector (#27)
* `reach_cols()` had fixed hex codes (#27)


