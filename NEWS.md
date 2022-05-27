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


