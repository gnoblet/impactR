# impactR 0.7.2

# impactR 0.7.1

## Minor bug fixes

* `clean_all()` and all internals functions now have tidy eval and take into acocunt the "other" string (#30)


# impactR 0.7

## New features

* Added a `NEWS.md` file to track changes to the package.
* `label()` labels all simple and multiple choices column providing kobo data, survey, and choices; `label_all_select_one()` does it for simple choice columns only, and `label_all_select_multiple()` for multiple choice columns (#26)
* `label_select_one()` and `label_select_multiple()` are helpers, they label one column (#26)

## Breaking changes

* `get_choices()` now has a `label` paramater to get labelled choices, that takes precedence over the `conc` parameter (#28)


## Minor bug fixes and improvements

* `reach_cols()` gains a new parameter `unnamed = T` that allows and has for default an unnamed output vector (#27)
* `reach_cols()` had fixed hex codes (#27)


