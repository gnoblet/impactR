
<!-- README.md is generated from README.Rmd. Please edit that file -->

# impactR <img src="man/figures/logo.png" align="right" alt="" width="120"/>

> Ease IMPACT’s data and database officers woRk

`impactR` started as a simple project: mainly a reminder of
totally-perfectible functions used and made on the go for the Burkina
Faso mission in 2021.

All of this is still a very rough draft, but useable.

Yet, it is becoming somehow bigger to make data officers and database
officers daily R work easier as follows:

-   utils.R: functions used on an everyday basis: import .csv and .xlsx
    files, recode NAs, get column names starting with a pattern, etc.
-   monitor.R: functions to produce logs and help monitoring data
    collection (logical tests, outliers, other answers)
-   clean.R: use of the same logs to clean the dataset (update values,
    delete surveys)
-   analysis.R: calculate optimums, weighted proportions and
    interactions, pivot tables reports
-   plots.R: plot functions and themes (colors, fonts)
-   maps.R: some utils (e.g. ease bbox) and some themes (mainly using
    `tmap`)

There is still a need to consolidate parameters and log checks, and (a
lot) of documentation to write.

Specs:

-   mainly using Kobo collection and the `tidyverse`, `srvyr`,
    `janitor`, `tmap`
-   it requires R 4.1+ (mostly for the native pipe `|>`).

## Installation

You can install the last version of impactR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gnoblet/impactR")
```

## Roadmap

From version 0.4, contributions should go with minimal and complete
commits as a good practice. The `dev` branch will be used from there.

-   [ ] introduce tidy eval wherever is makes sense
-   [ ] more plotting functions
-   [x] add (re) count columns post-cleaning for multiple choices
    columns and simple choice’s other column
-   [ ] MSNA analysis tools : roster (education, demography, WGI),
    weighting functions, analysis functions
-   [ ] Split this big mess into several consolidated small packages : a
    viz one, an analysis one and a cleaning one (maybe?).

## Side projects

There will be a Shiny app for cleaning and monitoring (in French for
now) whose repo will be
[collectoR](https://github.com/gnoblet/collectoR).

## Example

These are basics example of daily uses:

``` r
box::use(impactR[...])
## basic example codes and uses (not run!)

## Import a csv file with clean names and clean types
#import_csv("data.csv")

## Get colnames for sector foodsec whose variables start with "f_"
#tbl_col_start(data, "f_")

## Bbox with a buffer (useful to add compass, scale_bar, legends to a map)
#buffer_bbox(admin1_sf, buffer = 0.05)

## Make an outlier log for all numeric variables in the data.frame/tibble
#make_log_outlier(rawdata, survey, id_col = "uuid", cols_to_keep = c("uuid", "i_enum_id"))

## Make a log based on logical tests, outliers and "other" answers
#make_all_logs(rawdata, 
#              survey, 
#              check_list,
#              other = "other_", 
#              id_col = "uuid", 
#              cols_to_keep = c("uuid", "i_enum_id"))

## Recode parent "other" from a well-filled cleaning log
#recode_other_parent_from_log(data, log, id_col = "uuid")

## Calculate weigthed proportion for shelter type by group (e.g. administrative areas or population groups)
#svy_prop(design, group_cols = c("admin1", "group_pop"), "s_shelter_type", na.rm = T, level = 0.95)
```
