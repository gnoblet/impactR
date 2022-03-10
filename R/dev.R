# usr <- "bkf_hsm_bdd"
# pwd <- "hsm_BDD"
#
# ####----set global variables ----------
# kobo_server_url<-"https://kobo.humanitarianresponse.info/"
# kc_server_url<-"https://kc.humanitarianresponse.info/"
#
# kobo.humanitarianresponse.info
#
# api_token <- "57873b77d086f46ab900c9e63cacc677b96ba906"
#
#
# box::use(robotoolbox[...])
#
# library(robotoolbox)
# token <- kobo_token(username = usr,
#                     password = pwd,
#                     url = kobo_server_url)
# token
#
# kobo_setup(url = kobo_server_url, token = api_token)
#
# l <- kobo_asset_list()
# l
#
# data <- kobo_data("a8zTzGMJt53f9ATyhXH3jz")
#
#
#
# label_lookup_map <-function(.tbl) {
#   tibble::tibble(
#     col_name = .tbl |> colnames(),
#     labels = .tbl |> purrr::map_chr(purrr::attr_getter("label"))
#   )}
