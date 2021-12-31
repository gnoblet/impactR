#' @title Some colors for REACH plots and maps
#'
#' @param colors A character string among impactR::reach_colors()
#'
#' @return A named list of individual colors or palettes
#'
#' @export
reach_colors <- function(colors = NULL){

  l <- list(white = "#FFFFFF",
            black = "#00000",
            reach_main = c(main_grey = "#58585A", main_red = "#EE5859", main_lt_grey = "#c7c8ca", main_beige = "#D2CBB8"),
            reach_primary = c(main_grey = "#58585A", main_red = "#E5859"),
            reach_secondary = c(main_lt_grey = "#c7c8ca", main_beige = "#D2CBB8"),
            reach_iroise_5_hex = c("#DFECEF", "#B1D7E0", "#699DA3", "#236A7A", "#0C3842"),
            reach_red_5_hex = c("#AE2829", "#D05E5F", "#DB9797", "#EBC7C8", "#FAF2F2"),
            reach_grey = "#58585A",
            reach_lt_grey = "#C6C6C6",
            reach_lt_grey2 = "#818183",
            reach_lt_grey3 = "#E3E3E3",
            reach_dk_grey = "#464647",
            reach_two_dots = c("gold1", "blue2"),
            reach_three_dots = c("aquamarine2", "cornflowerblue", "brown1"),
            reach_red = "#EE5859",
            reach_orpink = "#f8aa9b",
            reach_pink = "#f5a6a7",
            reach_lt_pink = "#F9C6C7",
            reach_hot_pink = "#ef6d6f",
            reach_mddk_red = "#bf4749",
            reach_dk_red = "#782c2e",
            reach_beige = "#D2CBB8",
            reach_orange = "#F69E61",
            reach_lt_green = "#B0CFAC",
            reach_green = "#84A181",
            reach_dk_green = "#526450")

  if (is.null(colors)) return(l)

  if(!all(colors %in% names(l))) stop(paste0("At least one color does not exist. It must be one of: ", paste(names(l), collapse = ", "), ".")) else {
    return(`[`(l, colors))
  }
}


