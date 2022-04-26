#'##########################
#
# Because ggplot2 is part of the tidyverse
#
# First time playing with ggplot functions, especially color scales
# It totally needs to become more efficient
# - remove duplicated code
# - better pass null args
# - add warning and error messages
#
#'##########################


#' @title Some reach theme for ggplot
#'
#' @param family A character name of the font family. Default to "Leelawadee UI".
#'
#' @description Give some reach colors and fonts to a ggplot. Based on theme_bw().
#'
#' @return A theme to be added to the "+" ggplot grammar
#'
#' @export
theme_reach <- function(family = "Leelawadee UI") {

  rlang::check_installed("ggplot2", reason = "Package \"ggplot2\" needed for `theme_reach()` to work. Please install it.")

  ggplot2::theme_bw() +
    ggplot2::theme(
      title = ggplot2::element_text(family = family,
                                    size = 12,
                                    colour = "#58585A",
                                    hjust = 0.5,
                                    vjust = 0.5),
      text = ggplot2::element_text(family = family,
                                   colour = "#58585A"),
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 11),
      strip.text = ggplot2::element_text(size = 11),
      legend.title = ggplot2::element_text(size = 11),
      panel.background = ggplot2::element_rect(colour = "white", fill = "white", size = 0.5),
      strip.background = ggplot2::element_rect(linetype = "solid", colour = "#58585A", fill = "white")
    )
}


#' @title Some reach more minimal theme for ggplot
#'
#' @param family A character name of the font family. Default to "Leelawadee UI".
#'
#' @description Give some reach colors and fonts to a ggplot. Based on theme_bw().
#'
#' @return A theme to be added to the "+" ggplot grammar for `impactR::reach_flip_bar()`
#'
#' @export
theme_flip_simple_reach <- function(family = "Leelawadee UI") {

  rlang::check_installed("ggplot2", reason = "Package \"ggplot2\" needed for `theme_flip_simple_reach()` to work. Please install it.")

  ggplot2::theme_bw() +
    ggplot2::theme(
      title = ggplot2::element_text(family = family,
                                    size = 12,
                                    colour = "#58585A",
                                    hjust = 0.5,
                                    vjust = 0.5),
      text = ggplot2::element_text(family = family,
                                   colour = "#58585A"),
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 11),
      strip.text = ggplot2::element_text(size = 11),
      legend.title = ggplot2::element_text(size = 11),
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
}


#' @title Some reach more minimal theme for ggplot
#'
#' @param family A character name of the font family. Default to "Leelawadee UI".
#'
#' @description Give some reach colors and fonts to a ggplot. Based on theme_bw().
#'
#' @return A theme to be added to the "+" ggplot grammar for `impactR::reach_hist_bar()`
#'
#' @export
theme_hist_simple_reach <- function(family = "Leelawadee UI") {

  rlang::check_installed("ggplot2", reason = "Package \"ggplot2\" needed for `theme_hist_simple_reach()` to work. Please install it.")

  ggplot2::theme_bw() +
    ggplot2::theme(
      title = ggplot2::element_text(family = family,
                                    size = 12,
                                    colour = "#58585A",
                                    hjust = 0.5,
                                    vjust = 0.5),
      text = ggplot2::element_text(family = family,
                                   colour = "#58585A"),
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 11),
      strip.text = ggplot2::element_text(size = 11),
      legend.title = ggplot2::element_text(size = 11),
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
    )
}





#' @title Function to extract reach colors as hex codes
#'
#' @param ... Character names of reach colors. If NULL returns all colors
#' @param unnamed Should the output vector be unnamed? Default to T
#'
#' @return An hex code
#'
#' @details This function needs to be modified to add colors
#'
#' @export
reach_cols <- function(..., unnamed = T) {
  cols <- c(...)

  reach_colors <- c(white        = "#FFFFFF",
                    black        = "#000000",
                    main_grey    = "#58585A",
                    main_red     = "#EE5859",
                    main_lt_grey = "#c7c8ca",
                    main_beige   = "#D2CBB8",
                    iroise_1     = "#DFECEF",
                    iroise_2     = "#B1D7E0",
                    iroise_3     = "#699DA3",
                    iroise_4     = "#236A7A",
                    iroise_5     = "#0C3842",
                    red_main_1   = "#AE2829",
                    red_main_2   = "#D05E5F",
                    red_main_3   = "#DB9797",
                    red_main_4   = "#EBC7C8",
                    red_main_5   = "#FAF2F2",
                    red_alt_1    = "#792a2e",
                    red_alt_2    = "#c0474a",
                    red_alt_3    = "#ee5859",
                    red_alt_4    = "#f49695",
                    red_alt_5    = "#f8d6d6",
                    red_alt_na   = "#f8f4f4",
                    lt_grey_1    = "#C6C6C6",
                    lt_grey_2    = "#818183",
                    grey3        = "#E3E3E3",
                    dk_grey      = "#464647",
                    two_dots_1   = "#706441",
                    two_dots_2   = "#56b4e9",
                    two_dots_flashy_1 = "gold1",
                    two_dots_flashy_2 = "blue2",
                    three_dots_1 = "aquamarine2",
                    three_dots_2 = "cornflowerbluer",
                    three_dots_3 = "brown1",
                    orpink       = "#f8aa9b",
                    pink         = "#f5a6a7",
                    lt_pink      = "#F9C6C7",
                    hot_pink     = "#ef6d6f",
                    mddk_red     = "#bf4749",
                    dk_red       = "#782c2e",
                    orange       = "#F69E61",
                    lt_green     = "#B0CFAC",
                    green        = "#84A181",
                    dk_green     = "#526450")

  if (is.null(cols))
    cols_to_return <- reach_colors

  cols_to_return <- reach_colors[cols]

  if(unnamed){
    cols_to_return <- unname(cols_to_return)
  }

  return(cols_to_return)
}






#' Return function to interpolate a reach color palette
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param color_ramp_palette Should the output be a `grDevices::colorRampPalette` function or a vector of hex codes? Default to the former with TRUE.
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @return A color palette
#'
#' @export
reach_pal <- function(palette = "main", reverse = FALSE, color_ramp_palette = F, ...) {


  reach_palettes <- list(
    `main`            = reach_cols("main_grey", "main_red", "main_lt_grey", "main_beige"),
    `primary`         = reach_cols("main_grey", "main_red"),
    `secondary`       = reach_cols("main_lt_grey", "main_beige"),
    `two_dots`        = reach_cols("two_dots_1", "two_dots_2"),
    `two_dots_flashy` = reach_cols("two_dots_flashy_1", "two_dots_flashy_2"),
    `red_main`        = reach_cols("red_main_1", "red_main_2", "red_main_3", "red_main_4", "red_main_5"),
    `red_alt`         = reach_cols("red_alt_1", "red_alt_2", "red_alt_3", "red_alt_4", "red_alt_5"),
    `iroise`          = reach_cols("iroise_1", "iroise_2", "iroise_3", "iroise_4", "iroise_5"),
    `discrete_6`      = reach_cols("dk_grey", "red_main_1", "main_beige", "red_main_2", "lt_grey_2", "red_4")
  )

  pal <- reach_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  if (rlang::is_true(color_ramp_palette)) {
    rlang::check_installed("grDevices", reason = "Package \"grDevices\" needed for `reach_pal()` woth 'color_ramp_palette' set to `TRUE` to work. Please install it.")

    pal <- grDevices::colorRampPalette(pal, ...)
  }

  return(pal)
}