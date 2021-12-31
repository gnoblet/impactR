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
#' @param ... Character names of reach colors. If NULL? returns all colors.
#'
#' @return An hex code
#'
#' @details This function needs to be modified to add colors
#'
#' @export
reach_cols <- function(...) {
  cols <- c(...)

  reach_colors <- c(white        = "#FFFFFF",
                    black        = "#00000",
                    main_grey    = "#58585A",
                    main_red     = "#EE5859",
                    main_lt_grey = "#c7c8ca",
                    main_beige   = "#D2CBB8",
                    iroise_1     = "DFECEF",
                    iroise_2     = "B1D7E0",
                    iroise_3     = "699DA3",
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
                    red_alt_5    ="#f8d6d6",
                    red_alt_na   = "#f8f4f4",
                    lt_grey_1    = "#C6C6C6",
                    lt_grey_2    = "#818183",
                    grey3        = "#E3E3E3",
                    dk_grey      = "#464647",
                    two_dots_1   = "gold1",
                    two_dots_2   = "blue2",
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
    return (reach_colors)

  reach_colors[cols]
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
reach_pal <- function(palette = "main", reverse = FALSE, color_ramp_palette = T, ...) {

  reach_palettes <- list(
    `main`       = reach_cols("main_grey", "main_red", "main_lt_grey", "main_beige"),
    `primary`    = reach_cols("main_grey", "main_red"),
    `secondary`  = reach_cols("main_lt_grey", "main_beige"),
    `two_dots`   = reach_cols("two_dots_1", "two_dots_2"),
    `red_main`   = reach_cols("red_main_1", "red_main_2", "red_main_3", "red_main_4", "red_main_5"),
    `red_alt`   = reach_cols("red_alt_1", "red_alt_2", "red_alt_3", "red_alt_4", "red_alt_5"),
    `discrete_6` = reach_cols("dk_grey", "red_main_1", "main_beige", "red_main_2", "lt_grey_2", "red_4")
  )

  pal <- reach_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  if (rlang::is_true(color_ramp_palette)) pal <- grDevices::colorRampPalette(pal, ...)

  return(pal)
}







#' Color scale constructor for reach colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @return A color scale for ggplot
#'
#' @export
scale_color_reach  <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- reach_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("reach_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for reach colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' @return  A fill scale for ggplot
#'
#' @export
scale_fill_reach <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- reach_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("reach_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}





#' @title Simple bar graph
#'
#' @param .tbl A tibble
#' @param x The x variable
#' @param y The y variable
#' @param f The fill variable
#' @param c The color variable
#' @param facet The facet variable
#' @param pal The palette name from `impactR::reach_pal()`
#' @param reverse Should color be reversed? Default to FALSE
#' @param discrete Should color pal be used as a discrete scale? Default to TRUE
#' @param alpha Color's transparency
#' @param width Bar's width
#' @param scale_percent Should axis labels be scaled to nice percent? Default to FALSE
#' @param position Either 'fill' or 'dodge'
#' @param dodge_width If position is 'dodge', this is the dodge width
#' @param fill_name Legend title
#' @param fill_levels Levels to order fill labels
#' @param fill_labels Fill labels
#' @param theme Theme name. Default is NULL to reach theme. Other values are "simple", "void", "bw", "minimal"
#' @param title Title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#'
#' @export
reach_flip_bar <- function(.tbl,
                           x,
                           y             = NULL,
                           f             = NULL,
                           c             = NULL,
                           facet         = NULL,
                           pal           = NULL,
                           reverse       = F,
                           discrete      = T,
                           alpha         = 0.7,
                           width         = 0.3,
                           scale_percent = F,
                           position      = "fill",
                           dodge_width   = 0.5,
                           fill_name     = NULL,
                           fill_levels   = NULL,
                           fill_labels   = NULL,
                           theme         = NULL,
                           title         = NULL,
                           xlab          = NULL,
                           ylab          = NULL) {
  # Tidy eval over there
  # Main mapping
  if (rlang::is_null(fill_levels) & rlang::is_null(fill_labels)){
    g <- ggplot2::ggplot(
      .tbl,
      mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}, fill = {{ f }}, color = {{ c }}))
  } else if (!rlang::is_null(fill_levels) & !rlang::is_null(fill_labels)) {
    g <- ggplot2::ggplot(
      .tbl,
      mapping = ggplot2::aes(x = {{ x }},
                             y = {{ y }},
                             fill = factor({{ f }}, levels = fill_levels, labels = fill_labels),
                             color = factor({{ c }}, levels = fill_levels, labels = fill_labels)
      )
    )
  } else if (!rlang::is_null(fill_levels) & rlang::is_null(fill_labels)) {
    g <- ggplot2::ggplot(
      .tbl,
      mapping = ggplot2::aes(x = {{ x }},
                             y = {{ y }},
                             fill = factor({{ f }}, levels = fill_levels),
                             color = factor({{ c }}, levels = fill_levels)
      )
    )
  }

  # Should the graph use position_fill?
  if (position == "fill"){
    g <- g + ggplot2::geom_col(
      alpha    = alpha,
      width    = width,
      position = ggplot2::position_fill()
    )
  } else if (position == "dodge"){
    g <- g + ggplot2::geom_col(
      alpha    = alpha,
      width    = width,
      position = ggplot2::position_dodge(dodge_width)
    )
  } else{
    g <- g + ggplot2::geom_col(
      alpha = alpha,
      width = width)
  }

  # Should the y-flipped axis be scaled to percent?
  if (rlang::is_true(scale_percent)) {
    g <- g + ggplot2::scale_y_continuous(
      labels         = scales::label_percent(
        accuracy     = 1,
        decimal.mark = ",",
        suffix       = " %")
    )
  }

  # Should we use a facet grid? (good for two population and many regions, e.g.)
  if (!is.null(facet)) {
    g <- g + ggplot2::facet_wrap({{ facet }})
  }

  # Because a text legend should always be horizontal, especially for an horizontal bar graph
  g <- g + ggplot2::coord_flip()

  # Which reach palette should we use?
  if (!rlang::is_null(pal)) {
    if (!is.null(fill_name) & !is.null(fill_labels)) {
      g <- g + scale_fill_reach(pal, name = fill_name, labels = fill_labels, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(pal, name = fill_name, labels = fill_labels, reverse = reverse, discrete = discrete)
    } else if (!is.null(fill_name)) {
      g <- g + scale_fill_reach(pal, name = fill_name, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(pal, name = fill_name, reverse = reverse, discrete = discrete)
    } else if (!is.null(fill_labels)) {
      g <- g + scale_fill_reach(pal, labels = fill_labels, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(pal, name = fill_name, reverse = reverse, discrete = discrete)
    } else if (is.null(fill_name) & is.null(fill_labels)) {
      g <- g + scale_fill_reach(pal, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(pal, reverse = reverse, discrete = discrete)
    }
  } else {
    if (!is.null(fill_name) & !is.null(fill_labels)) {
      g <- g + scale_fill_reach(name = fill_name, labels = fill_labels, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(name = fill_name, labels = fill_labels, reverse = reverse, discrete = discrete)
    } else if (!is.null(fill_name)) {
      g <- g + scale_fill_reach(name = fill_name, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(name = fill_name, reverse = reverse, discrete = discrete)
    } else if (!is.null(fill_labels)) {
      g <- g + scale_fill_reach(labels = fill_labels, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(name = fill_name, reverse = reverse, discrete = discrete)
    } else if (is.null(fill_name) & is.null(fill_labels)) {
      g <- g + scale_fill_reach(reverse = reverse, discrete = discrete)
      g <- g + scale_fill_reach(reverse = reverse, discrete = discrete)
    }
  }

  # Use impactR::theme_reach()
  if (rlang::is_null(theme)) {
    g <- g + theme_reach()
  } else if (theme == "simple") {
    g <- g + theme_flip_simple_reach()
  } else if (theme == "void") {
    g <- g + ggplot2::theme_void()
  } else if (theme == "bw") {
    g <- g + ggplot2::theme_bw()
  } else if (theme == "minimal") {
    g <- g + ggplot2::theme_minimal()
  } else {
    g <- g
  }

  # Add titles and labs
  if (!rlang::is_null(title)) g  <- g + ggplot2::ggtitle(title)
  if (!rlang::is_null(xlab))  g  <- g + ggplot2::xlab(xlab)
  if (!rlang::is_null(ylab))  g  <- g + ggplot2::ylab(ylab)


  return(g)
}



#' @title Simple bar graph
#'
#' @param .tbl A tibble
#' @param x The x variable
#' @param y The y variable
#' @param f The fill variable
#' @param c The color variable
#' @param facet The facet variable
#' @param pal The palette name from `impactR::reach_pal()`
#' @param reverse Should color be reversed? Default to FALSE
#' @param discrete Should color pal be used as a discrete scale? Default to TRUE
#' @param alpha Color's transparency
#' @param width Bar's width
#' @param scale_percent Should axis labels be scaled to nice percent? Default to FALSE
#' @param position Either 'fill' or 'dodge'
#' @param dodge_width If position is 'dodge', this is the dodge width
#' @param fill_name Legend title
#' @param fill_levels Levels to order fill labels
#' @param fill_labels Fill labels
#' @param theme Theme name. Default is NULL to reach theme. Other values are "simple", "void", "bw", "minimal"
#' @param title Title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#'
#' @export
reach_hist_bar <- function(.tbl,
                           x,
                           y             = NULL,
                           f             = NULL,
                           c             = NULL,
                           facet         = NULL,
                           pal           = NULL,
                           reverse       = F,
                           discrete      = T,
                           alpha         = 0.7,
                           width         = 0.3,
                           scale_percent = F,
                           position      = "fill",
                           dodge_width   = 0.5,
                           fill_name     = NULL,
                           fill_levels   = NULL,
                           fill_labels   = NULL,
                           theme         = NULL,
                           title         = NULL,
                           xlab          = NULL,
                           ylab          = NULL) {
  # Tidy eval over there
  # Main mapping
  if (rlang::is_null(fill_levels) & rlang::is_null(fill_labels)){
    g <- ggplot2::ggplot(
      .tbl,
      mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}, fill = {{ f }}, color = {{ c }}))
  } else if (!rlang::is_null(fill_levels) & !rlang::is_null(fill_labels)) {
    g <- ggplot2::ggplot(
      .tbl,
      mapping = ggplot2::aes(x = {{ x }},
                             y = {{ y }},
                             fill = factor({{ f }}, levels = fill_levels, labels = fill_labels),
                             color = factor({{ c }}, levels = fill_levels, labels = fill_labels)
      )
    )
  } else if (!rlang::is_null(fill_levels) & rlang::is_null(fill_labels)) {
    g <- ggplot2::ggplot(
      .tbl,
      mapping = ggplot2::aes(x = {{ x }},
                             y = {{ y }},
                             fill = factor({{ f }}, levels = fill_levels),
                             color = factor({{ c }}, levels = fill_levels)
      )
    )
  }

  # Should the graph use position_fill?
  if (position == "fill"){
    g <- g + ggplot2::geom_col(
      alpha    = alpha,
      width    = width,
      position = ggplot2::position_fill()
    )
  } else if (position == "dodge"){
    g <- g + ggplot2::geom_col(
      alpha    = alpha,
      width    = width,
      position = ggplot2::position_dodge(dodge_width)
    )
  } else{
    g <- g + ggplot2::geom_col(
      alpha = alpha,
      width = width)
  }

  # Should the y-flipped axis be scaled to percent?
  if (rlang::is_true(scale_percent)) {
    g <- g + ggplot2::scale_y_continuous(
      labels         = scales::label_percent(
        accuracy     = 1,
        decimal.mark = ",",
        suffix       = " %")
    )
  }

  # Should we use a facet grid? (good for two population and many regions, e.g.)
  if (!is.null(facet)) {
    g <- g + ggplot2::facet_wrap({{ facet }})
  }

  # Because a text legend should always be horizontal, especially for an horizontal bar graph
  g <- g + ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45))

  # Which reach palette should we use?
  if (!rlang::is_null(pal)) {
    if (!is.null(fill_name) & !is.null(fill_labels)) {
      g <- g + scale_fill_reach(pal, name = fill_name, labels = fill_labels, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(pal, name = fill_name, labels = fill_labels, reverse = reverse, discrete = discrete)
    } else if (!is.null(fill_name)) {
      g <- g + scale_fill_reach(pal, name = fill_name, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(pal, name = fill_name, reverse = reverse, discrete = discrete)
    } else if (!is.null(fill_labels)) {
      g <- g + scale_fill_reach(pal, labels = fill_labels, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(pal, name = fill_name, reverse = reverse, discrete = discrete)
    } else if (is.null(fill_name) & is.null(fill_labels)) {
      g <- g + scale_fill_reach(pal, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(pal, reverse = reverse, discrete = discrete)
    }
  } else {
    if (!is.null(fill_name) & !is.null(fill_labels)) {
      g <- g + scale_fill_reach(name = fill_name, labels = fill_labels, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(name = fill_name, labels = fill_labels, reverse = reverse, discrete = discrete)
    } else if (!is.null(fill_name)) {
      g <- g + scale_fill_reach(name = fill_name, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(name = fill_name, reverse = reverse, discrete = discrete)
    } else if (!is.null(fill_labels)) {
      g <- g + scale_fill_reach(labels = fill_labels, reverse = reverse, discrete = discrete)
      g <- g + scale_color_reach(name = fill_name, reverse = reverse, discrete = discrete)
    } else if (is.null(fill_name) & is.null(fill_labels)) {
      g <- g + scale_fill_reach(reverse = reverse, discrete = discrete)
      g <- g + scale_fill_reach(reverse = reverse, discrete = discrete)
    }
  }

  # Use impactR::theme_reach()
  if (rlang::is_null(theme)) {
    g <- g + theme_reach()
  } else if (theme == "simple") {
    g <- g + theme_hist_simple_reach()
  } else if (theme == "void") {
    g <- g + ggplot2::theme_void()
  } else if (theme == "bw") {
    g <- g + ggplot2::theme_bw()
  } else if (theme == "minimal") {
    g <- g + ggplot2::theme_minimal()
  } else {
    g <- g
  }

  # Add titles and labs
  if (!rlang::is_null(title)) g  <- g + ggplot2::ggtitle(title)
  if (!rlang::is_null(xlab))  g  <- g + ggplot2::xlab(xlab)
  if (!rlang::is_null(ylab))  g  <- g + ggplot2::ylab(ylab)


  return(g)
}


