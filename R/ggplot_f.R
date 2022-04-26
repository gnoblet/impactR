
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

  rlang::check_installed("ggplot2", reason = "Package \"ggplot2\" needed for `scale_color_reach()` to work. Please install it.")

  pal <- reach_pal(palette = palette, reverse = reverse, color_ramp_palette = T)

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

  rlang::check_installed("ggplot2", reason = "Package \"ggplot2\" needed for `scale_fill_reach()` to work. Please install it.")

  pal <- reach_pal(palette = palette, reverse = reverse, , color_ramp_palette = T)

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


  rlang::check_installed("ggplot2", reason = "Package \"ggplot2\" needed for `reach_flip_bar()` to work. Please install it.")

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

  rlang::check_installed("ggplot2", reason = "Package \"ggplot2\" needed for `reach_hist_bar()` to work. Please install it.")

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
