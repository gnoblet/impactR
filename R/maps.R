#' @title Some  REACH basic `tmap` options
#'
#' @param check_and_fix Param check.and.fix from `tmap::tmap_options()`
#' @param fontfamily Param fontfamily from `tmap::tmap_options()`
#' @param fontface Param fontface from `tmap::tmap_options()`
#' @param legend_title_size Param legend.title.size from `tmap::tmap_options()`
#' @param legend_text_size Param legend.text.size from `tmap::tmap_options()`
#' @param legend_text_fontface Param legend legend.text.fontface from `tmap::tmap_options()`
#' @param legend_width Param legend.width from `tmap::tmap_options()`
#' @param legend_position Param legend.position from `tmap::tmap_options()`
#' @param legend_frame Param legend.frame from `tmap::tmap_options()`
#' @param output_dpi Param output.dpi from `tmap::tmap_options()`
#' @param frame Frame colors using `impactR::reach_cols()`
#' @param inner_margins Param inner.margins from `tmap::tmap_options()`
#' @param outer_margins Param outer.margins from `tmap::tmap_options()`
#' @param asp Param asp from `tmap::tmap_options()`
#' @param ... Other parameters to pass to `tmap::tmap_options()`
#'
#' @description Give some reach colors and font for maps
#'
#' @return Load tmap options globally
#'
#' @export
tmap_options_reach <- function(check_and_fix = T,
                               fontfamily = "Leelawadee UI",
                               fontface = 1,
                               legend_title_size = 0.9,
                               legend_text_size = 0.8,
                               legend_text_fontface = 1,
                               legend_width = 0.9,
                               legend_position = c("right", 0.14),
                               legend_frame = impactR::reach_colors()$reach_dk_grey,
                               output_dpi = 500,
                               frame = impactR::reach_cols("main_grey"),
                               inner_margins = c(0,0,0,0),
                               outer_margins= c(0,0,0,0),
                               asp = 0,
                               ...){
  tmap::tmap_options(check.and.fix = check_and_fix,
                     fontfamily = fontfamily,
                     fontface = fontface,
                     legend.title.size = legend_title_size,
                     legend.text.size = legend_text_size,
                     legend.text.fontface = legend_text_fontface,
                     legend.width =  legend_width,
                     legend.position = legend_position,
                     legend.frame = legend_frame,
                     output.dpi = output_dpi,
                     frame = frame,
                     inner.margins = inner_margins,
                     outer.margins = outer_margins,
                     asp = 0,
                     ...)
}


#' @title Bbbox buffer
#'
#' @param sf_obj A `sf` object
#' @param buffer A buffer, either one value or a vector of 4 values (left, bottom, right, top). Default to 0.
#'
#' @return A bbox with a buffer
#'
#' @export
buffer_bbox <- function(sf_obj, buffer = 0){

  if (!(length(buffer) %in% c(1,4)) | !is.numeric(buffer)) stop("Please provide a numeric buffer of length 1 or 4.")

  bbox <- sf::st_bbox(sf_obj)
  xrange <- bbox$xmax - bbox$xmin # range of x values
  yrange <- bbox$ymax - bbox$ymin # range of y values


  bbox_with_buffer <- if (length(buffer) == 1) {
    c(
      bbox[1] - (buffer * xrange), # xmin - left
      bbox[2] - (buffer * yrange), # ymin - bottom
      bbox[3] + (buffer * xrange), # xmax - right
      bbox[4] + (buffer * yrange) # ymax - top
    )
  } else if (length(buffer) == 4) {
    c(
      bbox[1] - (buffer[1] * xrange), # xmin - left
      bbox[2] - (buffer[2] * yrange), # ymin - bottom
      bbox[3] + (buffer[3] * xrange), # xmax - right
      bbox[4] + (buffer[4] * yrange) # ymax - top
    )
  } else {
    print("Missed something while writing the funtion.")
  }

}

