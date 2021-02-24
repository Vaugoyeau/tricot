
#' Creation of knitting image
#'
#' @param img Magick image
#' @param gs Grid size calculated from grid_size()
#' @inheritParams dplyr::arrange 
#' @inheritParams magick::image_raster
#' @inheritParams magick::image_scale
#' @inheritParams ggplot2::ggplot
#' @inheritParams ggplot2::aes
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::scale_color_manual
#' @inheritParams dplyr::distinct
#' @inheritParams ggplot2::theme
#' @inheritParams ggplot2::xlab
#' @inheritParams ggplot2::ylab
#'
#' @return
#' Knitting image
#' @export
#'
#' @examples
#' 
#' tricot::knitting_image(
#'   tricot::image_load(
#'     here::here("img", "Montagne.pdf")
#'     ), 
#'   tricot::grid_size(
#'     20, 
#'     30, 
#'     tricot::square_size(
#'       35,
#'       26
#'     )
#'     )
#'   )
#' 
#' tricot::knitting_image(
#'   tricot::image_load(
#'     here::here("img", "Monstre.tif")
#'     ), 
#'   tricot::grid_size(
#'     30, 
#'     30, 
#'     tricot::square_size(
#'       35,
#'       26
#'     )
#'     )
#'   )
#' 
#' tricot::knitting_image(
#'   tricot::image_load(
#'     here::here("img", "Pingouin.png")
#'     ), 
#'   tricot::grid_size(
#'     20, 
#'     20, 
#'     tricot::square_size(
#'       35,
#'       26
#'     )
#'     )
#'   )
#' 
#' tricot::knitting_image(
#'   tricot::image_load(
#'     here::here("img", "PeintureLaetitia.jpg")
#'     ), 
#'   tricot::grid_size(
#'     30, 
#'     20, 
#'     tricot::square_size(
#'       35,
#'       26
#'     )
#'     )
#'   )
#' 
knitting_image <- function(img, gs) {
  
  if(class(img) != "magick-image") {stop("img must be magick image")}
  if (!is.numeric(gs)) {stop("gs should be numeric")}
  if (length(gs) != 2) {stop("gs should have only two values")}

  raster_image <- 
    dplyr::arrange(
      magick::image_raster(
        magick::image_scale(
          img, 
          glue::glue('{gs["grid.width"]}x{gs["grid.height"]}!'))
        ), 
      col
      )
    
    
  kimage <- ggplot2::ggplot(raster_image) + 
    ggplot2::aes(x = x, y = max(y) + 1 - y, color = col) + 
    ggplot2::geom_point(shape = 15) + 
    ggplot2::scale_color_manual(values = dplyr::distinct(raster_image, col)$col) + 
    ggplot2::theme(
      legend.position = "none", 
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "black"),
      panel.grid.minor = ggplot2::element_line(colour = "grey")
    ) +
    ggplot2::xlab("maille") +
    ggplot2::ylab("rang")
    
  return(kimage)
  
}

