
#' Calculation of the wool quantity needed
#'
#' @param img_k ggplot image from knitting_image()
#' @param rg Row number to have 10 cm
#' @param m Stitch number to have 10 cm
#' @param p Weight (g) to make the test square of 10*10
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
#' Table with wool weight needed by color
#' @export
#'
#' @examples
#' 
#' tricot::wool_weigth_needed(
#'   tricot::knitting_image(
#'   tricot::image_load(
#'     "img/Montagne.pdf"
#'     ), 
#'   tricot::grid_size(
#'     20, 
#'     30, 
#'     tricot::square_size(
#'       35,
#'       26
#'     )
#'     )
#'   ),
#'   35,
#'   26,
#'   100
#' )
#' 
wool_weigth_needed <- function(img_k, rg, m, p) {
  
  if(sum(class(img_k) != "ggplot") == 0) {stop("img_k must be ggplot")}
  if (!is.numeric(rg)) {stop("rg should be numeric")}
  if (length(rg) != 1) {stop("rg should have only one value")}
  if (!is.numeric(m)) {stop("m should be numeric")}
  if (length(m) != 1) {stop("m should have only one value")}
  if (!is.numeric(p)) {stop("p should be numeric")}
  if (length(p) != 1) {stop("p should have only one value")}
  
  table <- dplyr::mutate(
        dplyr::count(
          img_k$data, 
          col
        ),
        "wool_needed (g)" = round(n * p / (rg * m), digits = 2)
      )
  
  table_couleur <-
    DT::formatStyle(
      DT::datatable(
        table,
        rownames = FALSE
        ),
      "col",
      backgroundColor = 
        DT::styleEqual(
          dplyr::distinct(table, col)$col,
          dplyr::distinct(table, col)$col
        )
    )
  
  return(table_couleur)
}

