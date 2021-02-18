
#' Load image
#'
#' @param path Path to load image
#' @inheritParams magick::image_read 
#' @inheritParams stringr::str_detect
#'
#' @return
#' Magick image in R
#' @export
#'
#' @examples
#' tricot::image_load("img/PeintureLaetitia.jpg")
#' tricot::image_load("img/Montagne.pdf")
#' tricot::image_load("img/Pingouin.png")
#' tricot::image_load("img/Monstre.tif")
image_load <- function(path) {
  if (!file.exists(path)) {stop("path is incorrect")}
  
  if (sum(stringr::str_detect(path, c(".jpeg", ".jpg", ".png", ".tif"))) > 0) {magick::image_read(path)}
  else if (stringr::str_detect(path, ".pdf")) {magick::image_read_pdf(path, pages = 1)}
  else {stop("format is not supported")}
}

