
#' Grid size
#'
#' @param h Expected height (cm) of knitting
#' @param w Expected width (cm) of knitting
#' @param ss Size square calculated from square_size()
#'
#' @return
#' Grid size to do grid on image
#' @export
#'
#' @examples
#' tricot::grid_size(20, 20, square_size(31,24)) 
#' tricot::grid_size(5, 5, square_size(35,26))
grid_size <- function(h, w, ss) {
  if (!is.numeric(h)) {stop("h should be numeric")}
  if (length(h) != 1) {stop("h should have only one value")}
  if (!is.numeric(w)) {stop("w should be numeric")}
  if (length(w) != 1) {stop("w should have only one value")}
  if (!is.numeric(ss)) {stop("gs should be numeric")}
  if (length(ss) != 2) {stop("gs should have only two values")}
  
  c(
    grid = ceiling(h/ss["height"]),
    grid = ceiling(w/ss["width"])
  )
}

