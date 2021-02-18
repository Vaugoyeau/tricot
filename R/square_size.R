
#' Square of size
#'
#' @param rg Row number to have 10 cm
#' @param m Stitch number to have 10 cm
#'
#' @return
#' Square size to do grid on image
#' @export
#'
#' @examples
#' tricot::square_size(31,24)
#' tricot::square_size(35,26)
square_size <- function(rg, m) {
  if (!is.numeric(rg)) {stop("rg should be numeric")}
  if (length(rg) != 1) {stop("rg should have only one value")}
  if (!is.numeric(m)) {stop("m should be numeric")}
  if (length(m) != 1) {stop("m should have only one value")}
  
  c(
    height = round(10/rg, 3),
    width = round(10/m, 3)
  )
}

