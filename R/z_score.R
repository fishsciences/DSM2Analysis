#' Z-score
#'
#' Calculate z-score
#'
#' @md
#' @param x           Numeric vector
#' @param x_mean      Mean of numeric vector (or mean of original data)
#' @param x_sd        Standard deviation of numeric vector (or sd of original data)
#'
#' @export
#' @examples
#' z_score(1:10, mean(1:10), sd(1:10))
#' z_score(1:10, 20, 5)

z_score <- function(x, x_mean, x_sd){
  (x - x_mean)/x_sd
}

