#' Proportion overlap
#'
#' Calculates proportion overlap in density distributions for two scenarios.
#'
#' @md
#' @param base,comp   Vector of flow, velocity, or stage values for baseline and comparison scenarios
#' @param mn,mx       Minimum and maximum values across all comparisons in the analysis
#'
#' @export
#' @examples
#'

proportion_overlap <- function(base, comp, mn, mx){
  if (length(base) != length(comp)) stop("base and comp are not same length")
  bd = density(base, from = mn, to = mx) # bd = baseline density
  cd = density(comp, from = mn, to = mx) # cd = comparison density
  dis = MESS::auc(bd[["x"]], abs(cd[["y"]] - bd[["y"]]))/(MESS::auc(bd[["x"]], bd[["y"]]) + MESS::auc(cd[["x"]], cd[["y"]])) # dis = 0 is completely overlapping; dis = 1 is no overlap
  return(list(po = 1 - dis,
              x = bd[["x"]], # base and comp have same x
              y.base = bd[["y"]],
              y.comp = cd[["y"]]))
}

