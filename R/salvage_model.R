#' Salvage model
#'
#' Zero-inflated model of salvage as function of Freeport flow, exports, and fish length.
#'
#' @md
#' @param run         Chinook Salmon run: fall, late_fall, winter
#' @param facility    Water export facility: CVP, SWP, both
#'
#' @export
#'

salvage_model <- function(run = "winter", facility = "both"){
  ss <- sac_salvage[sac_salvage[[run]] == 1,]
  if(facility == "SWP"){
    sm <- zeroinfl(sal_swp ~ sac_in_z + SWP_z + length_z + offset(log_released)| sac_in_z + SWP_z + length_z, data = ss, dist = "negbin", EM = TRUE)
  } else if(facility == "CVP"){
    sm <- zeroinfl(sal_cvp ~ sac_in_z + CVP_z + length_z + offset(log_released)| sac_in_z + CVP_z + length_z, data = ss, dist = "negbin", EM = TRUE)
  } else {
    sm <- zeroinfl(sal_tot ~ sac_in_z + exports_z + length_z + offset(log_released)| sac_in_z + exports_z + length_z, data = ss, dist = "negbin", EM = TRUE)
  }
  return(sm)
}

