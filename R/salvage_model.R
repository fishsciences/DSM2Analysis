#' Salvage model
#'
#' Zero-inflated model of salvage as function of Freeport flow, exports, and fish length.
#'
#'
#' @md
#' @param facility    Water export facility: CVP, SWP, both
#'
#' @export
#'

salvage_model <- function(facility){
  ss <- sac_salvage[sac_salvage$winter == 1,] # fit model based on winter-run observations
  if(facility == "SWP"){
    sm <- pscl::zeroinfl(sal_swp ~ sac_in_z + SWP_z + length_z + offset(log_released)| sac_in_z + SWP_z + length_z, data = ss, dist = "negbin")

  } else if(facility == "CVP"){

    sm <- pscl::zeroinfl(sal_cvp ~ sac_in_z + CVP_z + length_z + offset(log_released)| sac_in_z + CVP_z + length_z, data = ss, dist = "negbin")

  } else {
    sm <- pscl::zeroinfl(sal_tot ~ sac_in_z + exports_z + length_z + offset(log_released)| sac_in_z + exports_z + length_z, data = ss, dist = "negbin")

  }
  return(sm)
}

