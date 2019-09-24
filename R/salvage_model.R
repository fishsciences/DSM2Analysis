#' Salvage model
#'
#' Zero-inflated model of salvage as function of Freeport flow, exports, and fish length.
#'
#'
#' @md
#' @param run         Chinook Salmon run: Fall, LateFall, Winter
#' @param facility    Water export facility: CVP, SWP, both
#'
#' @export
#'

salvage_model <- function(run = "Winter", facility = "both"){
  ss_run <- c("Winter" = "winter", "Fall" = "fall", "LateFall" = "late_fall")
  ss <- sac_salvage[sac_salvage[[ss_run[[run]]]] == 1,]
  if(facility == "SWP"){
    sm <- pscl::zeroinfl(sal_swp ~ sac_in_z + SWP_z + length_z + offset(log_released)| sac_in_z + SWP_z + length_z, data = ss, dist = "negbin", EM = TRUE)
  } else if(facility == "CVP"){
    sm <- pscl::zeroinfl(sal_cvp ~ sac_in_z + CVP_z + length_z + offset(log_released)| sac_in_z + CVP_z + length_z, data = ss, dist = "negbin", EM = TRUE)
  } else {
    sm <- pscl::zeroinfl(sal_tot ~ sac_in_z + exports_z + length_z + offset(log_released)| sac_in_z + exports_z + length_z, data = ss, dist = "negbin", EM = TRUE)
  }
  return(sm)
}

