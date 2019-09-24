#' Salvage prediction
#'
#' Predicted salvage based on salvage_newdata and salvage_model.
#'
#' @md
#' @param run              Chinook Salmon run: Fall, LateFall, Winter
#' @param facility         Water export facility: CVP, SWP, both
#' @param newdata          Data frame with input data needed for model predictions
#'
#' @export
#'

salvage_prediction <- function(run, facility, newdata){
  newdata$salvaged <- predict(object = salvage_model(run, facility),
                              newdata = newdata,
                              type = "response")
  return(newdata)
}


