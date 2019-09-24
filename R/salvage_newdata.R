#' Salvage new data
#'
#' Prepare flow_df data frame to be used for salvage predictions
#'
#' @import dplyr
#'
#' @md
#' @param flow_df          Data frame with modeled flow input
#' @param annual_passage   Arbitrary annual passage used salvage predictions
#'
#' @export
#'

salvage_newdata <- function(flow_df, annual_passage){
  ss <- sac_salvage # in sac_salvage, the CVP_z (and related '_z' columns) were calculated based on whole dataset, not just winter run set
  nd <- flow_df %>%
    left_join(winter_run_size) %>%
    mutate(sac_in_z = z_score(Freeport, mean(ss$sac_in, na.rm = TRUE), sd(ss$sac_in, na.rm = TRUE)),
           CVP_z = z_score(CVP, mean(ss$CVP, na.rm = TRUE), sd(ss$CVP, na.rm = TRUE)),
           SWP_z = z_score(SWP, mean(ss$SWP, na.rm = TRUE), sd(ss$SWP, na.rm = TRUE)),
           exports_z = z_score(Exports, mean(ss$exports, na.rm = TRUE), sd(ss$exports, na.rm = TRUE)),
           length_z = z_score(Length, mean(ss$length, na.rm = TRUE), sd(ss$length, na.rm = TRUE)))
  nd$released <- nd$Winter * annual_passage # column with run name is entry timing
  nd$log_released <- log(nd$released)
  return(nd)
}

