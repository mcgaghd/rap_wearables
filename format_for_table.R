paste_quartiles <- function(quartiles, dp = 0) {
  quartiles <- round_dp(dp)(quartiles)
  return(paste0(quartiles[2], " (", quartiles[1], "-", quartiles[3], ")"))
}

get_table_numbers <- function(dat_loc, dat){
  # NUMBERS IN GROUP--------------------------------------------
  n <- format(nrow(dat_loc), big.mark = ",") # format command is to format with 1000s comma
  percent <- round_dp(1)(100 * nrow(dat_loc) / nrow(dat)) # note round_dp is a function factory, see useful_functions/rounding_functions.R
  
  # QUANTILES IN GROUP--------------------------------------------------------
  steps_quartiles <- quantile(dat_loc$med_steps, c(0.25, 0.5, 0.75))
  acc_quartiles <- quantile(dat_loc$overall_activity, c(0.25, 0.5, 0.75))
  mvpa_quartiles <- quantile(dat_loc$MVPA, c(0.25, 0.5, 0.75))
  sed_quartiles <- quantile(dat_loc$SB, c(0.25, 0.5, 0.75))
  sleep_quartiles <- quantile(dat_loc$sleep, c(0.25, 0.5, 0.75))
  lpa_quartiles <- quantile(dat_loc$LIPA, c(0.25, 0.5, 0.75))
  
  ##mean and SD
  # MEAN AND SD IN GROUP-------------------------------------------------------
  steps_mean <- round(mean(dat_loc$med_steps, na.rm = TRUE), 1)
  steps_sd <- round(sd(dat_loc$med_steps, na.rm = TRUE), 1)
  
  acc_mean <- round(mean(dat_loc$overall_activity, na.rm = TRUE), 1)
  acc_sd <- round(sd(dat_loc$overall_activity, na.rm = TRUE), 1)
  
  mvpa_mean <- round(mean(dat_loc$MVPA, na.rm = TRUE), 1)
  mvpa_sd <- round(sd(dat_loc$MVPA, na.rm = TRUE), 1)
  
  sed_mean <- round(mean(dat_loc$SB, na.rm = TRUE), 1)
  sed_sd <- round(sd(dat_loc$SB, na.rm = TRUE), 1)
  
  sleep_mean <- round(mean(dat_loc$sleep, na.rm = TRUE), 1)
  sleep_sd <- round(sd(dat_loc$sleep, na.rm = TRUE), 1)
  
  lpa_mean <- round(mean(dat_loc$LIPA, na.rm = TRUE), 1)
  lpa_sd <- round(sd(dat_loc$LIPA, na.rm = TRUE), 1)
  
  # OUTPUT AS ROW OF DATA FRAME--------------------------------------------------------
  table1_numbers <- data.frame(
    "N (Percent)" = paste0(n, " (", percent, ")"),
    "Daily Steps" = paste_quartiles(steps_quartiles),
    "Daily Steps Mean (SD)" = paste0(steps_mean, " (", steps_sd, ")"),
    "Overall Acceleration (mg)" = paste_quartiles(acc_quartiles, 1),
    "Overall Acceleration Mean (SD)" = paste0(acc_mean, " (", acc_sd, ")"),
    "MVPA (minutes/day)" = paste_quartiles(mvpa_quartiles),
    "MVPA Mean (SD)" = paste0(mvpa_mean, " (", mvpa_sd, ")"),
    "Sleep (minutes/day)" = paste_quartiles(sleep_quartiles),
    "Sleep Mean (SD)" = paste0(sleep_mean, " (", sleep_sd, ")"),
    "Sedentary (minutes/day)" = paste_quartiles(sed_quartiles),
    "Sedentary Mean (SD)" = paste0(sed_mean, " (", sed_sd, ")"),
    "Light (minutes/day)" = paste_quartiles(lpa_quartiles),
    "Light Mean (SD)" = paste0(lpa_mean, " (", lpa_sd, ")")
  )
  
  return(table1_numbers)
} 

