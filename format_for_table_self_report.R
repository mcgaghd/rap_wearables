paste_quartiles <- function(quartiles, dp = 0) {
  quartiles <- round(quartiles, dp)
  return(paste0(quartiles[2], " (", quartiles[1], "-", quartiles[3], ")"))
}


get_table_numbers <- function(dat_loc, dat) {
  # NUMBERS IN GROUP--------------------------------------------
  n <- format(nrow(dat_loc), big.mark = ",") # format command is to format with 1000s comma
  percent <- round(100 * nrow(dat_loc) / nrow(dat), 1) # calculate percentage
  
  # QUANTILES IN GROUP--------------------------------------------------------
  met_quartiles <- quantile(dat_loc$summed_MET_minutes, c(0.25, 0.5, 0.75), na.rm = TRUE)
  
  # OUTPUT AS ROW OF DATA FRAME--------------------------------------------------------
  table1_numbers <- data.frame("N (Percent)" = paste0(n, " (", percent, ")"),
                               "Summed MET Minutes" = paste_quartiles(met_quartiles, 1))
  return(table1_numbers)
}

