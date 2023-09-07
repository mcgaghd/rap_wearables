hes_disease_lookup <- function(df_hes, df_participant, icd10_query=NA, icd9_query=NA, 
                               case_type="prevalent", col_name="status", 
                               participant_date_col="date_end_accel") {
  # Select rows when an icd10 or icd9 query is not NA, and df_hes column matches
  # the respective query search
  row_filter <- ((!is.na(icd10_query) & grepl(icd10_query, df_hes$diag_icd10)) |
                   (!is.na(icd9_query) & grepl(icd9_query, df_hes$diag_icd9)))
  
  df_hes_disease <- df_hes[row_filter, c("eid", "date_hes")]
  
  df_hes_first_disease <-
    aggregate(df_hes_disease$date_hes, list(df_hes_disease$eid), min)
  
  colnames(df_hes_first_disease) <- 
    c("eid", "date_first_disease")
  
  # Merge into main data frame
  df_participant <- merge(
    df_participant,
    df_hes_first_disease,
    by = "eid",
    all.x = TRUE,
    suffixes = c("", "dup")
  )
  
  if (case_type=="prevalent") {
    df_participant[[col_name]] <- 
      (!is.na(df_participant$date_first_disease) &
        (df_participant$date_first_disease <= dat[[participant_date_col]]))
  } else if (case_type=="incident") {
    df_participant[[col_name]] <-
      (!is.na(df_participant$date_first_disease) &
        (df_participant$date_first_disease > dat[[participant_date_col]]))
  } else if (case_type=="date") {
    df_participant[[col_name]] <- df_participant$date_first_disease
  } else {
    df_participant[[col_name]] <- !is.na(df_participant$date_first_disease)
  }
  
  df_participant[, c("eid", col_name)]
}

death_disease_lookup <- function(df_death, df_participant, icd10_query, 
                                 col_name="mortality") {
  df_death_disease <- 
    df_death[grepl(icd10_query, df_death$cause_icd10), c("eid", "cause_icd10")]
  
  # Drop rows for participants with multiple entries
  df_death_disease <- df_death_disease[!duplicated(df_death_disease$eid), ]
  
  # Merge into main data frame
  df_participant <- merge(
    df_participant,
    df_death_disease,
    by = "eid",
    all.x = TRUE,
    suffixes = c("", "dup")
  )
  
  df_participant[[col_name]] <- !is.na(df_participant$cause_icd10)
  
  df_participant[, c("eid", col_name)]
}