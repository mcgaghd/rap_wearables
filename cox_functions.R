library(survival)

# Calculate confidence interval given Hazard ratio and Standard Error
calculate_CI <- function(HR, SE, upper) {
  Z <- qnorm(0.975)
  coef <- log(HR)
  
  if (upper) {
    return(exp(coef + Z * SE))
  }
  else {
    return(exp(coef - Z * SE))
  }
}

# Standardise column of data
standardise_column <- function(column, div='sd', verbose=FALSE) {
  if (div == 'sd') {
    mean_column <- mean(column)
    std_column <- sd(column)
    standardized_column <- (column - mean_column) / std_column
    
    if (verbose) {
      cat("Mean of original column: ", mean_column, "\n")
      cat("Standard deviation of original column: ", std_column, "\n")
    }
  }
  else {
    standardized_column <- column / div
    
    if (verbose) {
      cat("Standardising by dividing exposure by", div, "\n")
    }
  }
  
  return(standardized_column)
}

# Run Cox analysis extracting useful properties for association
coxph_analysis <- function(datafile, analysis_name, covariates, exposure_var, 
                           outcome_var, standardise_method = "sd",
                           calculate_chi_squared = FALSE) {
  if (is.character(datafile)) {
    dat <- fread(datafile, data.table = FALSE)
  } else {
    dat <- datafile
  }
  
  dat$standardized_exposure <- standardise_column(dat[[exposure_var]], standardise_method, FALSE)
  
  model <- coxph(as.formula(paste("Surv(age_entry_days, age_exit_days, ", outcome_var, ") ~ ", 
                                  paste(c('standardized_exposure', covariates), collapse = "+"))), 
                 data = dat)
  
  hazard_ratio <- summary(model)$coefficients['standardized_exposure', 2]
  standard_error <- summary(model)$coefficients['standardized_exposure', 3]
  ci_low <- calculate_CI(hazard_ratio, standard_error, FALSE)
  ci_high <- calculate_CI(hazard_ratio, standard_error, TRUE)
  
  if (calculate_chi_squared) {
    if (length(covariates) == 0) {
      model_without_primary <- coxph(Surv(age_entry_days, age_exit_days, RA_incident) ~ 1, 
                                     data = dat)
    } else {
      model_without_primary <- coxph(as.formula(paste("Surv(age_entry_days, age_exit_days, ", outcome_var, ") ~", 
                                                      paste(covariates, collapse = "+"))), 
                                     data = dat)
    }
    
    chi_squared_value = round(lmtest::lrtest(model_without_primary, model)$Chisq[2], 1)
  }
  
  n <- nrow(dat)
  cases <- sum(dat[[outcome_var]])
  
  power <- calculate_power(model)
  
  result_data <- data.frame(
    Variable = analysis_name,
    HR = hazard_ratio,
    SE = standard_error,
    CI_Low = ci_low,
    CI_High = ci_high,
    N_cases = paste0(n, " (", cases,")"),
    Power = n,
    ChiSquared = ifelse(calculate_chi_squared, chi_squared_value, NA)
  )
  
  return(result_data)
}

# Function to process multiple datasets and perform Cox Proportional Hazards analysis
multiple_cox_analyses <- function(dataset_paths, covariates, exposure_var, outcome_var, standardise_method = "sd") {
  results_list <- list()
  
  for (analysis_name in names(dataset_paths)) {
    dataset_path <- dataset_paths[[analysis_name]]
    result <- coxph_analysis(dataset_path, analysis_name, covariates, exposure_var, outcome_var, standardise_method)
    results_list <- append(results_list, list(result))
  }
  
  final_results <- do.call(rbind, results_list)
  return(final_results)
}