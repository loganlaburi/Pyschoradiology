library(dplyr)
library(openxlsx)
library(lm.beta)
library(broom)

input_file <- 
output_file

data <- read.xlsx(input_file)

# Define variable groups for the steps
control_vars <- c()
step2_vars <- c()

# Dependent variables
dependent_vars <- c()

# Create Excel workbook
wb <- createWorkbook()

for (dv in dependent_vars) {
  all_vars <- c(dv, control_vars, step2_vars)
    missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    warning(paste("Skipping", dv, "- missing variables:", paste(missing_vars, collapse = ", ")))
    next  
  }
  
  dv_data <- data[, all_vars, drop = FALSE]
  formulas <- list(
    Step1 = as.formula(paste(dv, "~", paste(control_vars, collapse = " + "))),
    Step2 = as.formula(paste(dv, "~", paste(c(control_vars, step2_vars), collapse = " + ")))
  )
  
  model_summaries <- list()
  model_coefs <- list()
  
  for (step in names(formulas)) {
    model <- tryCatch(
      lm(formulas[[step]], data = dv_data, na.action = na.exclude),
      error = function(e) {
        warning(paste("Model failed at", step, "for", dv, "| Error:", e$message))
        return(NULL)
      }
    )
    
    if (is.null(model) || df.residual(model) <= 0) {
      warning(paste("Insufficient data for", step, "model of", dv))
      next
    }
    
    std_model <- lm.beta(model)
    std_betas <- coef(std_model)
    
    conf <- tryCatch(
      confint(model),
      error = function(e) data.frame(conf.low = rep(NA, length(coef(model))), conf.high = rep(NA, length(coef(model))))
    )
    
    conf <- as.data.frame(conf)
    names(conf) <- c("conf.low", "conf.high")
    
    tidy_model <- tidy(model) %>%
      mutate(
        conf.low = conf$conf.low,
        conf.high = conf$conf.high,
        Beta = std_betas[term],
        `95 % CI [LL, UL]` = ifelse(!is.na(conf.low),
                                    paste0("[", sprintf("%.3f", conf.low), ", ", sprintf("%.3f", conf.high), "]"),
                                    ""),
        p = ifelse(p.value < .001, "< 0.001", sprintf("%.3f", p.value)),
        Beta = ifelse(is.na(Beta), "", sprintf("%.3f", Beta)),
        Predictor = term
      ) %>%
      select(Predictor, `95 % CI [LL, UL]`, p, Beta)
    
    model_summary <- summary(model)
    f_stats <- model_summary$fstatistic
    
    if (is.null(f_stats)) {
      f_stats <- c(NA, NA, NA)
    }
    
    model_stats <- data.frame(
      Step = step,
      R_squared = model_summary$r.squared,
      Adjusted_R_squared = model_summary$adj.r.squared,
      F_statistic = f_stats[1],
      df1 = f_stats[2],
      df2 = f_stats[3],
      Model_p_value = ifelse(is.na(f_stats[1]), NA,
                             pf(f_stats[1], f_stats[2], f_stats[3], lower.tail = FALSE))
    )
    
    model_summaries[[step]] <- model_stats
    model_coefs[[step]] <- tidy_model
  }

  if (length(model_coefs) == 0) next
  
  # Clean sheet name (Excel doesn’t like special characters)
  sheet_name <- gsub("[/:*?\"<>|]", "_", dv)
  addWorksheet(wb, sheetName = sheet_name)
  
  row_start <- 1
  for (step in names(model_coefs)) {
    writeData(wb, sheet = sheet_name, x = paste("Model:", step), startRow = row_start, colNames = FALSE)
    writeData(wb, sheet = sheet_name, x = model_coefs[[step]], startRow = row_start + 1)
    writeData(wb, sheet = sheet_name, x = model_summaries[[step]], startRow = row_start + nrow(model_coefs[[step]]) + 3)
    row_start <- row_start + nrow(model_coefs[[step]]) + 8
  }
}

# Save workbook
saveWorkbook(wb, output_file, overwrite = TRUE)
cat("Two-step hierarchical regression Excel saved at:", output_file, "\n")
