library(readxl)
library(writexl)
library(dplyr)

data_numeric <- data %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.)))) %>%
  select(where(~ !all(is.na(.)))) 

vars <- colnames(data_numeric)
n <- length(vars)

cor_matrix <- matrix(NA, n, n)
p_matrix <- matrix(NA, n, n)
colnames(cor_matrix) <- rownames(cor_matrix) <- vars
colnames(p_matrix) <- rownames(p_matrix) <- vars

for (i in 1:n) {
  for (j in 1:n) {
    x <- data_numeric[[i]]
    y <- data_numeric[[j]]
    
    pairwise <- complete.cases(x, y)
    x_clean <- x[pairwise]
    y_clean <- y[pairwise]
    
    if (length(x_clean) > 2 &&
        length(y_clean) > 2 &&
        sd(x_clean) != 0 &&
        sd(y_clean) != 0) {
      
      result <- tryCatch({
        test <- cor.test(x_clean, y_clean, method = "pearson")
        cor_matrix[i, j] <- test$estimate
        p_matrix[i, j] <- test$p.value
      }, error = function(e) {
        # Do nothing, leave NA
        NULL
      })
    }
  }
}

cor_matrix <- round(cor_matrix, 3)
p_matrix <- round(p_matrix, 4)

output_path <- 
write_xlsx(
  list(
    Correlation = as.data.frame(cor_matrix),
    P_Value = as.data.frame(p_matrix)
  ),
  output_path
)

cat("✅ Correlation and p-value matrices saved to:", output_path)
