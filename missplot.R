#missingness plot

library(dplyr)
library(naniar)

#create dataframe
df <- data2_9

#select columns w/ missing data
missing_data <- df[, c(8:14)]

#convert everything to numeric (this also makes blank cells = NA)
missing_data[] <- lapply(missing_data, function(x) {
  x[x == ""] <- NA
  as.numeric(x)
})

#rename columns for plot
colnames(missing_data) <- c(
  "NIH Card Sort",
  "NIH Flanker",
  "NIH List Sort",
  "NIH Processing Speed",
  "Positive Affect",
  "Positive Behavior",
  "PCEs"
)

#create plot
gg_miss_upset(
  missing_data,
  nsets = ncol(missing_data),   # ensure all columns are included (optional)
  order.by = "freq"             # order by frequency (optional)
)

