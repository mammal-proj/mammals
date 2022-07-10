## code to prepare `sealions` dataset


# Load libraries ------------------------------------------------------------------------------

  library(data.table)

# Load dataset --------------------------------------------------------------------------------

  sealions <- data.table::fread("data-raw/historicos.csv", na.strings = c("", "nan", "NA"))

# Further processing --------------------------------------------------------------------------

  # Non-numeric columns
  ind <- sealions[, names(.SD), .SDcols = !is.numeric]
  lapply(
    X = sealions[, ..ind],
    FUN = unique
  )
  sealions[, (ind) := lapply(.SD, as.factor), .SDcols = ind]

# Exporting the dataset -----------------------------------------------------------------------

  usethis::use_data(sealions, overwrite = TRUE)
