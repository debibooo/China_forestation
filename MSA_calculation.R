

work_dir<-"/.../MSA"
setwd(work_dir)

library(openxlsx)


if (!dir.exists("results")) dir.create("results")


csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)


for (csv_path in csv_files) {
  fname <- basename(csv_path)
  cat(">>> 处理", fname, "...\n")
  

  df <- read.csv(csv_path, row.names = 1, check.names = FALSE)
  colnames(df) <- as.numeric(colnames(df))
  

  MSA_values      <- c(
    pasture           = 0.3,
    rangeland         = 0.6,
    cropland_2Gbioen  = 0.3,
    rainfed_crops     = 0.2,
    irrigated_crops   = 0.05,
    forest_unmanaged  = 1,
    forest_managed    = 0.5,
    other             = 1,
    built_up          = 0.05
  )
  MSA_initial     <- 0.5
  regcoefficient  <- 0.081
  maxrestoredMSA  <- 0.9
  
 
  weighted_msa <- list()
  for (year in colnames(df)) {
    year_int <- as.integer(year)
    msa_numerator    <- 0
    area_denominator <- 0
    
    for (t in seq(2010, year_int, by = 10)) {
      t_index <- as.character(t)
      if (t == 2010) {
        At          <- 0
        current_msa <- MSA_initial
      } else {
        previous_t_index <- as.character(t - 10)
        At <- df['restored_forest', t_index] -
          df['restored_forest', previous_t_index]
        counter     <- (t - 2010) / 10
        current_msa <- MSA_initial +
          regcoefficient * log(10 * counter)
        current_msa <- min(current_msa, maxrestoredMSA)
      }
      msa_numerator    <- msa_numerator + At * current_msa
      area_denominator <- area_denominator + At
    }
    
    if (area_denominator > 0) {
      restored_forest_msa <- msa_numerator / area_denominator
    } else {
      restored_forest_msa <- 0
    }
    
    total_area        <- sum(df[, year], na.rm = TRUE)
    weighted_msa_sum  <- 0
    for (lutype in names(MSA_values)) {
      if (lutype %in% rownames(df)) {
        area                <- df[lutype, year]
        weighted_msa_sum    <- weighted_msa_sum +
          area * MSA_values[lutype]
      }
    }
    if ('restored_forest' %in% rownames(df)) {
      weighted_msa_sum <- weighted_msa_sum +
        area_denominator * restored_forest_msa
    }
    
    weighted_msa[[year]] <- weighted_msa_sum / total_area
  }
  
  out_df <- data.frame(
    year         = as.integer(names(weighted_msa)),
    Weighted_MSA = unlist(weighted_msa),
    row.names    = NULL
  )
  
  out_xlsx <- file.path("results",
                        sub("\\.csv$", ".xlsx", fname))
  wb <- createWorkbook()
  addWorksheet(wb, "Weighted_MSA")
  writeData(wb, "Weighted_MSA", out_df)
  saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  
  cat("    -> saved：", out_xlsx, "\n\n")
}

cat("saved ./results/, \n")