pollutantmean1 <- function(directory, pollutant, id = 1:332) {
  files_full <- list.files(directory, full.names=TRUE)
  ls_dat <- lapply(files_full, read.csv)
  df_dat <- do.call(rbind, ls_dat)
  sub_dat <- df_dat[which(df_dat$ID %in% id), ]
  mean(sub_dat[, pollutant], na.rm = TRUE)
}