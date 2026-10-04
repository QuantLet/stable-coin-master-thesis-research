# ==== FRM_SC_helpers.R ==================================================

pick_file_flexible <- function(path, strong_regex, weak_regex, role = 'file') {
  f <- list.files(path, pattern = strong_regex, full.names = TRUE, ignore.case = TRUE)
  if (!length(f)) f <- list.files(path, pattern = weak_regex, full.names = TRUE, ignore.case = TRUE)
  if (!length(f)) stop(sprintf('No %s found in %s (tried ''%s'' then ''%s'')', role, path, strong_regex, weak_regex))
  extract_max_date_from_name <- function(x) {
    b <- basename(x); ds <- gregexpr('[0-9]{8}', b, perl = TRUE); vals <- regmatches(b, ds)[[1]]
    if (length(vals)) as.integer(max(vals)) else NA_integer_
  }
  name_dates <- vapply(f, extract_max_date_from_name, integer(1))
  if (any(!is.na(name_dates))) return(f[which.max(name_dates)])
  # fallback by content
  best_file <- NULL; best_last <- as.Date('1900-01-01')
  for (fp in f) {
    hdr <- tryCatch(read.csv(fp, nrows = 1, check.names = FALSE), error = function(e) NULL); if (is.null(hdr)) next
    date_col <- if ('Date' %in% names(hdr)) 'Date' else if ('date' %in% names(hdr)) 'date' else names(hdr)[1]
    col <- tryCatch(read.csv(fp, colClasses = 'character')[[date_col]], error = function(e) NULL); if (is.null(col)) next
    d1 <- suppressWarnings(as.Date(col, format = '%m/%d/%Y')); if (all(is.na(d1))) d1 <- suppressWarnings(as.Date(col))
    d1 <- d1[!is.na(d1)]; if (!length(d1)) next
    last_d <- max(d1); if (last_d > best_last) { best_last <- last_d; best_file <- fp }
  }
  if (is.null(best_file)) best_file <- f[1]
  best_file
}

read_panel <- function(fp) {
  df <- readr::read_csv(fp, show_col_types = FALSE)
  stopifnot(ncol(df) >= 2)
  first <- names(df)[1]
  x <- as.character(df[[first]])
  d <- suppressWarnings(as.Date(x, format = '%m/%d/%Y'))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x))
  if (any(is.na(d))) stop(sprintf('Date parse failed in %s (first col ''%s'').', basename(fp), first))
  df[[first]] <- d; names(df)[1] <- 'ticker'
  for (nm in setdiff(names(df), 'ticker')) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  df
}

safe_vals  <- function(m) { v <- suppressWarnings(as.numeric(m)); v[is.finite(v)] }
safe_names <- function(m) colnames(m)

idx_start_at_or_after <- function(key, target) { w <- which(key >= target); if (length(w)) w[1] else NA_integer_ }
idx_end_at_or_before  <- function(key, target) { w <- which(key <= target); if (length(w)) tail(w, 1) else NA_integer_ }

hhi_from_values <- function(x) {
  x <- suppressWarnings(as.numeric(x)); x[!is.finite(x) | x < 0] <- 0
  s <- sum(x); if (!is.finite(s) || s <= 0) return(NA_real_)
  sum((x/s)^2)
}
