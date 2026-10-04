
source("FRM_SC_config.R"); source("FRM_SC_utils.R")

# NOTE: double-escaped backslashes so generated file keeps "\d" and "\."
price_path <- pick_file_flexible(input_path, "(?i)^Stable[_-]?Price_\\d{8}\\.csv$", "(?i)price.*\\.csv$", "Price CSV")
mcap_path  <- pick_file_flexible(input_path, "(?i)^Stable[_-]?(MarketCap|Mktcap)_\\d{8}\\.csv$", "(?i)(market.?cap|mkt.?cap).*\\.csv$", "MarketCap CSV")
macro_path <- pick_file_flexible(input_path, "(?i)^Stable[_-]?Macro_\\d{8}\\.csv$", "(?i)macro.*\\.csv$", "Macro CSV")

read_panel <- function(fp){
  df <- readr::read_csv(fp, show_col_types=FALSE)
  first <- names(df)[1]; x <- as.character(df[[first]])
  d <- suppressWarnings(as.Date(x, format="%m/%d/%Y")); if (all(is.na(d))) d <- suppressWarnings(as.Date(x))
  if (any(is.na(d))) stop(sprintf("Date parse failed in %s", basename(fp)))
  df[[first]] <- d; names(df)[1] <- "ticker"
  for (nm in setdiff(names(df),"ticker")) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  df
}
stock_prices <- read_panel(price_path); mktcap <- read_panel(mcap_path); macro <- read_panel(macro_path)

common <- intersect(colnames(stock_prices)[-1], colnames(mktcap)[-1])
keep_cols <- c("ticker", common)
stock_prices <- stock_prices[, keep_cols, drop=FALSE]
mktcap       <- mktcap[, keep_cols, drop=FALSE]

M_stock <- ncol(stock_prices)-1; M_macro <- ncol(macro)-1; M <- M_stock + M_macro
mktcap[is.na(mktcap)] <- 0

all_prices <- merge(stock_prices, macro, by="ticker", all.x=TRUE, sort=FALSE)
if (M_macro > 0) {
  macro_cols <- (M_stock + 2):(M + 1)
  all_prices[, macro_cols] <- zoo::na.locf(all_prices[, macro_cols, drop=FALSE], fromLast=TRUE, na.rm=FALSE)
}

dates <- all_prices$ticker; ticker <- as.integer(format(dates, "%Y%m%d")); N <- length(dates)
stopifnot(identical(mktcap$ticker, dates))

all_return <- diff(log(as.matrix(all_prices[, -1]))); all_return[!is.finite(all_return)] <- 0
stock_return <- all_return[, 1:M_stock, drop=FALSE]
macro_return <- if (M_macro>0) all_return[, (M_stock+1):M, drop=FALSE] else matrix(, nrow(all_return), 0)

FRM_sort <- function(x) sort(as.numeric(x), decreasing=TRUE, index.return=TRUE)
mktcap_index <- matrix(0, N, M_stock); mktcap_sort <- apply(mktcap[, -1, drop=FALSE], 1, FRM_sort)
for (tt in 1:N) mktcap_index[tt, ] <- mktcap_sort[[tt]]$ix
mktcap_index <- cbind(ticker, mktcap_index)

idx_start_at_or_after <- function(key, target){ w <- which(key >= target); if (length(w)) w[1] else NA }
idx_end_at_or_before  <- function(key, target){ w <- which(key <= target); if (length(w)) tail(w,1) else NA }

save(list = ls(), file = file.path(output_path, "stage_20_loaded.RData"))

