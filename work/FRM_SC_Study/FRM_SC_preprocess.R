# ==== FRM_SC_preprocess.R ==============================================
source('FRM_SC_helpers.R')

price_path <- pick_file_flexible(input_path, '(?i)^Stable[_-]?Price_\d{8}\.csv$', '(?i)price.*\.csv$', 'Price CSV')
mcap_path  <- pick_file_flexible(input_path, '(?i)^Stable[_-]?(MarketCap|Mktcap)_\d{8}\.csv$', '(?i)(market.?cap|mkt.?cap).*\.csv$', 'MarketCap CSV')
macro_path <- pick_file_flexible(input_path, '(?i)^Stable[_-]?Macro_\d{8}\.csv$', '(?i)macro.*\.csv$', 'Macro CSV')

stock_prices <- read_panel(price_path)
mktcap       <- read_panel(mcap_path)
macro        <- read_panel(macro_path)

# Common coins
common <- intersect(colnames(stock_prices)[-1], colnames(mktcap)[-1])
stopifnot(length(common) > 0)
keep_cols <- c('ticker', common)
stock_prices <- stock_prices[, keep_cols, drop = FALSE]
mktcap       <- mktcap[, keep_cols, drop = FALSE]

M_stock <- ncol(stock_prices) - 1
M_macro <- ncol(macro) - 1
M       <- M_stock + M_macro

mktcap[is.na(mktcap)] <- 0

all_prices <- merge(stock_prices, macro, by = 'ticker', all.x = TRUE, sort = FALSE)
if (M_macro > 0) {
  macro_cols <- (M_stock + 2):(M + 1)
  all_prices[, macro_cols] <- zoo::na.locf(all_prices[, macro_cols, drop = FALSE], fromLast = TRUE, na.rm = FALSE)
}

dates  <- all_prices$ticker
ticker <- as.integer(format(dates, '%Y%m%d'))
N      <- length(dates)
stopifnot(identical(mktcap$ticker, dates))

all_prices[, -1] <- lapply(all_prices[, -1], as.numeric)
all_return <- diff(log(as.matrix(all_prices[, -1])))
all_return[!is.finite(all_return)] <- 0
stock_return <- all_return[, 1:M_stock,     drop = FALSE]
macro_return <- if (M_macro > 0) all_return[, (M_stock+1):M, drop = FALSE] else NULL

FRM_sort <- function(x) sort(as.numeric(x), decreasing = TRUE, index.return = TRUE)
mktcap_index <- matrix(0, N, M_stock)
mktcap_sort  <- apply(mktcap[, -1, drop = FALSE], 1, FRM_sort)
for (tt in 1:N) mktcap_index[tt, ] <- mktcap_sort[[tt]]$ix
mktcap_index <- cbind(ticker, mktcap_index)

# Pretty labels for plots/exports
COIN_LABELS <- setNames(pretty_coin(colnames(stock_prices)[-1]), colnames(stock_prices)[-1]
