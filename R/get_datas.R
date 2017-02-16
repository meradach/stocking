#all_stocks <- stockSymbols()
StockData <- function(symbol, from = NULL) {
  # Wrapper to get stock data data.frame format -- any value in keeping as xts/zoo?
  # Creates a time-valued date column

  if(is.null(from)) from <- "1900-01-01"
  x <- quantmod::getSymbols(symbol, src = "yahoo", auto.assign = FALSE, from = from)
  x <- as.data.frame(x)

  colnames(x) <- c("open", "high", "low", "close", "volume",
                   if(ncol(x) == 6) "adjusted" else NULL)

  x$date <- strptime(rownames(x), format = "%Y-%m-%d", tz = "UTC")

  rownames(x) <- NULL

  #structure(x, class = "stock")
  return(x)
}


is.stock <- function(x) {
  inherits(x, "stock")
}


print.stock <- function(x, ...) {
  head(data.frame(x), 10)
}
