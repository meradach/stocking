#all_stocks <- stockSymbols()

GetStockData <- function(symbol, from = NULL) {
  # Wrapper to get stock data data.frame format -- any value in keeping as xts/zoo?
  # Creates a time-valued date column

  if(is.null(from)) from <- "1900-01-01"
  x <- quantmod::getSymbols(symbol, src = "yahoo", auto.assign = FALSE, from = from)
  x <- as.data.frame(x)

  colnames(x) <- .AdjustStockNames(symbol, names(x))

  x$date <- strptime(rownames(x), format = "%Y-%m-%d", tz = "UTC")

  rownames(x) <- NULL

  out <- list(data   = x,
              symbol = toupper(symbol),
              start  = min(x$date),
              end    = max(x$date))

  structure(out, class = "stock")
}


# StockWindow ----------------------------------------------------------------------------

SubsetStock <- function(stock, start = "min", end = "max") {
  # Returns a window of stock history, as class stock, given by start and end dates

  # Args:
  #   stock: object of class stock
  #   start: first day of stock price to include; defaults to first available in data
  #   end  : last day of stock price to include; defaults to last available in data

  # Returns:
  #   an object of class stock

  # Supply defaults
  if(start == "min" || start < stock$start) {
    stock$start <- min(stock$data$date)
  } else {
    stock$start <- start
  }
  if(end == "max" || end > stock$end) {
    stock$end <- max(stock$data$date)
  } else {
    stock$end <- end
  }

  # Subset data and replace data with subsetted version
  stock_data <- stock$data[stock$data$date >= stock$start &
                             stock$data$date <= stock$end, ]
  stock$data <- stock_data

  # Return output
  return(stock)
}


#######################################################################

is.stock <- function(x) {
  inherits(x, "stock")
}


#######################################################################

print.stock <- function(x, ...) {
  print(head(x$data, 5L))
  cat("  ...\n")
  print(tail(x$data, 5L))
}


#######################################################################

plot.stock <- function(x, ...) {
  # Simple plotting utility for class stock

  # Args:
  #   x: object of class stock

  prepped_data <- .PrepStockForPlot(x$data)
  title <- paste0(x$symbol, ": Open and Close Prices with High/Low Band")

  ggplot(prepped_data, aes(x = date, y = price)) +
    geom_ribbon(aes(ymin = low, ymax = high), fill = "grey70", alpha = 0.3) +
    geom_line(aes(color = time), lwd = 1.3) +
    scale_color_viridis(discrete = TRUE, option = "plasma", name = "Time") +
    labs(x = "Date", y = "Price", title = title) +
    theme_bw()
}


#######################################################################

.PrepStockForPlot <- function(stock_data) {
  # Prepares stock data from object of class "stock" for plotting with ggplot2

  price <- c(stock_data$open, stock_data$close)
  time  <- factor(rep(c("Open", "Close"), each = nrow(stock_data)), levels = c("Open", "Close"))
  date  <- rep(stock_data$date, times = 2L)
  high  <- rep(stock_data$high, times = 2L)
  low   <- rep(stock_data$low, times = 2L)

  plot_data <- data.frame(date = date,
                          price = price,
                          time = time,
                          high = high,
                          low = low)

  return(plot_data)
}


#######################################################################

.MakeFirstLetterLowerCase <- function(char) {
  # Makes first letter of vector char into lower case

  first_letters <- substr(char, 1L, 1L)
  lower_firsts  <- tolower(first_letters)
  char_ending   <- substr(char, 2L, nchar(char))

  paste0(lower_firsts, char_ending)
}


#######################################################################

.AdjustStockNames <- function(symbol, data_names) {
  # Standardizes names of stock data coming from quantmod::getSymbols
  # Returns character vector of adjusted names

  gsub_pattern   <- paste0(toupper(symbol), "\\.")
  char_no_symbol <- gsub(gsub_pattern, "", data_names)

  char_out <- .MakeFirstLetterLowerCase(char_no_symbol)

  return(char_out)
}
