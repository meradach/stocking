# StockWindow ----------------------------------------------------------------------------

StockWindow <- function(stock, start = "min", end = "max") {
  # Returns a window of stock history given by start and end dates

  # Args:
  #   stock: output of StockData()
  #   start: first day of stock price to include; defaults to first available in data
  #   end  : last day of stock price to include; defaults to last available in data

  # Returns:
  #   a data frame just like StockData() but with a smaller range of time values

  # Supply defaults
  if(start == "min") start <- min(stock$date)
  if(end == "max")   end   <- max(stock$date)

  # Return output
  out <- stock[stock$date >= start & stock$date <= end, ]
  structure(out, class = c("stock_window"))
}



# plot.stock_window ---------------------------------------------------------------------

plot.stock_window <- function(x, time, ...) {
  # Simple plotting utility for class stock_window

  # Args:
  #   x: object of class stock_window
  #   time: "open" or "close"

  # Checks
  if(!(time %in% c("open", "close"))) stop("Please supply 'open' or 'close' to time argument.")

  x <- as.data.frame(unclass(x))

  ggplot(x, aes_string(x = "date", y = time)) +
    geom_line(lwd = 1.2) +
    theme_bw()
}



# StockChange --------------------------------------------------------------------------

PercentChange <- function(stock, start = "min", end = "max", time = "open", plotit = FALSE) {
  # Computes the value change of the stock between two dates

  # Args:
  #   stock:  output of StockData()
  #   start:  first day of stock price to include; defaults to first available in data
  #   end  :  last day of stock price to include; defaults to last available in data
  #   time :  the time of day to use for computation ("open" or "close")
  #   plotit: whether to plot the results

  data <- StockWindow(stock, start = start, end = end)

  first_value <- head(data[[time]], 1)
  last_value  <- tail(data[[time]], 1)

  price_change  <- last_value - first_value
  percent_change <- price_change / first_value * 100

  if(plotit) {
    p <- plot(StockWindow(stock, start = start, end = end), time = time)
    print(p)
  }

  structure(round(percent_change, 4),
            first_price = first_value,
            last_price = last_value,
            price_change = round(price_change, 2))
}


PriceChange <- function(stock, start = "min", end = "max", time = "open") {
  # Simple wrapper around PercentChange to get price change; returns no other information.

  # Args:
  #   see PercentChange.

  # Returns:
  #   length-1 numeric.

  attr(PercentChange(stock, start, end, time), "price_change")
}



# StockChange -------------------------------------------------------------------------

StockChangeCandidates <- function(stock, pct_change, window = 1, time = "open") {
  # Determine the day on which a given percent change occurs within a given time window.

  # Args:
  #   stock: output of StockData()
  #   pct_change: the desired percent change in stock price (positive or negative)
  #   window: the time window (number of days) within which the change occurs.
  #   time: the time of day at which the computations are made ("open" or "close")

  # Checks
  if(pct_change == 0) stop("What are you even doing?")
  if(window < 1) stop("That's not a real time window.")

  # Full price vector
  price <- stock[[time]]

  # Take the appropriate lagged differences
  diffs <- diff(price, lag = window)

  # To get percent change we compare lagged differences to starting point
  start_prices  <- price[1:(length(price) - window)]
  price_changes <- diffs / start_prices * 100

  # Padding for ineligible values at start of stock time window
  pad <- rep(FALSE, window)

  # Comparing price_changes to pct_change depends on sign of pct_change
  compare <- if(pct_change < 0) `<` else `>`

  # Return vector of dates on which desired pct_change is achieved within given window
  c(pad, compare(price_changes, pct_change))
}



# PostChangeBuyDate --------------------------------------------------------------

PostChangeDate <- function(stock_change_candidates) {
  # Calculate date of earliest stock change per run of TRUES coming from StockChangeCandidates

  first_n_minus_1_cand <- head(stock_change_candidates, n = length(stock_change_candidates) - 1)
  substraction_vector  <- c(FALSE, first_n_minus_1_cand)

  first_dates_are_greater_than_0 <- stock_change_candidates - substraction_vector
  first_dates_post_change <- first_dates_are_greater_than_0 > 0

  return(first_dates_post_change)
}


# PostChangeDateBuyDelay ----------------------------------------------------------

PostChangeDateActionDelay <- function(post_change_date, delay = 0) {
  # Produce time delay between stock price change and date of action equal in number
  # of days to value 'delay'

  delay_vector             <- rep(FALSE, times = delay)
  post_change_date_trimmed <- head(post_change_date, n = length(post_change_date) - delay)
  action_date_vector          <- c(delay_vector, post_change_date_trimmed)
  action_date_vector[length(action_date_vector)] <- FALSE  # to prevent action on last day

  return(action_date_vector)
}



# FindStockPercentChanges ----------------------------------------------------------

FindStockPercentChanges <- function(current_price, other_prices) {
  # From a given price and a vector of other prices, find the % change of other prices
  # relative to current price.
  (other_prices - current_price) / current_price * 100
}




ProduceTrades <- function(stock, actions, threshold, time = "open") {
  # From vector of action dates, produce true actions and reversions using trade threshold

  # set up reversion vector
  reversions <- rep(FALSE, times = nrow(stock))

  # Return if no actions taken
  if(sum(actions) == 0) {
    return(list(actions    = reversions,
                reversions = reversions))
  }

  # Set up for loop if actions taken
  CompareFun <- if(threshold < 0) `<` else `>`
  i <- which(actions)[1L]

  while(TRUE) {
    # Find ALL timepoints when threshold is crossed, INCLUDING backwards in time
    pct_changes           <- FindStockPercentChanges(stock[[time]][i], stock[[time]])
    all_candidates        <- CompareFun(pct_changes, threshold)
    all_candidates_ix     <- which(all_candidates)

    # if there are reversion candidates after this action, find the correct one;
    # otherwise, kill all future action candidates and revert on the last possible day
    if(any(all_candidates_ix > i)) {
      all_fwd_candidates_ix <- all_candidates_ix[which(all_candidates_ix > i)]
      min_fwd_cand_ix       <- min(all_fwd_candidates_ix)

      actions[(i+1):min_fwd_cand_ix] <- FALSE
      reversions[min_fwd_cand_ix]    <- TRUE
    } else {
      actions[(i+1):length(actions)] <- FALSE
      reversions[length(reversions)] <- TRUE

      break()
    }

    # find next i
    if(min_fwd_cand_ix < length(actions)-1L && any(actions[(min_fwd_cand_ix+1L):length(actions)])) {
      current_actions <- which(actions)
      i <- min(current_actions[current_actions > i])
    } else {
      break()
    }
  }

  list(actions    = actions,
       reversions = reversions)
}


# CalculateStockDropReturn -----------------------------------------------------------

CalculateStockDropReturn <- function(stock, trades, budget, trade_fee,
                                     threshold, time = "open") {
  # Computes dollar return from the trade strategies given by actions and reversions

  # Args:
  #   stock: output of StockData()
  #   trades: list output of ProduceUsefulTrades()
  #   budget: the maximum dollar amount to invest at each buy
  #   trade_fee: the dollar fee for each trade
  #   threshold: the percent threshold that signals action; used to determine "buy" or "short"
  #   time: the time of day at which to trade ("open" or "close")

  action_prices    <- stock[[time]][trades$actions]
  reversion_prices <- stock[[time]][trades$reversions]

  action_quantity   <- StockQty(budget, action_prices)
  action_dollars    <- action_quantity * action_prices
  reversion_dollars <- action_quantity * reversion_prices

  n_trades <- sum(sapply(trades, sum))

  if(threshold < 0) {
    dollar_return    <- sum(reversion_dollars - action_dollars) - n_trades * trade_fee
    dollars_invested <- sum(action_dollars) + n_trades * trade_fee
  } else {
    dollar_return    <- sum(action_dollars - reversion_dollars) - n_trades * trade_fee
    dollars_invested <- sum(reversion_dollars) + n_trades * trade_fee
  }

  percent_return <- if(n_trades > 0) {
    dollar_return / dollars_invested * 100
  } else {
    0
  }

  stock_drop_return <- list(return      = round(dollar_return, 2),
                            amt_spent   = round(dollars_invested, 2),
                            pct_return  = round(percent_return, 2),
                            amt_pledged = budget * n_trades / 2L,
                            n_trades    = n_trades)
  return(stock_drop_return)
}




# BuyAndHoldSingle -----------------------------------------------------------------------

BuyAndHoldSingle <- function(stock, budget, trade_fee = 9, time = "open") {
  # Compute earnings from buy-and-hold strategy over entire stock history provided.
  # Use this as a benchmark to compare any trading strategy attempted.

  if(budget > 0) {
    buy_price  <- stock[[time]][1L]
    sell_price <- stock[[time]][nrow(stock)]

    buy_qty  <- StockQty(budget, buy_price)

    money_in  <- buy_price * buy_qty + trade_fee*2L  # only two trades are ever made in this approach
    money_out <- sell_price * buy_qty

    return_cash <- money_out - money_in
    return_pct  <- return_cash / money_in * 100

    out_list <-
      list(return   = round(return_cash, 2),
           amt_spent  = round(money_in, 2),
           pct_return = round(return_pct, 2))
  } else {
    out_list <-
      list(return     = 0,
           amt_spent  = 0,
           pct_return = 0)
  }

  return(out_list)
}



# StockDropAndRoll ---------------------------------------------------------------------

StockDropAndRoll <- function(stock, budget, pct_change, reversion_pct, search_window = 1,
                             action_delay = 0, trade_fee = 9, time = "open") {
  # Performs all steps to compute return from stock change trading strategy

  change_candidates <- StockChangeCandidates(stock, pct_change, search_window, time)
  post_change_dates <- PostChangeDate(change_candidates)
  actions           <- PostChangeDateActionDelay(post_change_dates, action_delay)

  trades            <- ProduceTrades(stock, actions, reversion_pct, time)

  stock_drop_returns <- CalculateStockDropReturn(stock, trades, budget, trade_fee, pct_change, time)

  buy_and_hold_returns <- BuyAndHoldSingle(stock = stock,
                                           budget = budget * stock_drop_returns$n_trades / 2L,
                                           trade_fee = trade_fee,
                                           time = time)

  drop_list <- list(stock_drop_returns,
                    source                = toupper(deparse(substitute(stock))),
                    budget                = budget,
                    pct_change_trigger    = pct_change,
                    reversion_pct_trigger = reversion_pct,
                    search_window         = search_window,
                    action_delay          = action_delay,
                    trade_fee             = trade_fee,
                    time                  = time,
                    pct_return_buy_and_hold  = buy_and_hold_returns$pct_return,
                    points_over_buy_and_hold = stock_drop_returns$pct_return - buy_and_hold_returns$pct_return,
                    stock[c("date", time)],
                    trades)

  drop_return <- unlist(drop_list, recursive = FALSE)

  names(drop_return)[grepl(time, names(drop_return))] <- "price"

  structure(drop_return, class = "drop_model")
}



# print.drop_model -----------------------------------------------------------

print.drop_model <- function(x, ...) {
  # Makes readable format for drop_model object

  line1 <- paste0("Stock:                    ", x$stock)
  line2 <- paste0("Amount spent:             ", x$amt_spent, 2)
  line3 <- paste0("Net return:               ", x$return, 2)
  line4 <- paste0("Percent return:           ", x$pct_return, 3)
  line5 <- paste0("Points over buy-and-hold: ", round(x$points_over_buy_and_hold, 2))


  cat(paste(line1, line2, line3, line4, line5, sep = "\n"))
}



# predict.drop_model --------------------------------------------------------

predict.drop_model <- function(object, newdata, ...) {
  # Get return from test data based on parameters of drop_model object.
  # Pass test stock data to argument "newdata".

  StockDropAndRoll(stock = newdata,
                   budget = object$budget,
                   pct_change = object$pct_change_trigger,
                   reversion_pct = object$reversion_pct_trigger,
                   search_window = object$search_window,
                   action_delay = object$action_delay,
                   trade_fee = object$trade_fee,
                   time = object$time)
}


# plot.drop_model  -------------------------------------------------------------------


RectDataDrop <- function(price, date, actions, reversions) {
  # Creates rectangle coordinates for plot

  # Args:
  #   price:      vector of prices associated with actions and reversions vectors
  #   actions:    logical vector indicating when to execute action
  #   reversions: logical vector indicating when to revert action

  # Coordinates for rectangles
  xmin <- date[actions]
  xmax <- date[reversions]

  ymin <- rep(min(price), times = length(xmin))
  ymax <- rep(max(price), times = length(xmin))

  # Data frame for plotting
  data.frame(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}


PlotDataDrop <- function(drop_model) {
  # Creates data frame for ggplot2

  data.frame(date = drop_model$date,
             price = drop_model$price)
}


plot.drop_model <- function(x, ...) {
  # Plots stock timeline with highlighting rectangles in trading regions.

  # Args:
  #   x: object of class drop_model

  rect_df   <- RectDataDrop(x$price, x$date, x$actions, x$reversions)
  plot_data <- PlotDataDrop(x)

  ggplot(NULL) +
    geom_rect(data = rect_df,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              alpha = 0.3, fill = "skyblue") +
    geom_line(data = plot_data, aes(x = date, y = price), lwd = 1.2) +
    labs(x = "Date", y = "Price", title = "Trading Periods") +
    theme_bw()
}


