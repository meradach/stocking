# MakePair -------------------------------------------------------------------------------------

MakePair <- function(stock1, stock2, FUN = `/`, time = "open", center = FALSE, scale = FALSE) {
  # Makes an object of class "pair".
  # Takes stock1 and stock2 and uses FUN to create a vector that relates
  # the daily prices of the stocks at the requested time (open or close).

  # Args:
  #   stock1, stock2: two objects of class "stock".  stock1 will be the first argument to FUN.
  #   FUN: the function to apply to the stocks, e.g. `/` or `-`.
  #   time: "open" or "close", the time at which to compute the function of prices each day.
  #   center: whether to center the stocks around their mean
  #   scale: whether to scale the stocks by their sd

  # Get names before modifying object
  name1 <- deparse(substitute(stock1))
  name2 <- deparse(substitute(stock2))

  # Subset to earliest common stock data
  earliest_common <- max(c(min(stock1$date), min(stock2$date)))
  stock1          <- stock1[stock1$date >= earliest_common, ]
  stock2          <- stock2[stock2$date >= earliest_common, ]

  # Standardize if requested
  if(center) {
    stock1_center <- mean(stock1[[time]])
    stock2_center <- mean(stock2[[time]])
  } else {
    stock1_center <- stock2_center <- 0
  }

  if(scale) {
    stock1_scale <- sd(stock1[[time]])
    stock2_scale <- sd(stock2[[time]])
  } else {
    stock1_scale <- stock2_scale <- 1
  }

  stock1_for_pair <- (stock1[[time]] - stock1_center) / stock1_scale
  stock2_for_pair <- (stock2[[time]] - stock2_scale) / stock2_scale

  # Create list and return special class
  pair <- list(date     = stock1$date,
               stock1   = structure(stock1[[time]], symbol = toupper(name1)),
               stock2   = structure(stock2[[time]], symbol = toupper(name2)),
               pair     = structure(FUN(stock1_for_pair, stock2_for_pair),
                                    time = time, FUN = FUN,
                                    centered = center, scaled = scale)
  )

  structure(pair, class = "pair")
}



# PairFun ---------------------------------------------------------------------------

PairFun <- function(pair) {
  # Return function used to generate object of class pair from MakePair()

  attr(pair$pair, "FUN")
}


# PairStats ----------------------------------------------------------------------------------------

PairStats <- function(pair) {
  # Computes basic statistics from pair data

  return(c(mean   = mean(pair$pair),
           median = median(pair$pair),
           sd     = sd(pair$pair),
           min    = min(pair$pair),
           max    = max(pair$pair))
  )
}



# FindCandidates --------------------------------------------------------------------------

FindCandidates <- function(pair, k = 1, stock = c(1, 2), pos = c("open", "close")) {
  # Finds opening or closing position candidates

  # Args:
  #   pair: pair data from MakePair()
  #   k: number of standard deviations at which to set candidate trigger threshold,
  #   stock: which stock position to open/close, stock 1 or stock 2 (1 is numerator stock)
  #   pos: whether function searches for open-position candidates or close-position candidates

  # Choose defaults
  stock <- stock[1]
  pos   <- pos[1]

  # Generate and store pair statistics
  stats <- PairStats(pair)

  # The threshold of comparison depends on whether we're looking for opening or closing candidates.
  # For open-position candidates, we compare normalied pair values to s.d. * k;
  # for close-position candidates, we compare normalized pair values to 0.
  pair_th <- if(pos == "open") {stats["sd"] * k}  else  {0L}

  # Normalize pair data around mean. Then determine pair value relative to threshold.
  norm_pair  <- pair$pair - stats["mean"]
  norm_fun   <- if(stock == 1) `-` else `+`
  norm_pair_rel_th <- norm_fun(norm_pair, pair_th)

  # Take difference of sequential relative pair values. This tells us if we're moving up or down
  # through time.
  diff_rel_pair <- diff(norm_pair_rel_th)

  # Take difference of signs of relative pair values.  This tells us if we're
  # crossing a threshold (a non-zero indicates threshold crossing)
  diff_sign_rel_pair <- diff(sign(norm_pair_rel_th))

  # Stock 1, "open"  ==> positive value difference and positive sign difference is candidate
  # Stock 1, "close" ==> negative value difference and negative sign difference is candidate
  # Stock 2, "open"  ==> negative value difference and negative sign difference is candidate
  # Stock 2, "close" ==> positive value difference and positive sign difference is candidate
  candidate <- if((stock == 1 & pos == "open") | (stock == 2 & pos == "close")) {
    diff_rel_pair > 0  &  diff_sign_rel_pair > 0
  } else {
    diff_rel_pair < 0  &  diff_sign_rel_pair < 0
  }

  # Check boundary conditions -- do we open at beginning or close at end?
  element_first <- if(pos == "open") {
    if(stock == 1) norm_pair_rel_th[1] > 0  else  norm_pair_rel_th[1] < 0
  } else {
    FALSE      # we never close a position as our first move
  }

  return(c(element_first, candidate))
}



# FindPositions --------------------------------------------------------------

FindPositions <- function(open_candidates, close_candidates) {
  # From a vector of open-position candidates and close-position candidates,
  # this function finds the final open positions and close positions.

  # Args:
  #   *_candidates: logical vector of open- or close-position candidates,
  #                 the output of FindCandidates()

  # Returns:
  #   named list of length 2, each being a logical vector indicating the
  #   open and close positions

  # Perform some initial checks.  If you don't open, you don't close.  Return all FALSE.
  # OR if you open on the last day, you also wouldn't trade.
  if(sum(open_candidates) == 0 ||
     min(which(open_candidates)) == length(open_candidates) ) {

    return(list(open  = rep(FALSE, times = length(open_candidates)),
                close = rep(FALSE, times = length(close_candidates))))

  }

  # Create vector of candidate open-position indices
  cand_open_ix  <- which(open_candidates)

  # You don't close a position before you open one.
  close_candidates[1:min(cand_open_ix)] <- FALSE

  # Now check that there are close candidates available.  If not, assign last move to close.
  if(sum(close_candidates) == 0)  close_candidates[length(close_candidates)] <- TRUE

  # Create vector of candidate close-position indices
  cand_close_ix <- which(close_candidates)

  # Set up values for loop
  open_ix  <- 0                                 # for valid starting sum in loop
  open_vec <- min(cand_open_ix)                 # for seeding final open pos vector
  close_ix <- close_vec <- min(cand_close_ix)   # for loop and seeding final close pos vector

  # This loop builds vectors of indices for open/close positions.
  # It iteratively seeks the minimum open (close) index that is greater than the
  # previous close (open) index.  It builds a vector of indices (inefficiently?).
  while(!is.na(open_ix + close_ix)) {
    open_ix  <- suppressWarnings(cand_open_ix[min(which(cand_open_ix > close_ix))])
    close_ix <- suppressWarnings(cand_close_ix[min(which(cand_close_ix > open_ix))])

    open_vec  <- na.omit(c(open_vec, open_ix))
    close_vec <- na.omit(c(close_vec, close_ix))
  }

  # Finally we reconstruct a logical vector using these indices and put in a list
  open_pos <- close_pos <- rep(FALSE, length(open_candidates))

  open_pos[open_vec]   <- TRUE
  close_pos[close_vec] <- TRUE

  # If last move is to open, then make final entry a close so that we're always
  # cashing out at the end of the exercise
  if(max(which(open_pos)) > max(which(close_pos))) {
    close_pos[length(close_pos)] <- TRUE
  }

  list(open  = open_pos,
       close = close_pos)
}



# StockQty -----------------------------------------------------------------------

StockQty <- function(budget, stock_price) {
  # A simple wrapper to compute the number of shares you can buy with a given budget.
  # Either can be a vector as long as the other is a scalar, otherwise unexpected results!
  as.integer(floor(budget / stock_price))
}



# TradeQty ----------------------------------------------------------------------

TradeQty <- function(pair, positions, budget, opening_stock) {
  # Computes the quantity of stock traded at open and close

  # Args:
  #   pair: the output of MakePair()
  #   positions: output of FindPositions()
  #   budget: the dollar budget available to the trader FOR EACH BUY
  #   opening_stock: 1 or 2; the stock that gets shorted with the given positions objects

  # Returns:
  #   list of quantities traded for each open and close position

  # Stock quantities associated with this opening_stock value
  if(opening_stock == 1) {
    selling_stock <- pair$stock1
    buying_stock  <- pair$stock2
  } else {
    selling_stock <- pair$stock2
    buying_stock  <- pair$stock1
  }

  # When opening a position, define the qty of stock sold (shorted) and stock bought
  open_sell_qty <- positions$open * StockQty(budget, selling_stock)
  open_buy_qty  <- positions$open * StockQty(budget, buying_stock)

  # Initialize close-position trades to 0
  close_buy_qty <- close_sell_qty <- rep(0, times = length(open_sell_qty))

  # Fill in close-position trade quantities with the quantites we traded at open-position
  if(sum(positions$open) > 0) {
    close_buy_qty[positions$close]  <- open_sell_qty[positions$open]
    close_sell_qty[positions$close] <- open_buy_qty[positions$open]
  }

  # Return list
  list(open_sell_qty = open_sell_qty, open_buy_qty = open_buy_qty,
       close_buy_qty = close_buy_qty, close_sell_qty = close_sell_qty)
}



# FindTrades ----------------------------------------------------------------------

FindTrades <- function(pair, positions1, positions2, budget) {
  # Uses prices and positions to compute trades

  # Args:
  #   positions*: the output of FindPositions() for stock *
  #   budget: the dollar budget available to the trader FOR EACH BUY

  # Returns:
  #   data frame with open and close buy/sell quantities of stock1 and stock2

  # Compute trade quantites for open- and close-positions relative to stock 1 and 2
  trade_qty_1 <- TradeQty(pair, positions1, budget, opening_stock = 1)
  trade_qty_2 <- TradeQty(pair, positions2, budget, opening_stock = 2)

  # Return trade volumes data frame
  data.frame(open1_sell1_qty  = trade_qty_1$open_sell_qty,
             open1_buy2_qty   = trade_qty_1$open_buy_qty,
             close1_buy1_qty  = trade_qty_1$close_buy_qty,
             close1_sell2_qty = trade_qty_1$close_sell_qty,
             open2_buy1_qty   = trade_qty_2$open_buy_qty,
             open2_sell2_qty  = trade_qty_2$open_sell_qty,
             close2_sell1_qty = trade_qty_2$close_sell_qty,
             close2_buy2_qty  = trade_qty_2$close_buy_qty)
}



# FindReturn ---------------------------------------------

FindReturn <- function(pair, trades, budget, trade_fee) {
  # Computes return from trade

  # Args:
  #   pair: the output of MakePair()
  #   trades: the output of FindTrades
  #   budget: the dollar budget available to the trader FOR EACH BUY
  #   trade_fee: length-one numeric vector with cost of a single trade

  # Returns:
  #   length-one numeric vector of trade return

  # # How many times do we have to call on our budget for a trade at open?
  # n_opens_1 <- sum(trades$open1_buy2_qty > 0)
  # n_opens_2 <- sum(trades$open2_buy1_qty > 0)
  #
  # # What's the total cash-on-hand required for these opens?
  # total_budget_open1 <- budget * n_opens_1
  # total_budget_open2 <- budget * n_opens_2

  # How much do we spend in opening purchases?
  expense_1 <- sum(trades$open2_buy1_qty * pair$stock1)
  expense_2 <- sum(trades$open1_buy2_qty * pair$stock2)

  # At opening, compute the gains from the trades.  This is the money earned by the
  # short sale, less the money spent on buying
  open1_cash <- sum(trades$open1_sell1_qty * pair$stock1) - expense_2
  open2_cash <- sum(trades$open2_sell2_qty * pair$stock2) - expense_1

  # At closing, compute money earned by selling and money lost by buying
  close1_cash <- sum(-trades$close1_buy1_qty * pair$stock1 +
                       trades$close1_sell2_qty * pair$stock2)
  close2_cash <- sum(-trades$close2_buy2_qty * pair$stock2 +
                       trades$close2_sell1_qty * pair$stock1)

  # Data frame trades has one non-zero element for every trade
  n_trades <- sum(trades > 0)

  # Overall return is money on-hand after opening and closing, less trade fees per trade
  cash_money <- open1_cash + open2_cash + close1_cash + close2_cash - n_trades * trade_fee

  # Return money earned
  structure(cash_money, n_trades = n_trades, amt_spent = expense_1 + expense_2)
}



# PairTrade --------------------------------------------------------------------

PairTrade <- function(pair, k, budget, trade_fee) {
  # Wrapper for entire sequence of pairs trading functions.
  # Performs all calculations to determine return from pairs trading strategy.

  # Args:
  #   pair:       an object of class "pair" from MakePair() function
  #   k:          the threshold to cross for opening positions
  #   budget:     the dollar budget that the trader has to work with
  #   FUN:        the function to apply to stock 1 and stock 2 to make the pair
  #   time:       the daily timepoint at which to compute the pair ("open" or "close")
  #   trades_fee: the fee charged per trade event by the broker

  # Compute open-position and close-position candidates
  open_cand_1  <- FindCandidates(pair = pair, k = k, stock = 1, pos = "open")
  open_cand_2  <- FindCandidates(pair = pair, k = k, stock = 2, pos = "open")
  close_cand_1 <- FindCandidates(pair = pair, k = k, stock = 1, pos = "close")
  close_cand_2 <- FindCandidates(pair = pair, k = k, stock = 2, pos = "close")


  # Compute positions for stock 1 and stock 2
  pos1 <- FindPositions(open_candidates = open_cand_1, close_candidates = close_cand_1)
  pos2 <- FindPositions(open_candidates = open_cand_2, close_candidates = close_cand_2)

  # Compute trade volume
  trades <- FindTrades(pair = pair, positions1 = pos1, positions2 = pos2, budget = budget)

  # Compute return
  return     <- FindReturn(pair = pair, trades = trades, budget = budget, trade_fee = trade_fee)
  spent      <- attr(return, "amt_spent")
  pct_return <- if(spent == 0)  0  else as.numeric(return) / spent * 100

  # Output with attributes
  pair_return <- list(return      = as.numeric(return),
                      amt_spent   = spent,
                      pct_return  = pct_return,
                      amt_pledged = budget * attr(return, "n_trades")/4,
                      n_trades    = attr(return, "n_trades"),
                      stock1      = attr(pair$stock1, "symbol"),
                      stock2      = attr(pair$stock2, "symbol"),
                      k           = k,
                      budget      = budget,
                      trade_fee   = trade_fee,
                      time        = attr(pair$pair, "time"),
                      FUN         = PairFun(pair),
                      centered    = attr(pair$pair, "centered"),
                      scaled      = attr(pair$pair, "scaled"))

  structure(pair_return, class = "pair_model")
}


#  PairSplit --------------------------------------------------------------------

PairSplit <- function(pair, split, start = "min", end = "max") {
  # Splits pair data into two lists, one for training and one for testing.

  # Args:
  #   pair: object of class "pair" from MakePair()
  #   split: the DATE on which to begin the test data in yyyy-mm-dd format
  #   start: optional start date of training data.  If not specified,
  #          the earliest date ("min") in the pair object will be used.
  #   end:   the latest data of the testing data.  If not specified,
  #          the latest date ("max") in the pair object will be used.

  # Retrieve attributes
  nm1 <- attr(pair$stock1, "symbol")
  nm2 <- attr(pair$stock2, "symbol")
  tm  <- attr(pair$pair, "time")
  fn  <- PairFun(pair)
  ct  <- attr(pair$pair, "centered")
  sc  <- attr(pair$pair, "scaled")

  # Compute dates
  min_date <- if(start == "min") min(pair$date) else start
  max_date <- if(end   == "max") max(pair$date) else end

  # Subsetting vectors
  train_ix <- (pair$date >= min_date) & (pair$date < split)
  test_ix  <- (pair$date >= split)    & (pair$date <= max_date)

  # Training data
  train <- lapply(pair, function(x) x[train_ix])
  test  <- lapply(pair, function(y) y[test_ix])

  # Make list, assign attributes, and return
  out <- setNames(vector(mode = "list", length = 2), c("train", "test"))
  pair_list <- list(train = train, test = test)

  for(i in 1:length(pair_list)) {
    attr(pair_list[[i]]$stock1, "symbol") <- nm1
    attr(pair_list[[i]]$stock2, "symbol") <- nm2
    attributes(pair_list[[i]]$pair) <- list(time = tm, FUN = fn, centered = ct, scaled = sc)
    attr(pair_list[[i]], "class") <- "pair"
    out[[i]] <- pair_list[[i]]
  }
  out
}



# print.pair_model -----------------------------------------------------------

print.pair_model <- function(x, ...) {
  # Makes readable format for pair_model object

  line1 <- paste0("Stock 1:        ", x$stock1)
  line2 <- paste0("Stock 2:        ", x$stock2)
  line3 <- paste0("Amount spent:   ", round(x$amt_spent, 2))
  line4 <- paste0("Total return:   ", round(x$return, 2))
  line5 <- paste0("Percent return: ", round(x$pct_return, 3))

  cat(paste(line1, line2, line3, line4, line5, sep = "\n"))
}


# predict.pair_model --------------------------------------------------------

predict.pair_model <- function(object, newdata, ...) {
  # Get return from test data based on parameters of pair_model object.
  # Pass test pair to argument "newdata".

  PairTrade(pair = newdata,
            k = object$k,
            budget = object$budget,
            trade_fee = object$trade_fee)
}


# summary.pair_model ---------------------------------------------------------

# summary.pair_model <- function(object, ...) {
#   # A summary method for pair_model.  Just prints stuff for nice viewing.
#
#   cat(paste0("Return.....", object$return),
#       paste0("Amount Spent...", object$amt_spent),
#       paste0, )
# }


# plot.pair -----------------------------------------------------------------------

PlotPositionsData <- function(pair, k) {
  # Builds positions data for plotting function if type == "positions"

  # Compute open-position and close-position candidates
  open_cand_1  <- FindCandidates(pair = pair, k = k, stock = 1, pos = "open")
  open_cand_2  <- FindCandidates(pair = pair, k = k, stock = 2, pos = "open")
  close_cand_1 <- FindCandidates(pair = pair, k = k, stock = 1, pos = "close")
  close_cand_2 <- FindCandidates(pair = pair, k = k, stock = 2, pos = "close")

  # Compute positions for stock 1 and stock 2
  pos1 <- FindPositions(open_candidates = open_cand_1, close_candidates = close_cand_1)
  pos2 <- FindPositions(open_candidates = open_cand_2, close_candidates = close_cand_2)

  # Build data frame
  dat <- data.frame(pos1, pos2)
  names(dat) <- c("open1", "close1", "open2", "close2")

  dat
}


RectColors <- function(plot_positions) {
  # A function for use within RectData() that determines on which side of the mean
  # the trade is happening, so it can be colored accordingly

  # Args:
  #   plot_positions: output of PlotPositionsData

  # Get list of cumulative sums of open and close positions
  pos_list <- lapply(plot_positions, cumsum)

  # Take the difference between the open cumsum and the close cumsum for each position.
  # This gives us stretches of trading periods for each position.
  s1 <- pos_list$open1 - pos_list$close1
  s2 <- pos_list$open2 - pos_list$close2

  # The difference vector of these stretches indicates the boundaries of the stretches.
  # The stretches will be denoted by 1s in contrast to 0s. We rely on the sum of these
  # difference vectors to be zero, so we pad the start and end of the vectors with 0s
  # in order for the +1 and -1 values to balance out.
  ds1 <- diff(c(0, s1, 0))
  ds2 <- diff(c(0, s2, 0))

  # We have to both determine the ORDER of open1 and open2, and safeguard against
  # a simultaneous close/open.  For the ORDER we use an element-wise difference
  # of the differences, so that open1 is positive and open2 is negative.
  # The safeguard is to remove the closing indicators in the diff.
  ds1   <- ifelse(ds1 == -1, 0, ds1)
  ds2   <- ifelse(ds2 == -1, 0, ds2)
  ddiff <- ds1 - ds2

  # The orders are given by the signs of these values
  orders <- ddiff[ddiff != 0]

  # And we return a vector to describe the action in each move
  factor(ifelse(orders == 1, "Above", "Below"))
}


RectData <- function(pair, plot_positions) {
  # Creates rectangle coordinates for plot

  # Args:
  #   pair: pair object
  #   plot_positions: output of PlotPositionsData

  # Whether you open or you close -- used for min and max of rectangles
  opens  <- plot_positions$open1 | plot_positions$open2
  closes <- plot_positions$close1 | plot_positions$close2

  # Coordinates for rectangles
  xmin <- pair$date[opens]
  xmax <- pair$date[closes]

  ymin <- rep(min(pair$pair), times = length(xmin))
  ymax <- rep(max(pair$pair), times = length(xmin))

  # Color data
  cols <- RectColors(plot_positions)

  # Data frame for plotting
  data.frame(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, Open = cols)
}


PositionsPlot <- function(x, k) {
  # Creates ggplot object of pair value with highlighted positions

  # Args:
  #   x: object of class "pair"
  #   k: the threshold to cross for opening positions
  #   pair_df: data frame of

  # Convert pair data to data frame
  pair_df <- as.data.frame(unclass(x))

  # Generate data frame of positions
  pos_df <- PlotPositionsData(x, k)

  # Generate data frame of rectangle coordinates
  rect_df <- RectData(x, pos_df)

  # Compute statistics
  stat    <- PairStats(x)
  threshold_upper <- stat["mean"] + k*stat["sd"]
  threshold_lower <- stat["mean"] - k*stat["sd"]

  # Recompute rect_df min and max to account for thresholds' being beyond min and max
  # (if this is not corrected, you might get rectangles that don't fill the space)
  rect_df$ymin <- pmin(threshold_lower, rect_df$ymin)
  rect_df$ymax <- pmax(threshold_upper, rect_df$ymax)

  # Generate plot
  p <- ggplot(NULL) +
    geom_rect(data = rect_df,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Open),
              alpha = 0.3) +
    scale_fill_manual(values = c("skyblue", "gold")) +
    geom_hline(yintercept = stat["mean"], color = "darkgrey", lwd = 1.2) +
    geom_hline(yintercept = c(stat["mean"] + k*stat["sd"], stat["mean"] - k*stat["sd"]),
               color = "darkgrey", lty = 3, lwd = 1.2) +
    geom_line(data = pair_df, aes(x = date, y = pair), lwd = 1.2) +
    labs(x = "Date", y = "Pair Value", title = "Trading Periods") +
    theme_bw()

  return(p)
}


StockPlot <- function(x) {
  # Creates ggplot object of stock history for stock1 and stock2

  # Args:
  #   x: object of class "pair"

  # Get attributes for use in plot
  name1 <- attr(x$stock1, "symbol")
  name2 <- attr(x$stock2, "symbol")
  time  <- attr(x$pair, "time")
  time  <- paste0(toupper(substr(time, 1, 1)), substr(time, 2, nchar(time)))

  # Make data frame for plotting
  df <- data.frame(date   = rep(x$date, 2),
                   stock  = c(x$stock1, x$stock2),
                   Symbol = factor(
                              rep(c(name1, name2), each = length(x$date)),
                              levels = c(name1, name2))
                   )

  # Plot data
  p <- ggplot(df, aes(x = date, y = stock, color = Symbol)) +
    geom_line(lwd = 1.2) +
    labs(x = "Date", y = paste("Price at", time)) +
    theme_bw()

  return(p)
}



plot.pair <- function(x, k, type = "positions", ...) {
  # Plotting method for object of class "pair"

  # Args:
  #   x: object of class "pair"
  #   k: the threshold to cross for opening positions
  #   type: "positions" plots the pair value with open/close positions highlighted.
  #         "stocks" plots the two stock values.

  # Checks
  if(!(type %in% c("positions", "stocks"))) {
    stop("Please choose plot type 'positions' or 'stocks'")
  }

  if(missing(k) && type == "positions") {
    stop("Please supply a value for k for a positions plot, or set type = 'stocks'.")
  }

  # Proceed according to desired plot type
  if(type == "positions") {
    p <- PositionsPlot(x, k)
  } else {
    p <- StockPlot(x)
  }

  return(p)
}
