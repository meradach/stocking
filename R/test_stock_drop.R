library(ggplot2)
load(file = "data/itw_hon_stock_data.rda")
load(file = "data/ko_pep_stock_data.rda")

source("R/get_datas.R")
source("R/stock_drop_and_roll.R")

itw_sm <- SubsetStock(itw, start = "2016-12-01")
ComputePercentChange(itw_sm)
ComputePercentChange(itw_sm, plotit = TRUE)

ComputePriceChange(itw_sm)



source("R/pairs.R")
source("R/stock_drop_and_roll.R")
sdr1 <- StockDropAndRoll(stock = itw, budget = 10000, pct_change = -20, reversion_pct = 20,
                 search_window = 2, action_delay = 1, trade_fee = 9, time = "open")
plot(sdr1)

train <- itw[itw$date >= "2015-01-01" & itw$date < "2016-01-01", ]
test  <- itw[itw$date >= "2016-01-01", ]

drop_model <- StockDropAndRoll(train, 10000, -5, 10, 2, 0, 9, "open")
predict(drop_model, test)

plot(drop_model)


ko_train <- ko[ko$date >= "2015-01-01" & ko$date < "2016-01-01", ]
ko_test  <- ko[ko$date >= "2016-01-01", ]

drop_model_ko <- StockDropAndRoll(ko_train, 10000, -5, 5, search_window = 2, 0, 9, "open")
predict(drop_model_ko, ko_test)

plot(drop_model_ko)
plot(predict(drop_model_ko, ko_test))

################################################################
#   TESTING BUY DATE LOGICAL VECTOR FOR POST-CHANGE ACTIVITY   #
################################################################
#
# source("R/stock_drop_and_roll.R")
#
# q <- data.frame(unclass(StockWindow(itw, start = "2016-12-01")))
# PercentChange(q, plotit = TRUE)
#
# q_change_cands <- StockChangeCandidates(q, pct_change = -1, window = 2)
# q_change_cands
# plot(q$date, q$open, type = "l")
# abline(v = as.POSIXct(q$date[q_change_cands]))
#
# q_post_date <- PostChangeDate(q_change_cands)
# q_post_date
# abline(v = as.POSIXct(q$date[q_post_date]), lwd = 3, lty = 2)
#
# q_buy_delay <- PostChangeDateActionDelay(q_post_date, delay = 0)
# q_buy_delay
# abline(v = as.POSIXct(q$date[q_buy_delay]), lty = 3, lwd = 4, col = "green")


# #################################################
# #   BUILDING A PERCENT CHANGE SEARCH FUNCTION   #
# #################################################
#
# actions <- q_buy_delay
#
# price_at_action <- q[["open"]][q_buy_delay]
# date_at_action  <- q$date[q_buy_delay]
#
# forward_prices <- FindForwardPrices(actions, stock = q, time = "open")
# percentages <- lapply(seq_along(price_at_action), function(i)
#                         FindForwardPercentChanges(price_at_action[i], forward_prices[[i]])
#                       )
#
# first_crossing_list <- lapply(percentages, FindFirstReversionCandidates, threshold = 1)
#
# filled_out_crossing <- FillOutReversionCandidates(actions, first_crossing_list)
#
# # trades <- ProduceUsefulTrades(actions, filled_out_crossing)
# trades <- ProduceTrades(q, actions = actions, threshold = 1, time = "open")
#
# plot(q$date, q$open, type = "l", lwd = 2)
# abline(v = as.POSIXct(q$date[trades$actions]), lwd = 2, col = "steelblue")
# abline(v = as.POSIXct(q$date[trades$reversions]), lwd = 2, lty = 3, col = "red")
