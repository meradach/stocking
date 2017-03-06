library(ggplot2)
library(viridis)
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

train <- SubsetStock(itw, "2015-01-01", "2015-12-31")
test  <- SubsetStock(itw, "2016-01-01")

drop_model <- StockDropAndRoll(train, 10000, -5, 10, 2, 0, 9, "open")
predict(drop_model, test)

plot(drop_model)
plot(predict(drop_model, test))



ko_train <- ko[ko$date >= "2015-01-01" & ko$date < "2016-01-01", ]
ko_test  <- ko[ko$date >= "2016-01-01", ]

drop_model_ko <- StockDropAndRoll(ko_train, 10000, -5, 5, search_window = 2, 0, 9, "open")
predict(drop_model_ko, ko_test)

plot(drop_model_ko)
plot(predict(drop_model_ko, ko_test))
