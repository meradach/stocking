# cand_1_open <- FindCandidates(pr)
#
# lines(pr$date, pr$pair, type = "l")
# abline(v = pr$date[cand_1_open], col = "lightgray")
# abline(h = PairStats(pr)["mean"] + PairStats(pr)["sd"])

######################################################################

source("R/get_datas.R")
source("R/pairs.R")

#itw <- StockData("itw")
#hon <- StockData("hon")
#save(itw, hon, file = "R/itw_hon_stock_data.rda")
load(file = "data/itw_hon_stock_data.rda")

#ko <- StockData(symbol = "ko")
#pep <- StockData(symbol = "pep")
#save(ko, pep, file = "R/ko_pep_stock_data.rda")
load(file = "data/ko_pep_stock_data.rda")

k      <- 1
budget <- 10000
fun    <- `/`
time   <- "open"
fee    <- 9

pr <- MakePair(itw, hon, FUN = fun, time = time)
prs <- PairSplit(pr, split = "2016-01-01", start = "2015-01-01")

#debug(PairProfit)
train_profit <- PairTrade(prs$train, k = k, budget = budget, trade_fee = fee)

predict(train_profit, newdata = prs$test)
PairTrade(prs$test, k = k, budget = budget, trade_fee = fee)


# Find max profit
ks   <- seq(0.1, 2, by = 0.1)
mods <- lapply(ks, function(k) PairTrade(prs$train, k = k, budget = 10000, trade_fee = 9))
cash <- sapply(mods, function(x) x$pct_return)

plot(x = ks, y = cash, type = "l", lwd = 2)

predict(mods[[18]], newdata = prs$test)


# ------------------------------------------------------------------------------
# Testing the standardize feature

pr_n  <- MakePair(itw, hon, FUN = fun, time = time, center = FALSE, scale = FALSE)
prs_n <- PairSplit(pr_n, split = "2016-01-01", start = "2015-01-01")
st_no <- PairTrade(prs_n$train, k = 1, budget = 10000, trade_fee = 9) # should be same as train_profit
predict(st_no, newdata = prs_n$test)

pr_y  <- MakePair(itw, hon, FUN = `-`, time = time, center = TRUE, scale = TRUE)
prs_y <- PairSplit(pr_y, split = "2016-01-01", start = "2015-01-01")
st_y <- PairTrade(prs_y$train, k = 1, budget = 10000, trade_fee = 9)
predict(st_y, newdata = prs_y$test)




lar <- MakePair(stock1 = ko, stock2 = pep, FUN = `-`)
lar_split <- PairSplit(lar, split = "2016-01-01", start = "2015-01-01")
lar_train <- PairTrade(lar_split$train, k = 2, budget = 30000, trade_fee = 9)
predict(lar_train, newdata = lar_split$test)
plot(lar_split$train, k = 1)



# ---------------------------------------------------------------------------------
# Test plotting

library(ggplot2)
source("R/pairs.R")

plot(pr, k = 1, type = "positions")
plot(pr, type = "stocks")

plot(prs$train, k = 1, type = "positions")
plot(prs$test, k = 1, type = "positions")
plot(prs$test, type = "stocks")

plot(lar_split$train, k = 1)
plot(lar_split$train, type = "stocks")
