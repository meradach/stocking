options(warn = 1)
source("R/get_datas.R")
debug(GetStockData)

itw_all <- GetStockData("itw")
str(itw_all)
is.stock(itw_all)
itw_all

itw_2017 <- GetStockData("itw", from = "2017-01-01")
itw_2017
str(itw_2017)

library(ggplot2)
library(viridis)
plot(itw_2017)

itw_subsetted <- SubsetStock(itw, start = "2016-03-01")
str(itw_subsetted)
plot(itw_subsetted)
