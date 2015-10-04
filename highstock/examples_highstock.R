require(rCharts)
require(quantmod)
require(plyr)

# set working directory 
setwd('set the working director')

## source the highstock class and plotting functions
# Function sourceDir is defined in the Examples section of ?source:
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
sourceDir(path = 'highstock/R/')

## Download data
# Specify period of time we are interested in
startDate <- as.Date("2010-01-01")
endDate   <- as.Date("2013-12-31")

# Define the tickers we are interested in
tickers <- c('AAPL')
getSymbols(tickers, src = "google", from = startDate, to = endDate)
stocks <- do.call(rbind, lapply(tickers, getQData(OHLCV)))
aapl <- subset(stocks, stock=='AAPL')

# 1. Standard candle plot 
p <- candlePlot(data = aapl, name='AAPL');p

# 2. add Volume to plot
hs.addVOL(p, data = aapl, height = 60);p

# 3. add moving average 
hs.addEMA(p, data = aapl, n = 10);p

# 4. add BBands 
hs.addBBands(p, data = aapl, n = 20, sd = 2);p

# 5. add MACD 
hs.addMACD(p, data = aapl);p
# moving average


