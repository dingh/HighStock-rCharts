# add Volume
hs.addVOL <- function
(
  rChart, 
  data,
  height = 80,
  yAxis = 'Volume'
)
{
  rChart$params$height <- rChart$params$height + height;
  top = 0;  
  for (i in rChart$params$yAxis) {
    top = top + i$height
  } 
  
  rChart$yAxis(title = list(text = "Volume"), top = top + 70 , height = height, offset = 0, 
               linewidth = 2, id = yAxis, replace = F)  
  rChart$series(data = toJSONArray2(data[c("date", "Volume")], 
                                    json = F, names = F, replace = F), type = "column", name = "Volume", 
                yAxis = 'Volume', marker = list(radius = 4))
}

# add EMA to the main plot
hs.addEMA <- function
(
  rChart, 
  data, 
  yAxis = 'OHLC', 
  n = 20
)
{
  
  ema <- EMA(data$Close, n)
  rChart$series(data = toJSONArray2(data.frame(data$date, ema), json = F, names = F), 
                name = paste('EMA(', n,')', sep = ''), type = 'line',  yAxis = yAxis, 
                marker = list(radius = 4))
}

# add BBands to the main plot
hs.addBBands <- function
(
  rChart, 
  data, 
  yAxis = "OHLC", 
  n = 20, 
  sd = 2, 
  maType = "SMA" 
)
{
  HLC <- data[c("High", "Low", "Close")]
  bb <- BBands(HLC, n = n, maType = maType, sd = sd)
  rChart$series(data = toJSONArray2(data.frame(data$date, bb[,1]), json = F, names = F), 
                name = paste('dn(', n,')', sep = ''), type = 'line', lineWidth = 1, yAxis = yAxis, color = 'black',
                marker = list(radius = 4))
  rChart$series(data = toJSONArray2(data.frame(data$date, bb[,2]), json = F, names = F), 
                name = paste(maType,'(', n,')', sep = ''), dashStyle = 'shortdot', lineWidth = 1, yAxis = yAxis, 
                marker = list(radius = 4))
  rChart$series(data = toJSONArray2(data.frame(data$date, bb[,3]), json = F, names = F), 
                name = paste('up(', n,')', sep = ''), type = 'line', lineWidth = 1, yAxis = yAxis, color = 'black',
                marker = list(radius = 4))
}

# add addMACD to the main plot
hs.addMACD <- function
(
  rChart, 
  data, 
  fast = 12, 
  slow = 26, 
  signal = 9, 
  type = "EMA", 
  percent = TRUE,
  height = 100
)
{
  macd <- MACD(data$Close, nFast = fast, nSlow = slow, nSig = signal, 
               maType = type)
  macd.histogram <- macd[,1] - macd[,2] 
  
  rChart$params$height <- rChart$params$height + height;
  # get the top
  top = 0;
  for (i in rChart$params$yAxis) {
    top = top + i$height
  } 
  
  rChart$yAxis(title = list(text = "MACD"), top = top + 70, height = height, offset = 0, linewidth = 2, id = 'MACD', replace = F)
  
  rChart$series(data = toJSONArray2(data.frame(data$date, macd.histogram), json = F, names = F, replace = F), 
                type = "column", name = "MACD histogram", yAxis = 'MACD', marker = list(radius = 4))
  
  rChart$series(data = toJSONArray2(data.frame(data$date, macd[,1]), json = F, names = F, replace = F), 
                type = "line", name = "MACD", yAxis = 'MACD', marker = list(radius = 4))
  rChart$series(data = toJSONArray2(data.frame(data$date, macd[,2]), json = F, names = F, replace = F), 
                type = "line", name = "Signal", yAxis = 'MACD', marker = list(radius = 4))
}

#download tickers
getQData <- function(fun)
{
  myfunc <- function(name)
  {
    # Remoe stock name from columns
    dt            <- get(name)
    colnames(dt)  <- gsub(paste(name, ".", sep=""), "", colnames(dt), fixed=T)    
    # Call quantmod function
    tmp           <- fun(dt)    
    stock         <- data.frame(t=index(tmp), coredata(tmp))
    stock$date    <- as.numeric(as.POSIXct(stock$t, origin="1970-01-01")) * 1000
    stock$stock   <- name    
    return( stock )
  }
  
  return( myfunc )
}

