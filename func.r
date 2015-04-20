library(timeDate)
library(TTR)

TradingDates <- function(year=format(Sys.Date(), "%Y"), FUN=holidayNYSE) {
  # the next few lines should be removed when this code is added to a package
  # that Imports timeDate
  if (!"package:timeDate" %in% search()) {
    suppressPackageStartupMessages({ 
      if (!require(timeDate)) {
        stop("timeDate must be installed to use this function.")
      }
    })
    on.exit(detach(package:timeDate, unload=TRUE))
  }
  ## End of code that should be removed when this is added to a package
  year <- as.numeric(year)
  fun <- match.fun(FUN)
  do.call('c', lapply(year, function(y) {
    holidays <- as.Date(fun(year=y))
    all.days <- seq.Date(as.Date(paste(y, '01-01', sep='-')), 
                         as.Date(paste(y, '12-31', sep='-')), by='days')
    nohol <- all.days[!all.days %in% holidays]
    nohol[!format(nohol, '%w') %in% c("6", "0")] #neither holiday nor weekend
  }))
}
#' @export
#' @rdname PrevTradingDate
NextTradingDate <- function(Date, n=1) {
  stopifnot(require(xts)) #remove this line when this is added to a package that Imports xts (needed for first/last)
  D <- as.Date(Date)
  y <- as.numeric(format(D, "%Y"))
  trading.days <- TradingDates(y)
  out <- trading.days[trading.days > Date]
  if (length(out) >= n) {
    last(first(out, n))
  } else { 
    next.year.td <- TradingDates(y + 1)
    max.n <- length(out) + length(next.year.td)
    new.n <- n - length(out) # how many trading days we need from next year
    if (n > max.n) stop("'n' is too large. Try something less than 252.")
    # if it's the last trading day of the year, return the first trading date of
    # next year
    last(first(TradingDates(y + 1), new.n))
  }
}

CalculateReturns <- function(vec,cap,benchmark){
  correct <- 0
  incorrect <- 0
  null <- 0
  
  for (i in 1:length(vec)){
    if (vec[i] == "1")
      correct = correct + 1
    else if (vec[i] == "0")
      incorrect = incorrect + 1
    else
      null = null + 1
  }
  
  #print (correct/length(vec))
  #print (incorrect/length(vec))
  #print (null/length(vec))
  
  cat("Capital:",cap)
  cat("Benchmark:",benchmark)
}

BullMarket <- function(bull.obs,capital,consec.pos){
  if (consec.pos > 0){
    if(x[1] < 0){
      transactions = transactions + 1
      correct <- 0
    }
    else{
      correct <- 1
    }
    capital = ((1+x[1]) * capital)
  }
  return(capital)
}

BearMarket <- function(bear.obs,capital,consec.neg){
  correct = 2
  if (consec.neg > 0){
    if(x[1] > 0){
      transactions = transactions + 1
      correct <- 0
    }
    else{
      correct <- 1
    }
    capital = ((1+(-1*x[1])) * capital)
  }
  return(capital)
}

DetermineTrend <- function(prev.cap,capital,trend){
  
  diff = capital - prev.cap
  if(prev.cap <= capital){
    if (trend < 1){
      trend = 1
    }
    else{
      trend = trend + 1
    }
  }
  else{
    if(trend > 1){
      trend = -1
    }
    else {
      trend = trend - 1
    }
  }
  return(trend)
}

RepopAvg <- function(average,bull.obs){
  average[3] = average[2]
  average[2] = average[1]
  average[1] = bull.obs
  return(average)
}

RecalcAvg <- function(inputs,average){
  average = mean(inputs)
  return(average)
}

CalcReturns <- function(prev.cap,capital){
  returns = ((capital-prev.cap)/capital)
  return(returns)
}