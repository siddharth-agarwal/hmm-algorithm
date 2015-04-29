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

BullMarket <- function(bull.obs,capital,consec.pos,x1){
  if (consec.pos > 0){
    if(x1 < 0){
      correct <- 0
    }
    else{
      correct <- 1
    }
    capital = ((1+x1) * capital)
  }
  return(capital)
}

BearMarket <- function(bear.obs,capital,consec.neg, x1){
  correct = 2
  if (consec.neg > 0){
    if(x1 > 0){
      correct <- 0
    }
    else{
      correct <- 1
    }
    capital = ((1+(-1*x1)) * capital)
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

GetData <- function(symbol, startdate){
  start.date = as.Date(startdate)
  data = getSymbols(symbol, src = "yahoo", from = start.date, env=NULL, return.class="xts")
  data$Log.Returns <- Delt(Cl(data),k=1,type="arithmetic")
  return(data)
}

runModel <- function(stockData,states, startTraining, endTraining, algoDays){
  kTimeframe = algoDays
  kStates = states
  kUpper.bound = 0.7
  
  startDate2 = startTraining
  trainingEndDate2 = endTraining
  
  trend <- 1
  
  prev.capital <- 100
  capital <- 100
  prev.benchmark <- 100
  benchmark <- 100
  
  returns <- 0
  rolling.avg = 0.5
  mvg.avg <- rep(0.5,times=3)
  
  sharpe <- data.frame(date= numeric(kTimeframe), returns= numeric(kTimeframe))
  returns.to.graph <- data.frame(returns.date= list(kTimeframe), returns.time= numeric(kTimeframe), benchmark.time = numeric(kTimeframe))
  
  correct <- 0
  incorrect <- 0
  correctReturns <- vector(mode = "numeric")
  incorrectReturns <- vector(mode = "numeric")
  
  for(i in 1:kTimeframe) {
    startDate2 = NextTradingDate(startDate2)
    trainingEndDate2 = NextTradingDate(trainingEndDate2)
    
    inSampleMktData <-  window(stockData,start=startDate2 ,end=trainingEndDate2)
    outOfSampleMktData <-  window(stockData,start=trainingEndDate2+1)
    
    returns.to.graph[i,1] = as.character.Date(startDate2)
    sharpe[i,1] = startDate2
    
    HMM <- depmix(Log.Returns~1, family = gaussian(), nstates = kStates, data = inSampleMktData)
    set.seed(1)
    fit.model <- try(fit(HMM, verbose = FALSE))
    if(class(fit.model) == "try-error") {
      returns.to.graph[i,2] = prev.capital
      returns.to.graph[i,3] = prev.benchmark
      print("error")
      next;
    }
    transition.matrix <- posterior(fit.model)             # Compute probability of being in each state
    
    bull.prob = transition.matrix$S1
    bear.prob = transition.matrix$S2
    
    inSampleMktData$bull.prob = bull.prob
    inSampleMktData$bear.prob = bear.prob
    inSampleMktData <- na.omit(inSampleMktData)
    
    x <- coredata(outOfSampleMktData$Log.Returns)
    bull <- coredata(inSampleMktData$bull.prob)
    bear <- coredata(inSampleMktData$bear.prob)
    
    benchmark = benchmark * (1+ x[1])
    
    returns.to.graph[i,2] = capital
    returns.to.graph[i,3] = benchmark
    
    bull.obs = bull[length(bull)]
    bear.obs = bear[length(bear)]
    
    mvg.avg = RepopAvg(mvg.avg,bull.obs)
    c <- EMA(mvg.avg,2)
    rolling.avg = c[3]
    
    prev.capital <- capital
    prev.benchmark <- benchmark
    
    if (rolling.avg > kUpper.bound){
      capital = BullMarket(bull.obs,capital,trend,x[1])
    }
    
    if ((1-rolling.avg) > kUpper.bound){
      capital = BearMarket(bear.obs,capital,trend,x[1])
    }
    returns <- CalcReturns(prev.capital,capital)
    
    if (returns < 0){
      incorrect = incorrect + 1
      incorrectReturns[i] = returns
    } else if (returns > 0) {
      correct = correct + 1
      correctReturns[i] = returns
    }
    sharpe[i,2] = returns
  }
  return(returns.to.graph)
}