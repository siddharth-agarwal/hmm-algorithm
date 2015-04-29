library(PerformanceAnalytics)
runModel <- function(stockData,states, startTraining, endTraining, trainDays, algoDays, tradeBound)
  
  kTimeframe = algoDays
  kStates = states
  kUpper.bound = tradeBound

  trend <- 1
  
  prev.capital <- 100
  capital <- 100
  prev.benchmark <- 100
  benchmark <- 100
  
  returns <- 0
  rolling.avg = 0.5
  mvg.avg <- rep(0.5,times=3)
  
  sharpe <- data.frame(date= numeric(kTimeframe), returns= numeric(kTimeframe))
  returns.to.graph <- data.frame(date= numeric(kTimeframe), returns.time= numeric(kTimeframe), benchmark.time = numeric(kTimeframe))
  
  correct <- 0
  incorrect <- 0
  correctReturns <- vector(mode = "numeric")
  incorrectReturns <- vector(mode = "numeric")
  
  for(i in 1:kTimeframe) {
    startDate2 = NextTradingDate(startTraining)
    trainingEndDate2 = NextTradingDate(endTraining)
    
    inSampleMktData <-  window(stockData,start=startDate2 ,end=trainingEndDate2)
    outOfSampleMktData <-  window(stockData,start=trainingEndDate2+1)
    
    returns.to.graph[i,1] = startDate2
    sharpe[i,1] = startDate2
    
    HMM <- depmix(logret~1, family = gaussian(), nstates = kStates, data = inSampleMktData)
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
    
    x <- coredata(outOfSampleMktData$logret)
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
      capital = BullMarket(bull.obs,capital,trend)
    }
    
    if ((1-rolling.avg) > kUpper.bound){
      capital = BearMarket(bear.obs,capital,trend)
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
    return(returns.to.graph)
  }