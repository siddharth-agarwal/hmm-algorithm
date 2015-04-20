library(ggplot2)
library(reshape2)

ggplot(returns.to.graph, aes(as.Date(date))) + 
  geom_line(aes(y = benchmark.time, colour = "benchmark")) + 
  geom_line(aes(y = returns.time, colour = "returns"))

ggplot(returns.to.graph, aes(as.Date(date))) + 
  geom_line(aes(y = benchmark.time, colour = "benchmark"))

plot(transition.matrix$state, type="s")

plot(dailyRet, type="line")

plot(inSampleMktData$bull.prob, type="s")