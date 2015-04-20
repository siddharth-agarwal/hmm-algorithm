library(quantmod)
library(depmixS4)

symbol = "^GSPC"

startDate2 = as.Date("2006-9-01")
trainingEndDate2 = as.Date("2006-12-01")
getSymbols(symbol, src = "yahoo", from = startDate2)
mktdata <- head(GSPC,-1)

dailyRet <- Delt(Cl(GSPC),k=1,type="arithmetic")
dailyRet$logret <- Delt(Cl(GSPC),k=1,type="arithmetic")
dailyRet$logvol <- Delt(GSPC$GSPC.Volume,k=1,type="arithmetic")
dailyRet$logvol <- (-dailyRet$logvol)

dailyRet <- na.omit(dailyRet)

for(i in 1:nrow(dailyRet)) {
  if (dailyRet$logret[i] > 0)
  {
    dailyRet$discrete[i] <- 1
  }
  else
  {
    dailyRet$discrete[i] <- 0
  }
}