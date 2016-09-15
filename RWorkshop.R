loadBankMarketingData <- function()
{
  bankData <- read.csv("C:/RWorkshop/BankMarketing.csv")
  numberCols <- sapply(bankData, function(x) {return(is.numeric(x) | is.integer(x))})
  bankData[numberCols] <- sapply(bankData[numberCols], scale)
  return(bankData)
}

trainBankModel <- function()
{
  bankData <- loadBankMarketingData()
  testIndices <- sample(nrow(bankData), 900)
  testSet <- bankData[testIndices,]
  trainingSet <- bankData[-testIndices,]
  bankModel <- nnet(Y~., trainingSet, size=10, MaxNWts=5000, maxit=2000)
  return(bankModel)
}

bankModel <- trainBankModel()
plot(bankModel)