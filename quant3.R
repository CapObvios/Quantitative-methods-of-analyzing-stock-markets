library(quantmod) 
library(fBasics) 
library(car) 
library(tseries) 
library(normtest) 
library(moments) 
library(forecast) 
library(lmtest)
library(MASS)
library(nnet)
library(randomForest)

MSFT = getSymbols('MSFT', src = 'yahoo', auto.assign = FALSE, from = as.Date("2010-01-01"), to = as.Date("2017-01-01"))
SP = getSymbols('^GSPC', src = 'yahoo', auto.assign = FALSE, from = as.Date("2010-01-01"), to = as.Date("2017-01-01"))

getLogReturns <-  function(data)
{
  return (na.omit(diff(log(data))))
}

MSFTLogRet <- getLogReturns(Ad(MSFT))
SPLogRet <- getLogReturns(Ad(SP))


getDirectionCoeffsVector <- function(data)
{
  N <- length(data)
  idx=c(1:N)[data>0]
  jdx=c(1:N)[data<=0]
  A=rep(0,N);A[idx]=1;A[jdx]=0
  return (A)
}

MsftDir <- getDirectionCoeffsVector(MSFTLogRet)
SpDir <- getDirectionCoeffsVector(SPLogRet)

#==================================================================================
#a)
# Usually the data is divided in the 2:1 proportions train and test data respectively. Let's roughly divide like that: 1200:561.
MsftTrainData <- cbind(MsftDir[1:1200], MSFTLogRet[1:1200])
MsftTestData <- cbind(MsftDir[1201:1761], MSFTLogRet[1201:1761])

SpTrainData <- cbind(SpDir[1:1200], SPLogRet[1:1200])
SpTestData <- cbind(SpDir[1201:1761], SPLogRet[1201:1761])

#==================================================================================
#b)
N <- length(MsftTrainData[,1])

createLagTable <- function(data1, data2)
{
  N <- length(data1[,1])
  return (cbind(Ai = as.vector(data1[,1][4:N]), Ai1 = as.vector(data1[,1][3:(N-1)]), Ai2 = as.vector(data1[,1][2:(N-2)]), Ai3 = as.vector(data1[,1][1:(N-3)]),
                Mi = as.vector(data2[,1][4:N]), Mi1 = as.vector(data2[,1][3:(N-1)]), Mi2 = as.vector(data2[,1][2:(N-2)]), Mi3 = as.vector(data2[,1][1:(N-3)])))
}

#Ai=MsftTrainData[,1][4:N];Aim1=MsftTrainData[,1][3:(N-1)];Aim2=MsftTrainData[,1][2:(N-2)];Aim3=MsftTrainData[,1][1:(N-3)]
#Mi=SpTrainData[,1][4:N];Mim1=SpTrainData[,1][3:(N-1)];Mim2=SpTrainData[,1][2:(N-2)];Mim3=SpTrainData[,1][1:(N-3)]

coeffsTable <- data.frame(createLagTable(MsftTrainData, SpTrainData))
testTable <-data.frame(createLagTable(MsftTestData, SpTestData))

msftGlm <- glm(Ai~Ai1+Ai2+Ai3+Mi1+Mi2+Mi3,family="binomial", data = coeffsTable)


msftGlmPredict <- predict(msftGlm, newdata = testTable, type="response")


m1P=rep("Down",length(testTable$Ai))
m1P[msftGlmPredict>0.5]="Up"
tb <- table(m1P,testTable$Ai)
ErrorRate <- (tb[1,2] + tb[2,1])/sum(tb)

#d)


msftLda <- lda(Ai~Ai1+Ai2+Ai3+Mi1+Mi2+Mi3, data = coeffsTable)
msftLdaPredict <- predict(object = msftLda, newdata = testTable)
msftLdaPredict$class

msftLdaP=rep("Down",length(testTable$Ai))
msftLdaP[msftLdaPredict$class==1]="Up"
tbLda <- table(msftLdaP,testTable$Ai)
ErrorRateLda <- (tbLda[1,2] + tbLda[2,1])/sum(tbLda)

#e)
msftQda <- qda(Ai~Ai1+Ai2+Ai3+Mi1+Mi2+Mi3, data = coeffsTable)
msftQdaPredict <- predict(object = msftQda, newdata = testTable)

msftQdaP=rep("Down",length(testTable$Ai))
msftQdaP[msftQdaPredict$class == 1]="Up"
tbQda <- table(msftQdaP,testTable$Ai)
ErrorRateQda <- (tbQda[1,2] + tbQda[2,1])/sum(tbQda)

#f) 

msft.x<-cbind(Ai1 = coeffsTable$Ai1,Ai2 = coeffsTable$Ai2,Mi1 = coeffsTable$Mi1, Mi2 = coeffsTable$Mi2)
msft.nn<-nnet(msft.x,coeffsTable$Ai1,size=2,linout=T,skip=T,maxit=10000,decay=1e-2,reltol=1e-7,abstol=1e-7,range=1.0)
summary(msft.nn)

msftNnetPredict <- predict(msft.nn,cbind(Ai1 = testTable$Ai1,Ai2 = testTable$Ai2,Mi1 = testTable$Mi1, Mi2 = testTable$Mi2))

msftNnetP=rep("Down",length(testTable$Ai))
msftNnetP[msftNnetPredict > 0.5]="Up"
tbNnet <- table(msftNnetP,testTable$Ai)
ErrorRateNnet <- (tbNnet[1,2] + tbNnet[2,1])/sum(tbNnet)

#=========================================================================================
#/////////////////////////////////////////////////////////////////////////////////////////
#=========================================================================================
#2

# I will take Microsoft stocks because I'm interested in their behavior

MsftTrainData2 <- MSFT[1:1200]
MsftTestData2 <- MSFT[1201:1761]


myATR <- function(x) ATR(HLC(x))[, "atr"]
mySMI <- function(x) SMI(HLC(x))[, "SMI"]
myADX <- function(x) ADX(HLC(x))[, "ADX"]
myAroon <- function(x) aroon(x[, c("High", "Low")])$oscillator
myBB <- function(x) BBands(HLC(x))[, "pctB"]
myChaikinVol <- function(x) Delt(chaikinVolatility(x[, c("High", "Low")]))[, 1]
myCLV <- function(x) EMA(CLV(HLC(x)))[, 1]
myEMV <- function(x) EMV(x[, c("High", "Low")], x[, "Volume"])[, 2]
myMACD <- function(x) MACD(Cl(x))[, 2]
myMFI <- function(x) MFI(x[, c("High", "Low", "Close")], x[, "Volume"])
mySAR <- function(x) SAR(x[, c("High", "Close")])[, 1]
myVolat <- function(x) volatility(OHLC(x), calc = "garman")[, 1]

T.ind <- function(quotes, tgt.margin = 0.02, n.days = 10) {
  v <- apply(HLC(quotes), 1, mean)
  r <- matrix(NA, ncol = n.days, nrow = NROW(quotes))
  for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x)
  x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x <
                                       -tgt.margin]))
  if (is.xts(quotes))
    xts(x, time(quotes))
  else x
}

#candleChart(last(MSFT, "3 months"), theme = "white", TA = NULL)
avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
addT.ind <- newTA(FUN = T.ind, col = "red", legend = "tgtRet")
addAvgPrice(on = 1)
addT.ind()



data.model <- specifyModel(T.ind(MSFT) ~ Delt(Cl(MSFT),k=1:10) + myATR(MSFT) + mySMI(MSFT)
                           + myADX(MSFT)+ myBB(MSFT)+ myCLV(MSFT)+CMO(Cl(MSFT))+EMA(Delt(Cl(MSFT)))+myVolat(MSFT)
                           + myMACD(MSFT)+ RSI(Cl(MSFT)) +runMean(Cl(MSFT))+runSD(Cl(MSFT)))


Tdata.train <- as.data.frame(modelData(data.model,
                                       data.window=c("2010-01-01",'2015-12-31')))

Tdata.eval <- na.omit(as.data.frame(modelData(data.model,
                                              data.window=c('2016-01-01',"2017-01-01"))))

Tform <- as.formula('T.ind.MSFT ~ .')

set.seed(1234)
rf <- buildModel(data.model,method='randomForest',
                 training.per=c("2010-01-01",'2015-12-31'),
                 ntree=50, importance=T)

varImpPlot(rf@fitted.model, type = 1)
imp <-importance(rf@fitted.model, type=1)
p <- predict(rf@fitted.model, Tdata.eval)

rownames(imp)[which(imp > 10)]

#========================================================================================
data.model.new <- specifyModel(T.ind(MSFT) ~ myATR(MSFT) + mySMI(MSFT) + myADX(MSFT)+ myMACD(MSFT) +runMean(Cl(MSFT)))

Tdata.train.new <- as.data.frame(modelData(data.model.new,
                                       data.window=c("2010-01-01",'2015-12-31')))

Tdata.eval.new <- na.omit(as.data.frame(modelData(data.model.new,
                                              data.window=c('2016-01-01',"2017-01-01"))))

set.seed(1234)
rf.new <- buildModel(data.model.new,method='randomForest',
                 training.per=c("2010-01-01",'2015-12-31'),
                 ntree=50, importance=T)

varImpPlot(rf.new@fitted.model, type = 1)
imp.new <-importance(rf.new@fitted.model, type=1)
p.new <- predict(rf.new@fitted.model, Tdata.eval)

rownames(imp.new)[which(imp.new > 10)]


#summary(rf)
#rfP <- predict(rf, Tdata.eval)


#rf <-randomForest(Tform, data=Tdata.train, ntree=50)
#p <- predict(rf, Tdata.eval)

varImpPlot(rf)

importance(rf)
