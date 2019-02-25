#PART1
#DOWNLOAD YOU DATA

#1.1
library(TSA)
#look at eacf
eacf(rt);
#check your variants with aic
"AIC";arima(rt, c(0,0,0))$aic; arima(rt, c(0,0,1))$aic;
arima(rt, c(0,0,2))$aic; arima(rt, c(0,0,3))$aic;#my variants
arima(rt, c(1,0,0))$aic;arima(rt, c(1,0,1))$aic;#my variants
arima(rt, c(1,0,2))$aic;arima(rt, c(0,0,3))$aic;#my variants
arima(rt, c(2,0,0))$aic;arima(rt, c(2,0,1))$aic;#my variants
arima(rt, c(2,0,2))$aic;arima(rt, c(2,0,3))$aic;#my variants
arima(rt, c(3,0,0))$aic;arima(rt, c(3,0,1))$aic;#my variants
arima(rt, c(3,0,2))$aic;arima(rt, c(3,0,3))$aic;#my variants
#ar - лаги значеиний ma - лаги ошибок

#1.2 arch effect 
acf((ma_mod_ref$res)^2)

#1.3 
library(fGarch)
summary(garchFit(formula=~arma(1,1)+garch(1,0), data=rt, trace=F))
summary(garchFit(formula=~arma(1,1)+garch(1,1), data=rt, trace=F))
summary(garchFit(formula=~arma(1,1)+garch(1,2), data=rt, trace=F))
summary(garchFit(formula=~arma(1,1)+garch(2,0), data=rt, trace=F))
summary(garchFit(formula=~arma(1,1)+garch(2,1), data=rt,trace=F ))
summary(garchFit(formula=~arma(1,1)+garch(2,2), data=rt, trace=F))

arma_garch_mod<-garchFit(formula=~arma(,)+garch(,), data=rt, trace=F)

#1.4 
#mu -  интерсепт в арме
#arx - coed for ar 
#max coef fro ma


#1.5
#beta - dispersia
#omega- intercept
#alpha - for e

#1.6
#click on model or
summary(model)

#1.7 
#click on model or
summary(model)
#estimate+-2st err 

#1.8
normalTest(residuals(arma_garch_mod), method = 'jb', na.rm = TRUE)
#HO - normal distribution of residuals

#1.9
library(forecast)
frct<-predict(arma_garch_mod, 3)
frct

#1.10
#DO NOT DO IT GGGUUUUYYY

#PART2
#download data pico
lr_pico<-diff(log(Cl(pico)))
lr_pico<-na.omit(lr_pico)
#to 1 and 0 
idP<-c(1:length(lr_pico))[lr_pico>0]
jdP<-c(1:length(lr_pico))[lr_pico<=0]
lrp_adj<-rep(0,length(lr_pico));lrp_adj[idP]=1;lrp_adj[jdP]=0

N<-length(lr_pico)
L0<-lrp_adj[3:N,1];
L1<-lr_pico[2:(N-1),1];
L2<-lr_pico[1:(N-2),1]


dt<-cbind(A1=as.vector(L0),A2=as.vector(L1),A3=as.vector(L2));
#splitting to 2 subsets
train<-head(dt,length(dt[,1])-200)
test<-tail(dt,200)
#model building
model<-glm(A1~A2+A3,data=as.data.frame(train), family='binomial') #check just data.frame without as 
#preiction
prediction<-predict(model, data.frame(test),type="response")


#2.1 
#check models coef by model cbtr+enter or
summary(model)
#model is e^(intercepl+coefA1*A1+coefA2*A2)/(1+e^(intercepl+coefA1*A1+coefA2*A2))

#2.2
confint.lm(model)

#2.3
tail(prediction,1)

#2.4
#make vector of resilts with up and down
pred_res<-rep("Down",200)
pred_res[prediction>0.5]<-"Up"
#make table with real data
res<-table(as.numeric(test[,1]),pred_res)
#compute correctness
precision<-((res[1,1]+res[2,2])/(res[1,2]+res[2,1]+res[1,1]+res[2,2]))
#ANSWER
precision

#2.5
library(MASS)
#model building
model_lda<-lda(A1~A2+A3,data=as.data.frame(train)) #check just data.frame without as 
#preiction
prediction_lda<-predict(model_lda, data.frame(test))
#make vector of results with up and down
pred_res_lda<-rep("Down",200)
pred_res_lda[prediction_lda$class==1]<-"Up"
#make table with real data
res_lda<-table(as.numeric(test[,1]),pred_res_lda)
#compute correctness
precision_lda<-((res_lda[1,1]+res_lda[2,2])/(res_lda[1,2]+res_lda[2,1]+res_lda[1,1]+res_lda[2,2]))
#ANSWER
precision_lda


#2.6
#ANSWER
tail(prediction_lda$class, 1)


#2.7
#model building
model_qda<-qda(A1~A2+A3,data=as.data.frame(train)) #check just data.frame without as 
#preiction
prediction_qda<-predict(model_qda, data.frame(test))
#make vector of results with up and down
pred_res_qda<-rep("Down",200)
pred_res_qda[prediction_qda$class==1]<-"Up"
#make table with real data
res_qda<-table(as.numeric(test[,1]),pred_res_qda)
#compute correctness
precision_qda<-((res_qda[1,1]+res_qda[2,2])/(res_qda[1,2]+res_qda[2,1]+res_qda[1,1]+res_qda[2,2]))
#ANSWER
precision_qda

#2.8
#ANSWER
head(tail(prediction_qda$class,2),1)



