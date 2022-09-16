#SUPPORT VECTOR REGRESSOR
#install.packages("caret")
library(caret)

#esecuzione dello script di preprocessing del dataset
source("./Script/preprocessing.R")

#lettura dei dataset
btc<-read.csv(file="./Output/Data/btc_week.csv")
btc.training<-read.csv(file="./Output/Data/btc_week_tr.csv")
btc.testing<-read.csv(file="./Output/Data/btc_week_te.csv")

#separazione dei predittori dalla risposta
btc.test.features<-subset(btc.testing, select=-Close)
btc.test.target<-subset(btc.testing, select=Close)[,1]

#definizione del modello di validazione
fitControl<-trainControl(method = "repeatedcv", number = 10, repeats = 5)

#definizione dei valori dei parametri su cui fare tuning (decommentare solo quella relativa al modello che si vuole usare)
grid.linear<-expand.grid(C=c(0.01,0.05,0.1,0.25,0.5,0.75,1,1.5,2,5))
#grid.poly<-expand.grid(C=c(0.01,0.05,0.1,0.25,0.5,0.75,1,1.5,2,5),scale=c(0.1,0.2,0.5,0.6),degree=c(2,3))
#grid.radial<-expand.grid(C=c(0.01,0.05,0.1,0.25,0.5,0.75,1,1.5,2,5),sigma=c(0.1,0.2,0.5,0.7,1,1.5,2,2.5,3,5))

#set del seed per ottenere sempre gli stessi risultati
set.seed(111)

#creazione del modello (decommentare solo il modello che si vuole usare)
model<-train(Close~High+Low+Volume+Open, data=btc.training, method="svmLinear", trControl=fitControl, tuneGrid=grid.linear)
#model<- train(Close~High+Low+Volume+Open, data=btc.training, method="svmPoly", trControl=fitControl, tuneGrid=grid.poly)
#model<- train(Close~High+Low+Volume+Open, data=btc.training, method="svmRadial", trControl=fitControl, tuneGrid=grid.radial)

#stampa del modello
model$finalModel
model

#plot del tuning dei parametri
plot(model)

#calcolo delle misure di performance del modello sul test set
predictions<-predict(model,btc.test.features)
results<-data.frame(Real=btc.test.target, Predicted=predictions)
resid<-btc.test.target - predictions
rmse<-sqrt(mean((resid)^2))
r2<-cor(btc.test.target, predictions)^2
mae<- mean(abs(btc.test.target - predictions))

#stampa delle misure di performance
rmse
r2
mae

#plot dei residui in funzione del valore predetto (decommentare solo quello relativo al modello usato)
plot(predictions,resid, col="blue1", main="SVR Linear: Dispersione delle predizioni",ylim=c(-6000,6000),xlab="Valori Predetti", ylab="Residui")
abline(0,0, col="red")

#plot(fitted, resid, col="blue1", main="SVR Polynomial: Dispersione delle predizioni", ylim=c(-6000,6000),xlab="Valori Predetti", ylab="Residui")
#abline(0,0, col="red")

#plot(fitted, resid, col="blue1", main="SVR Radial: Dispersione delle predizioni", ylim=c(-6000,6000),xlab="Valori Predetti", ylab="Residui")
#abline(0,0, col="red")
