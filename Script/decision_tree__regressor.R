#DECISION TREE REGRESSOR
#install.packages("caret")
#install.packages("rpart.plot")
library(caret)
library(rpart.plot)

#esecuzione dello script di preprocessing del dataset
source("./Script/preprocessing.R")

#lettura dei dataset
btc<-read.csv(file="./Output/Data/btc_week.csv")
btc.training<-read.csv(file="./Output/Data/btc_week_tr.csv")
btc.testing<-read.csv(file="./Output/Data/btc_week_te.csv")

#separazione dei predittori dalla risposta
btc.test.features<-subset(btc.testing, select=-Close)
btc.test.target<-subset(btc.testing, select=Close)[,1]

#definizione dei parametri del metodo repeated cross validation (10 fold e 5 ripetizioni)
fitControl<-trainControl(method = "repeatedcv", number = 10, repeats = 5)

#set del seed per ottenere sempre gli stessi risultati
set.seed(111)

#creazione del modello
model<- train(Close~High+Low+Volume+Open, data=btc.training, method="rpart", trControl=fitControl, tuneLength=10)

#stampa del modello
model$finalModel
model

#plot del tuning dei parametri del modello
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

#plot dell'albero decisionale
prp(model$finalModel, main= "Albero di decisione con cp=0.00029",box.palette = "Reds", tweak = 1.1, type=4, extra = 1)

#plot dei residui in funzione del valore predetto
plot(predictions,resid, col="blue1", main="DTR: Dispersione delle predizioni",ylim=c(-6000,6000),xlab="Valori Predetti", ylab="Residui")
abline(0,0, col="red")