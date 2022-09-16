#install.packages("caret")
library(caret)

#lettura del dataframe da file .csv
btc<-read.csv(file = "./Data/BTC-USD.csv")

#rimozione attributi non necessari
btc$Adj.Close<-NULL

#trasformazione della data nel tipo Date
btc$Date<-as.Date(btc$Date)

#per ciascuna riga si aggiunge la settimana del corrisp. anno
btc$Week<-as.numeric(strftime(btc$Date, format = "%V"))

#Per fare in modo che la settimana degli ultimi giorni di dicembre non si accavalli con quella dei primi giorni di gennaio dell'anno successivo
#si prolunao l'ultima settimana di dicembre

#Es. di ciò che può capitare:
# 25/12/2014 (Giovedì della settimana n. 52) ... 28/12/2014 (Domenica della settimana n. 52) , 29/12/2014 (Lunedì della settimana n. 1)
#in pratica si vuole che la settimana n.1 parta dal primo giorno del nuovo anno, ovvero dal 01/01/2015

x<-btc[as.numeric(format(btc$Date,'%m'))==12 & as.numeric(format(btc$Date,'%d'))>=28,"Week"]
for (t in 2:length(x)){
  if(x[t]==1)
    x[t]<-x[t-1]
}
btc$Week[as.numeric(format(btc$Date,'%m'))==12 & as.numeric(format(btc$Date,'%d'))>=28]<-x

#Problema speculare al precedente: se si vuole che i primi giorni di gennaio non abbiano la stessa settimana degli ultimi giorni di dicembre dell'anno precedente
#si prolunga la prima settimana di gennaio

#Es. di ciò che può accadere:
# 01/01/2016 (Venerdì della settimana n.52 dell'anno 2015)
x<-btc[as.numeric(format(btc$Date,'%m'))==1 & as.numeric(format(btc$Date,'%d'))<=4,"Week"]
for (t in 2:length(x)){
  if(x[length(x)-t+1]>=52)
    x[length(x)-t+1]<-x[length(x)-t+2]
}

btc$Week[as.numeric(format(btc$Date,'%m'))==1 & as.numeric(format(btc$Date,'%d'))<=4]<-x


#calcolo il max prezzo settimanale
high<-aggregate(btc$High,by=list(btc$Week,format(btc$Date,'%Y')),FUN=max)
colnames(high)<-c("Week","Year","High")

#calcolo il minimo prezzo settimanale
low<-aggregate(btc$Low,by=list(btc$Week,format(btc$Date,'%Y')),FUN=min)
colnames(low)<-c("Week","Year","Low")

#calcolo la media dei volumi settimanali
vol<-aggregate(btc$Volume,by=list(btc$Week,format(btc$Date,'%Y')),FUN=mean)
colnames(vol)<-c("Week","Year","Volume")

#calcolo il prezzo di apertura settimanale (coincide con il prezzo di apertura del primo giorno della settimana)
opn<-aggregate(btc$Open,by=list(btc$Week,format(btc$Date,'%Y')),FUN=head,1)
colnames(opn)<-c("Week","Year","Open")

#calcolo il prezzo di chiusura settimanale (coincide con il prezzo di chiusura dell'ultimo giorno della settimana)
cls<-aggregate(btc$Close,by=list(btc$Week,format(btc$Date,'%Y')),FUN=tail,1)
colnames(cls)<-c("Week","Year","Close")



#join dei nuovi valori su base settimanale
btc.week<- inner_join(high,low)
btc.week<-inner_join(btc.week,vol)
btc.week<-inner_join(btc.week,opn)
btc.week<-inner_join(btc.week,cls)

#rimozione dei campi Year e Week perché non costituiranno features utili
btc.week<-btc.week[,-c(1,2)]

#scaling del dataset
btc.week$High<-(btc.week$High-min(btc.week$High))/(max(btc.week$High)-min(btc.week$High))
btc.week$Low<-(btc.week$Low-min(btc.week$Low))/(max(btc.week$Low)-min(btc.week$Low))
btc.week$Volume<-(btc.week$Volume-min(btc.week$Volume))/(max(btc.week$Volume)-min(btc.week$Volume))
btc.week$Open<-(btc.week$Open-min(btc.week$Open))/(max(btc.week$Open)-min(btc.week$Open))

#shuffling del dataset
set.seed(689)
btc.week<-btc.week[sample(1:nrow(btc.week)), ]

#split del dataframe in modo da avere 75% training e 25% test
set.seed(789)
training.indexes<-createDataPartition(btc.week$Close,p=.75,list=FALSE)
btc.week.training<-btc.week[training.indexes,]
btc.week.testing<-btc.week[-training.indexes,]

#salvataggio del nuovo dataset su disco
write.csv(btc.week,"./Output/Data/btc_week.csv",row.names = FALSE)

#salvataggio dei dataset di training e test su disco
write.csv(btc.week.training,"./Output/Data/btc_week_tr.csv",row.names = FALSE)
write.csv(btc.week.testing,"./Output/Data/btc_week_te.csv",row.names = FALSE)












