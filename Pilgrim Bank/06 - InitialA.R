# ----- PILGRIM BANK A
dtab = read.csv("06 - PilgrimABC.csv")
#rinomico colonne per evitare problemi con i nomi
Profit=dtab$X9Profit
Online=dtab$X9Online
Age=dtab$X9Age
Income=dtab$X9Inc
Tenure=dtab$X9Tenure
District=dtab$X9District
summary(Profit)
#il primo quartile è negativo, vedo bene che è asimmetrico visto che media e mediana sono molto diversi
hist(Profit)
#c'è un discreto numero di clienti che mi fanno perdere, è molto scodato
#-------
N=length(Profit)
Nprofitable = sum(Profit>0)
cat('profitable = ', Nprofitable, ' out of ', N, '\n')
# Pareto curve
cumprofit=cumsum(sort(Profit,decreasing=TRUE))*100/sum(Profit)
plot(100*(1:N)/N,cumprofit,type='l')
grid()
# mean profits
cat('average profit ', mean(Profit), '\n')
ProfitOnline = Profit[Online==1]
cat('average profit ON', mean(ProfitOnline), '\n') #verifichiamo quanto detto nel Business Case
ProfitOffline = Profit[Online==0]
cat('average profit OFF', mean(ProfitOffline), '\n')
#intervallo di confidenza per vedere se i dati sono sufficienti o no, 
# se intervallo grande avrei bisogno di più clienti
cat('Conf int profit ', t.test(Profit)$conf.int, '\n')
cat('p-value difference ',t.test(ProfitOnline,ProfitOffline)$p.value, '\n')
#t.test su due popolazioni, vado a vedere p-value, il p-value è alto e quindi vedo che 
# NON c'è una differenza significativa, quindi non rifiuto l'ipotesi nulla sulla differenza tra le medie

mod = lm(Profit ~ Online)
summary(mod)
#intercetta = media offline
#differenza è 6 dollari, p-value simile ma calcolato in modo diverso da prima
#variabile non significativa forse perché metto insieme clienti molto diversi, quindi controllo per età

#control for age
mod = lm(Profit ~ Online+Age)
summary(mod)
#sembrerebbe ci sia differnza tra online e offline grazie all'effetto di Age
# significatività sia statistica che business
# età diverse potrebbero implicare redditi diversi
# giovane usa di più online ma ha un reddito minore quindi fa guadagnare di meno
#32mila osservazioni ma solo 23mila g.d.l.
# 8289 ossrvazioni eliminate per missingness, qualche dato è mancante
head(Age) #valori mancanti segnati come NA

Age1 = as.factor(Age)
summary(Age1)
mod = lm(Profit ~ Online+Age1)
summary(mod)
#age12,13,... sono binarie, ci dà tante informazioni

# check profitability of high and low brackets
lapply(split(Profit,as.factor(Age)),mean) 
#lapply=list apply, splitto in gruppi a seconda delle Age, ottengo una lista??? e ci applico mean
# non considero online e offline
# sembra che i giovani siano meno profittevoli degli anziani

#il 20% dei giovani usano l'online e scende per le fasce di età successive
lapply(split(Online,as.factor(Age)),mean)


# Potential bias due to missing data!
# perché mancano le età? conto cointestato, conto società, ...?
# magari c'è un motivo per cui non mi hanno dato l'età
sum(is.na(Age))
AgeGiven = ifelse(is.na(Age),0,1) # 0 dove c'è NA, 1 se c'è l'età
mod = lm(Profit ~ AgeGiven)
#cerco di capire se c'è una differenza statisticamente significativa sul profitto tra dove c'è 
# l'età e dove non c'è
summary(mod)
#se butto via i dati dove manca l'età sto distorgendo l'analisi
#non è uniforme chi ha fornito i dati e chi no
lapply(split(Profit,as.factor(AgeGiven)),mean)
# profittabilità media più alta tra chi mi ha dato l'età

#Data Imputation = cerco di riempire i missing
#potremmo creare un modello di regressione per trovare i valori mancanti

# Replace missing with Zero
AgeZero = ifelse(is.na(Age),0,Age) 
#dove c'è NA metto 0, ma essendo ordinali non ha senso, sembra che chi non dato l'età è un neonato
table(AgeZero)
mod = lm(Profit ~ Online+AgeZero)
summary(mod)
#ci è venuto questo valore di online perché ho scelto arbitrariamente di mettere 0

# Replace missing with mean
mm = mean(Age, na.rm=TRUE)
#na.rm=TRUE mi fa la media senza considerare i NA, 
# anche se non ha molto senso perché è discretizzato, ma rimane comunque ordinato
AgeAverage = ifelse(is.na(Age),mm,Age)
table(AgeAverage)
mod = lm(Profit ~ Online+AgeAverage)
summary(mod)
#cambia il coefficiente di Online quindi a seconda di come tappo il buco esce un risultato diverso 
# quindi non è così che dobbiamo procedere

# control for AgeGiven
mod = lm(Profit ~ Online+AgeZero+AgeGiven)
summary(mod)
#Online è venuto 19
#questo è un modo per considerare AgeZero come categorico

mod = lm(Profit ~ Online+AgeAverage+AgeGiven)
summary(mod)
#Online viene 19 come prima
#ho tappato il buco in modo arbitrario per poter fare la regressione, 
# ma poi la considero come categorica
#R^2 è miserevole quindi il modello spiega il 2.5% della variabilità

# Deal with missing income
#faccio lo stesso ragionamento di prima
IncomeZero = ifelse(is.na(Income),0,Income)
IncomeGiven = ifelse(is.na(Income),0,1)
mod = lm(Profit ~ Online+AgeAverage+AgeGiven+IncomeZero+IncomeGiven)
summary(mod)
#Online scende un po', R^2 è cresciuto, è raddoppiato ma rimane sempre piccolo

# Control for Tenure and district
any(is.na(District))
table(District) #ha tre valori e nessun NA, ma sono considerati numerici
# quindi devo creare varibili categoriche
any(is.na(Tenure))
District1100 = ifelse(District==1100,1,0)
District1200 = ifelse(District==1200,1,0)
mod = lm(Profit ~ Online+AgeAverage+AgeGiven+IncomeZero+IncomeGiven+Tenure+District1100+District1200)
summary(mod)

#qualcosa di R^2 lo grattiamo ma poco
#Online è circa 13

#potremmo considerare Age come categorico
#potremmo considerare il rapporto tra Online e Age

#un valore di soglia dell'R^2 non c'è, dipende se migliora la mia prestazione economica

################ PARTE B #################
#CUSTOMER RETENTION: quali sono le strategie giuste per mantenere un cliente
#customer churn: cliente ci molla
#costa di meno mantenere un cliente rispetto ad acquisirne di nuovi
#cerco di capire quali clienti rischio di perdere

#usiamo anche i dati dell'anno successivo
# vediamo se i clienti sono fedeli o no

#mod1 con R^2 basso
#mod2 con dati anagrafici con R^2 basso
#mod3 cosidero le serie temporali, se un cliente mi ha fatto far soldi l'anno prima,
# allora me li rifarà rifare l'anno dopo
# i missing sono clienti persi (non ci interessano)
#mod4 tenendo profitto, ma eliminando età e reddito l'R^2 rimane alto,
# magari gli elementi eliminati li abbiamo usati male
#possiamo capire se il profitto in anni successivi è correlato
cor(Profit9,Profit0,use="complete.obs") #correlazione al 60%
#con boxplot su Profit0 vediamo che ci sono outlier
sum(Profit0>5000,na.rm=TRUE) #8 sopra
#guardo anche sotto 0
#cosa succede all'R^2 se rimuovo gli outlier? CONTROLLARE

#l'obiettivo è capire se ci sta un incentivo sul passare da offline a online
#incentivo di 100 euro è troppo perché in media guadagno 18
#magari devo controllare il rapporto tra Online e Age

#un cliente online è più fedele o meno fedele?
####PRT 2 . ANALYZE retention with OLS (minimi quadrati ordinari)

#c'è un buco sul profitto più grande
#non so il profitto ma so che è online

#R^2 basso ma che può mirare le mie azioni può essere utile

#quando prevedo una variabile 0/1 con un modello OLS non prevede esattamente ma un valore vicino
#vedo due gruppi ben separati

#potrei vederli come probabilità, ma ci sono valori maggiori di 1

#####PARTE 1

#--- PART 2 - ANALYZE retention with OLS
sum(is.na(Online0))
sum(is.na(Profit0))
which(is.na(Profit0) != is.na(Online0))
which(is.na(Profit0) < is.na(Online0))
which(is.na(Profit0) > is.na(Online0))
#--- ADD retain variable
Retain = ifelse(is.na(Profit0),0,1) #decido io cosa mettere a 1 e cosa a 0: 
# ora decido 1 caso buono (tengo cliente), ma spesso 1 è l'anomalia
mod1 = lm(Retain ~ Online9)
summary(mod1)
mod2 = lm(Retain ~ Online9+AgeZero+AgeGiven+IncomeZero+IncomeGiven+Tenure+District1100+District1200)
summary(mod2)
mod3 = lm(Retain ~ Profit9+Online9+AgeZero+AgeGiven+IncomeZero+IncomeGiven+Tenure+District1100+District1200)
summary(mod3)
#R^2 scricchiola un po' perché stiamo prevedendo una variabile 0/1
hist(mod3$fitted,nclass=50)
#due gruppi di clienti
#ho usato tutti i dati per fittare e niente per validare
#ci sono due mode alte, e una moda piccolina tra 0.7 e 0.8
#rischio di perderlo ma mi fa guadagnare o perdere soldi --> decisione business
#potrei interpretarlo come probabilità di tenerlo o no
#conviene usare un metodo di classificazione invece che uno di regressione
#qual è la soglia sotto cui preoccuparmi?

#--- PART 3 - ANALYZE retention with Logistic regression
glm.out = glm(Retain ~ Profit9+Online9+AgeZero+AgeGiven+IncomeZero+
                IncomeGiven+Tenure+District1100+District1200,family=binomial(logit))
summary(glm.out)
#le stime dei coefficienti sono piccoli, ma è normale perché poi entrano nella funzione
#cambiano diagnostiche, non c'è R^2
hist(glm.out$fitted.values,nclass=50)
#istogramma simile a quello di prima ma tutti concentrato tra 0 e 1. C'è di nuovo una gobbetta
# tra 0.7 e 0.8. devo scegliere dove metto la soglia,
#al variare della soglia varia l'auc, è un costo o un guadagno la diversa soglia
plot(mod3$fitted, glm.out$fitted, xlab="Predicted OLS", ylab="Predicted logit")
#c'è una specie di compressione, è non lineare

#--- PART 4 - Demographics vs. Past profit to analyze profitability 
mod1 = lm(Profit0 ~ 
            Profit9+Online9+Tenure+District1100+District1200+factor(Age)+factor(Income))
#uso age e income come categoriche
summary(mod1) #R^2 migliora
#molti missing eliminati
mod2 = lm(Profit0 ~ Profit9+Online9+Tenure)
summary(mod2)
mod3 = lm(Profit0 ~ District1100+District1200+factor(Age)+factor(Income))
summary(mod3)

###aggiungere relazione tra variabili

###posso eliminare outlier
dtab = subset(dtab, subset = X0Profit>-3000) #elimino perdita anomala
#fa aumentare R^2
