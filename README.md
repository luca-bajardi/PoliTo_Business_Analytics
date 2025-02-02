# PoliTo_Business_Analytics
Questo repository include tutti gli homework del corso <a href="https://didattica.polito.it/pls/portal30/gap.pkg_guide.viewGap?p_cod_ins=03PVFNG&p_a_acc=2020&p_header=S&p_lang=IT">Business Analytics</a> del Politecnico di Torino.

Per l'analisi dei dati abbiamo utilizzato il <a href="https://www.r-project.org/">linguaggio di programmazione R</a>.

Per la formattazione del testo e l'impaginazione degli homework abbiamo utilizzato <a href="https://rmarkdown.rstudio.com/">R Markdown</a> e i <a href="https://github.com/pandoc/lua-filters">Lua filters</a>.

### Analisi Colonial Broadcasting
Il dataset che utilizziamo in questa tesina è relativo ai film mandati in onda in America nel 1992 da tre delle principali reti televisive. In particolare nel business case fornito dall’Harvard Business School, facciamo riferimento ad una di queste: la “Colonial Broadcasting Company”.

L’obiettivo è quello di capire quali siano le variabili e come queste interagiscano, nell’influenzare gli ascolti per un determinato programma. Gli ascolti vengono espressi attraverso un valore di rating, nello specifico, ogni punto di rating rappresenta 921000 famiglie americane che stanno guardando il programma.

Formuliamo diversi modelli di regressione per capire quali siano le variabili migliori da utilizzare per prevedere il rating del film al quale siamo interessati. Il problema principale legato a questo dataset, è che questo è costituito da poche osservazioni, quindi per riuscire a formulare dei modelli più accurati, è necessario ricorrere alla cross-validation.

Inizialmente proviamo a formulare dei modelli con un’unica variabile, quella del rating del programma precedente, anche se ovviamente non ci permette di avere una spiegazione completa. Per questo vorremmo introdurre nei modelli, altre variabili. Per capire quante e quali variabili andare ad utilizzare, applichiamo il metodo esaustivo, il metodo backward e quello forward, in modo da stabilire quale sia il modello migliore. Infine proviamo a formulare dei modelli alternativi andando ad utilizzare le regressioni ridge e lasso, per penalizzare in maniera differente i vari coefficienti delle variabili indipendenti.

### Analisi Pilgrim Bank
Utilizziamo il dataset fornito dalla “Harvad Business School” relativo alla redditività di alcuni clienti nella Pilgrim Bank nel corso degli anni 1999 e 2000.

Inizialmente, cerchiamo di predire il profitto di un cliente in base alle sue caratteristiche anagrafiche, questa analisi potrebbe essere utile per la banca nel caso in cui debba decidere ad esempio se concedere un prestito oppure no. Vorrei quindi una previsione molto accurata in modo da capire se quel determinato cliente sarà in grado di restituire il denaro e scegliere quelle azioni finanziarie che siano il meno rischiose possibile. Nella previsione è importante tenere conto del fatto che potrebbero mancare alcune informazioni relative ai clienti, in questo caso è necessario scegliere il modo opportuno per cercare di non buttare via quei dati, ma utilizzarli comunque per la nostra previsione.

Un altro tipo di analisi che vogliamo effettuare è quella nel tempo. Infatti, per ciascun cliente che è presente nella base dati nel 1999, abbiamo un’informazione sull’anno successivo: sappiamo se il cliente decide di lasciare la banca oppure di rimanerci e in quest’ultimo caso conosciamo anche il suo profitto in quell’anno. Quindi l’obiettivo della nostra analisi è quello di riuscire a prevedere se un cliente rimarrà fedele alla banca o meno. Per farlo, utilizziamo diversi metodi di classificazione tra questi l’albero di decisione e il knn. Inoltre poiché la variabile target alla quale siamo interessati è binaria, possiamo utilizzare anche la regressione logistica e l’svm.

Infine ci concentriamo sull’eventuale passaggio di un cliente dal servizio offline a quello online. Se riuscissimo a capire che, per una determinata tipologia di clienti, il passaggio al canale online, permette di guadagnare maggiormente, la banca potrebbe ad esempio portare avanti delle campagne pubblicitarie per fare in modo che i clienti scelgano l’opzione più profittevole. Per ottenere questo tipo di informazione, applichiamo un algoritmo di clustering (il k-means) considerando solamente quei clienti che nel 1999 utilizzavano il servizio di banca offline e che nel 2000 sono rimasti clienti della banca. All’interno di ogni cluster poi, calcoliamo la media, separatamente per coloro che hanno scelto di passare al digitale e per quelli che sono rimasti con il servizio standard. In questo modo riusciamo a definire delle tipologie di clienti e capire se per la banca risulta più o meno vantaggioso far passare il cliente alla banca online.
