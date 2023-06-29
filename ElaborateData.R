
# ELABORAZIONE DEI DATI PER POI STUDIARLI

# Come prima setto la working directory
setwd("C:/Users/enric/Desktop/Progetto Modelli")
# Ora scarico il workspace in output da DownloadData.R
source("DownloadData.R")
# Libreria ggplot che quasi sicuro servirà e altre
library(ggplot2)
library(zoo)
library(forecast)
library(stats)

# Importo i database che mi mancano
X2000 <- read_excel("C:/Users/enric/Desktop/Progetto Modelli/AirData/2000.xls",  skip = 1)
X2010 <- read_excel("C:/Users/enric/Desktop/Progetto Modelli/AirData/2010.xls",  skip = 1)
X2019 <- read_excel("C:/Users/enric/Desktop/Progetto Modelli/AirData/2019.xls",  skip = 1)
X2020 <- read_excel("C:/Users/enric/Desktop/Progetto Modelli/AirData/2020.xls",  skip = 1)

# Stampo qualche grafico per farmi un'idea di come vanno le cose
# Utilizzo intanto i 5 aggregati italiani
# Costruisco la serie storica per ciascuno di loro
Tur_NE <- ts(as.numeric(Turismo[23,2:22]), freq = 1, start = 2000)
Tur_NO <- ts(as.numeric(Turismo[22,2:22]), freq = 1, start = 2000)
Tur_CE <- ts(as.numeric(Turismo[24,2:22]), freq = 1, start = 2000)
Tur_SU <- ts(as.numeric(Turismo[25,2:22]), freq = 1, start = 2000)
Tur_IS <- ts(as.numeric(Turismo[26,2:22]), freq = 1, start = 2000)

# Il grafico lo stampo dopo con ggplot
# Ora stampo tutte le serie storiche in un grafico
# In teoria questo comando mi aiuta a fare la legenda fuori dal grafico
#par(mar=c(5, 4, 4, 8), xpd=TRUE)
#plot(2000:2020, Tur_NE, type = "l", col = 2, lwd = 3, ylim = c(0,15), xlab = "Anno", 
#     ylab = "Tasso in %", main = "Tasso di turisticità")
#grid(lwd = 2, lty = 2)
# da capire come stoppare la griglia dentro gli assi
#points(2000:2020, Tur_NO, type = "l", col = 3, lwd = 3)
#points(2000:2020, Tur_CE, type = "l", col = 4, lwd = 3)
#points(2000:2020, Tur_SU, type = "l", col = 5, lwd = 3)
#points(2000:2020, Tur_IS, type = "l", col = 6, lwd = 3)
# la legenda non funziona 
# (alla fine funziona ma senza i nomi completi)
#legend("topright", inset=c(-0.2, -0.1), legend=c("NE","NO", 
#                                              "CE", "SU", "IS"),
#       col = 2:6, lty = 1, lwd = 3)
# Metto i vettori in un database e provo il grafico con ggplot
Tur_ts <- data.frame(anno = 2000:2020, NordEst = Tur_NE, NordOvest = Tur_NO,
                     Centro = Tur_CE, Sud = Tur_SU, Isole = Tur_IS)
Tur_ts_plot <- ggplot(Tur_ts, aes(x = anno, y= NordEst, color = "NE")) +
  geom_line() +
  geom_line(aes(y = NordOvest, color = "NO")) +
  geom_line(aes(y = Centro, color = "CE")) +
  geom_line(aes(y = Sud, color = "SU")) +
  geom_line(aes(y = Isole, color = "IS")) +
  xlab("Mesi") +
  ylab("Valori") +
  ggtitle("Tasso turistico")

# Le serie sembrano avere andamento abbastanza costante
# Fino al 2019 ovviamente
# Provo a prendere il dato "istantaneo" del 2019
# Estraggo un piccolo dataframe con 2 colonne: regione e dato del 2019
Tur_19 <- Turismo[1:20, c(1,21)]
# Provo ad ordinarlo in ordine alfabetico per regione
Tur_19 <- Tur_19[order(Tur_19$Regione),]

# Provo a stampare i dati per vedere cosa esce
Tur_19_plot <- ggplot(Tur_19, aes(x=Regione, y=`2019`)) + geom_boxplot() + 
  theme(axis.text.x = element_blank())
# Questi sono i tassi turistici delle varie regioni

# Provo a vedere un'altra variabile come si comporta
# per esempio vediamo mov e pas
# una indica i movimenti complessivi mensili in un aeroporto
# l'altra i passeggeri che ivi transitano
# come prima cosa sostituisco gli na con 0
mov[] <- lapply(mov, function(x) na.fill(x, 0))
pas[] <- lapply(pas, function(x) na.fill(x, 0))
# innanzitutto i dati vanno aggregati per regione
# creo un nuovo dataframe in cui salvo i dati per regione
# Prima correggo un dato anomalo
mov$Palermo[236] <- 0
# Mi sa devo calcolare le colonne una alla volta...
Abruzzo <- as.numeric(mov$Pescara)
Basilicata <- rep(0, 276)
Calabria <- as.numeric(mov$Lamezia.T.) + as.numeric(mov$Reggio.Cal.) + 
  as.numeric(mov$Crotone)
Campania <- as.numeric(mov$Napoli)
EmiliaRomagna <- as.numeric(mov$Bologna) + as.numeric(mov$Forli.) +
  as.numeric(mov$Parma) + as.numeric(mov$Rimini)
FriuliVeneziaGiulia <- as.numeric(mov$Ronchi.dei.L.)
Lazio <- as.numeric(mov$Roma.CIA) + as.numeric(mov$Roma.FCO)
Liguria <- as.numeric(mov$Genova)
Lombardia <- as.numeric(mov$Bergamo) + as.numeric(mov$Brescia) + 
  as.numeric(mov$Milano.LIN) + as.numeric(mov$Milano.MXP)
Marche <- as.numeric(mov$Ancona)
Molise <- rep(0,276)
Piemonte <- as.numeric(mov$Cuneo) + as.numeric(mov$Torino)
Puglia <- as.numeric(mov$Bari) + as.numeric(mov$Brindisi) +
  as.numeric(mov$Foggia)
Sardegna <- as.numeric(mov$Alghero) + as.numeric(mov$Olbia) + 
  as.numeric(mov$Cagliari)
Sicilia <- as.numeric(mov$Catania) + as.numeric(mov$Palermo) + 
  as.numeric(mov$Trapani)
Toscana <- as.numeric(mov$Firenze) + as.numeric(mov$Grosseto) +
  as.numeric(mov$Siena)
TrentinoAltoAdige <- as.numeric(mov$Bolzano)
Umbria <- as.numeric(mov$Perugia)
ValledAosta <- rep(0, 276)
Veneto <- as.numeric(mov$Venezia) + as.numeric(mov$Verona) +
  as.numeric(mov$Treviso)
mov_reg <- data.frame(Abruzzo, Basilicata, Calabria, Campania,
                              EmiliaRomagna, FriuliVeneziaGiulia, Lazio,
                              Liguria, Lombardia, Marche, Molise, Piemonte,
                              Puglia, Sardegna, Sicilia, Toscana, 
                              TrentinoAltoAdige, Umbria, ValledAosta, Veneto)
# faccio la stessa identica cosa per i passeggeri
Abruzzo <- as.numeric(pas$Pescara)
Basilicata <- rep(0, 276)
Calabria <- as.numeric(pas$Lamezia.T.) + as.numeric(pas$Reggio.Cal.) + 
  as.numeric(pas$Crotone)
Campania <- as.numeric(pas$Napoli)
EmiliaRomagna <- as.numeric(pas$Bologna) + as.numeric(pas$Forli.) +
  as.numeric(pas$Parma) + as.numeric(pas$Rimini)
FriuliVeneziaGiulia <- as.numeric(pas$Ronchi.dei.L.)
Lazio <- as.numeric(pas$Roma.CIA) + as.numeric(pas$Roma.FCO)
Liguria <- as.numeric(pas$Genova)
Lombardia <- as.numeric(pas$Bergamo) + as.numeric(pas$Brescia) + 
  as.numeric(pas$Milano.LIN) + as.numeric(pas$Milano.MXP)
Marche <- as.numeric(pas$Ancona)
Molise <- rep(0,276)
Piemonte <- as.numeric(pas$Cuneo) + as.numeric(pas$Torino)
Puglia <- as.numeric(pas$Bari) + as.numeric(pas$Brindisi) +
  as.numeric(pas$Foggia)
Sardegna <- as.numeric(pas$Alghero) + as.numeric(pas$Olbia) + 
  as.numeric(pas$Cagliari)
Sicilia <- as.numeric(pas$Catania) + as.numeric(pas$Palermo) + 
  as.numeric(pas$Trapani)
Toscana <- as.numeric(pas$Firenze) + as.numeric(pas$Grosseto) +
  as.numeric(pas$Siena)
TrentinoAltoAdige <- as.numeric(pas$Bolzano)
Umbria <- as.numeric(pas$Perugia)
ValledAosta <- rep(0, 276)
Veneto <- as.numeric(pas$Venezia) + as.numeric(pas$Verona) +
  as.numeric(pas$Treviso)
pas_reg <- data.frame(Abruzzo, Basilicata, Calabria, Campania,
                      EmiliaRomagna, FriuliVeneziaGiulia, Lazio,
                      Liguria, Lombardia, Marche, Molise, Piemonte,
                      Puglia, Sardegna, Sicilia, Toscana, 
                      TrentinoAltoAdige, Umbria, ValledAosta, Veneto)
# creo i dataframe aggregati
NordEst <- mov_reg$FriuliVeneziaGiulia + mov_reg$Veneto +
  mov_reg$EmiliaRomagna + mov_reg$TrentinoAltoAdige
NordOvest <- mov_reg$ValledAosta + mov_reg$Piemonte + mov_reg$Lombardia +
  mov_reg$Liguria
Centro <- mov_reg$Toscana + mov_reg$Marche + mov_reg$Lazio + 
  mov_reg$Umbria
Sud <- mov_reg$Abruzzo + mov_reg$Molise + mov_reg$Campania + 
  mov_reg$Basilicata + mov_reg$Puglia + mov_reg$Calabria
Isole <- mov_reg$Sicilia + mov_reg$Sardegna
# mettiamoli in un dataframe
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2022-12-01")
date_vector <- format(seq(from = start_date, to = end_date, by = "month"), "%d-%m-%Y")
mov_ag <- data.frame(date_vector, NordEst, NordOvest, Centro, Sud, Isole)
# Ora i passeggeri
NordEst <- pas_reg$FriuliVeneziaGiulia + pas_reg$Veneto +
  pas_reg$EmiliaRomagna + pas_reg$TrentinoAltoAdige
NordOvest <- pas_reg$ValledAosta + pas_reg$Piemonte + pas_reg$Lombardia +
  pas_reg$Liguria
Centro <- pas_reg$Toscana + pas_reg$Marche + pas_reg$Lazio + 
  pas_reg$Umbria
Sud <- pas_reg$Abruzzo + pas_reg$Molise + pas_reg$Campania + 
  pas_reg$Basilicata + pas_reg$Puglia + pas_reg$Calabria
Isole <- pas_reg$Sicilia + pas_reg$Sardegna
# mettiamoli in un dataframe
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2022-12-01")
date_vector <- format(seq(from = start_date, to = end_date, by = "month"), "%d-%m-%Y")
pas_ag <- data.frame(date_vector, NordEst, NordOvest, Centro, Sud, Isole)

# per poter plottare mi servono i due dati mancanti
# uno lo ricavo come media dei dati prima e dei dati dopo
mov_ag$NordEst[18] <- mov_ag$NordEst[17]*0.5+mov_ag$NordEst[19]*0.5
mov_ag$NordOvest[18] <- mov_ag$NordOvest[17]*0.5+mov_ag$NordOvest[19]*0.5
mov_ag$Centro[18] <- mov_ag$Centro[17]*0.5+mov_ag$Centro[19]*0.5
mov_ag$Sud[18] <- mov_ag$Sud[17]*0.5+mov_ag$Sud[19]*0.5
mov_ag$Isole[18] <- mov_ag$Isole[17]*0.5+mov_ag$Isole[19]*0.5
# l'ultimo dato invece lo ricavo dal lisciamento esponenziale
# non funzionano nè il lisciamento esponenziale nè l'auto.arima
#fit <- auto.arima(ts(mov_ag$NordEst, start = 2000, frequency = 12))
#mov_ag$NordEst[276] <- forecast(fit, 1)$mean[1]
# non funziona damn
# vediamo se funziona con le altre serie
#fit <- HoltWinters(ts(mov_ag$NordOvest, start = 2000, frequency = 12))
#mov_ag$NordOvest[276] <- predict(fit, n.ahead = 1)
#fit <- HoltWinters(ts(mov_ag$Centro, start = 2000, frequency = 12))
#mov_ag$Centro[276] <- predict(fit, n.ahead = 1)
#fit <- HoltWinters(ts(mov_ag$Sud, start = 2000, frequency = 12))
#mov_ag$Sud[276] <- predict(fit, n.ahead = 1)
#fit <- HoltWinters(ts(mov_ag$Isole, start = 2000, frequency = 12))
#mov_ag$Isole[276] <- predict(fit, n.ahead = 1)
# nemmeno tutti gli altri
# per ora levo l'ultima riga, poi me ne occupo
mov_ag <- mov_ag[-276,]

# dato che ggplot non funziona provo a rendere il vettore del tempo un vettore numerico
mov_ag$date_vector <- 1:275
# stampiamo le serie storiche e vediamo che esce
mov_ag_plot <- ggplot(mov_ag, aes(x = date_vector, y = NordEst, color = "NE")) +
  geom_line() +
  geom_line(aes(y = NordOvest, color = "NO")) +
  geom_line(aes(y = Centro, color = "CE")) +
  geom_line(aes(y = Sud, color = "SU")) +
  geom_line(aes(y = Isole, color = "IS")) +
  xlab("Mesi") +
  ylab("Valori") +
  ggtitle("Movimenti aggregati")

# okay ora facciamo la stessa roba con i passeggeri
# creo i dataframe aggregati
NordEst <- pas_reg$FriuliVeneziaGiulia + pas_reg$Veneto +
  pas_reg$EmiliaRomagna + pas_reg$TrentinoAltoAdige
NordOvest <- pas_reg$ValledAosta + pas_reg$Piemonte + pas_reg$Lombardia +
  pas_reg$Liguria
Centro <- pas_reg$Toscana + pas_reg$Marche + pas_reg$Lazio + 
  pas_reg$Umbria
Sud <- pas_reg$Abruzzo + pas_reg$Molise + pas_reg$Campania + 
  pas_reg$Basilicata + pas_reg$Puglia + pas_reg$Calabria
Isole <- pas_reg$Sicilia + pas_reg$Sardegna
# mettiamoli in un dataframe
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2022-12-01")
date_vector <- format(seq(from = start_date, to = end_date, by = "month"), "%d-%m-%Y")
pas_ag <- data.frame(date_vector, NordEst, NordOvest, Centro, Sud, Isole)
# Ora i passeggeri
NordEst <- pas_reg$FriuliVeneziaGiulia + pas_reg$Veneto +
  pas_reg$EmiliaRomagna + pas_reg$TrentinoAltoAdige
NordOvest <- pas_reg$ValledAosta + pas_reg$Piemonte + pas_reg$Lombardia +
  pas_reg$Liguria
Centro <- pas_reg$Toscana + pas_reg$Marche + pas_reg$Lazio + 
  pas_reg$Umbria
Sud <- pas_reg$Abruzzo + pas_reg$Molise + pas_reg$Campania + 
  pas_reg$Basilicata + pas_reg$Puglia + pas_reg$Calabria
Isole <- pas_reg$Sicilia + pas_reg$Sardegna
# mettiamoli in un dataframe
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2022-12-01")
date_vector <- format(seq(from = start_date, to = end_date, by = "month"), "%d-%m-%Y")
pas_ag <- data.frame(date_vector, NordEst, NordOvest, Centro, Sud, Isole)

# per poter plottare mi servono i due dati mancanti
# uno lo ricavo come media dei dati prima e dei dati dopo
pas_ag$NordEst[18] <- pas_ag$NordEst[17]*0.5+pas_ag$NordEst[19]*0.5
pas_ag$NordOvest[18] <- pas_ag$NordOvest[17]*0.5+pas_ag$NordOvest[19]*0.5
pas_ag$Centro[18] <- pas_ag$Centro[17]*0.5+pas_ag$Centro[19]*0.5
pas_ag$Sud[18] <- pas_ag$Sud[17]*0.5+pas_ag$Sud[19]*0.5
pas_ag$Isole[18] <- pas_ag$Isole[17]*0.5+pas_ag$Isole[19]*0.5
# per ora levo l'ultima riga, poi me ne occupo
pas_ag <- pas_ag[-276,]

# dato che ggplot non funziona provo a rendere il vettore del tempo un vettore numerico
pas_ag$date_vector <- 1:275
# stampiamo le serie storiche e vediamo che esce
pas_ag_plot <- ggplot(pas_ag, aes(x = date_vector, y = NordEst, color = "NE")) +
  geom_line() +
  geom_line(aes(y = NordOvest, color = "NO")) +
  geom_line(aes(y = Centro, color = "CE")) +
  geom_line(aes(y = Sud, color = "SU")) +
  geom_line(aes(y = Isole, color = "IS")) +
  xlab("Mesi") +
  ylab("Valori") +
  ggtitle("Passeggeri aggregati")

# voglio creare, sia di mov, sia di pas, un nuovo dataframe aggregato 
# stavolta però i dati devono essere annuali
# come prima cosa, a me interessano solo i dati fino al 2020
# quindi levo tutti quelli dopo
mov_ag <- mov_ag[1:252,]
pas_ag <- pas_ag[1:252,]
# ora provo ad aggregare i dati
# in realtà è sufficiente sommarli, essendo valori assoluti
# creo i nuovi dataframe
# prima i nuovi vettori (uso sempre gli stessi tanto si sovrascrivono)
NordEst <- NordOvest <- Centro <- Sud <- Isole <- rep(0,21)
x <- 1:12
for (i in 0:20){
  ind <- (i*12)+x
  NordEst[i+1] <- sum(mov_ag$NordEst[ind])
  NordOvest[i+1] <- sum(mov_ag$NordOvest[ind])
  Centro[i+1] <- sum(mov_ag$Centro[ind])
  Sud[i+1] <- sum(mov_ag$Sud[ind])
  Isole[i+1] <- sum(mov_ag$Isole[ind])
}
# aggiungo al database il vettore anno
y <- 2000:2020
mov_an <- data.frame(y,NordEst,NordOvest,Centro,Sud,Isole)
for (i in 0:20){
  ind <- (i*12)+x
  NordEst[i+1] <- sum(pas_ag$NordEst[ind])
  NordOvest[i+1] <- sum(pas_ag$NordOvest[ind])
  Centro[i+1] <- sum(pas_ag$Centro[ind])
  Sud[i+1] <- sum(pas_ag$Sud[ind])
  Isole[i+1] <- sum(pas_ag$Isole[ind])
}
pas_an <- data.frame(y,NordEst,NordOvest,Centro,Sud,Isole)

# stampiamo le serie storiche e vediamo che esce
mov_an_plot <- ggplot(mov_an, aes(x = y, y = NordEst, color = "NE")) +
  geom_line() +
  geom_line(aes(y = NordOvest, color = "NO")) +
  geom_line(aes(y = Centro, color = "CE")) +
  geom_line(aes(y = Sud, color = "SU")) +
  geom_line(aes(y = Isole, color = "IS")) +
  xlab("Anni") +
  ylab("Valori") +
  ggtitle("Movimenti aggregati annuali")

# stampiamo le serie storiche e vediamo che esce anche per i passeggeri
pas_an_plot <- ggplot(pas_an, aes(x = y, y = NordEst, color = "NE")) +
  geom_line() +
  geom_line(aes(y = NordOvest, color = "NO")) +
  geom_line(aes(y = Centro, color = "CE")) +
  geom_line(aes(y = Sud, color = "SU")) +
  geom_line(aes(y = Isole, color = "IS")) +
  xlab("Anni") +
  ylab("Valori") +
  ggtitle("Passeggeri aggregati annuali")

# Salvo le due variabili nel formato che mi interessa
mov_ts <- mov_an
pas_ts <- pas_an
names(mov_ts)[1] <- names(pas_ts)[1] <- "anno"

# Ora è arrivato il momento di occuparsi di un'altra variabile
# Prendiamo in considerazione la variabile PIL
# Partiamo dagli aggregati in serie storica
# Sono già a posto devo solo fare la trasposta
Pil_ts <- data.frame(t(Pil_agg))
# Devo solo rimuovere la prima riga
Pil_ts <- Pil_ts[-1,]
names(Pil_ts) <- c("NordOvest", "NordEst", "Centro", "Sud", "Isole")
# levo anche l'ultima riga che è del 2021
Pil_ts <- Pil_ts[-22,]
# Non si sa perché è un dataframe di char, li metto double
Pil_ts$NordEst <- as.numeric(Pil_ts$NordEst)
Pil_ts$NordOvest <- as.numeric(Pil_ts$NordOvest)
Pil_ts$Centro <- as.numeric(Pil_ts$Centro)
Pil_ts$Sud <- as.numeric(Pil_ts$Sud)
Pil_ts$Isole <- as.numeric(Pil_ts$Isole)
# Ultima cosa aggiungo l'anno
anno <- 2000:2020
Pil_ts <- cbind(anno, Pil_ts)
# Facciamo una stampa dei dati per scrupolo
Pil_ts_plot <- ggplot(Pil_ts, aes(x = anno, y = NordEst, color = "NE")) +
  geom_line() +
  geom_line(aes(y = NordOvest, color = "NO")) +
  geom_line(aes(y = Centro, color = "CE")) +
  geom_line(aes(y = Sud, color = "SU")) +
  geom_line(aes(y = Isole, color = "IS")) +
  xlab("Anni") +
  ylab("Valori") +
  ggtitle("PIL")

# Controlliamo ora il database su Produttività del lavoro,
# valore aggiunto del turismo e unità di lavoro nel settore del turismo
ValAgg_ts <- data.frame(t(ValAgg_agg))
nomi <- ValAgg_ts[1,]
ValAgg_ts <- ValAgg_ts[-1,]
names(ValAgg_ts) <- nomi
ValAgg_ts <- ValAgg_ts[1:16,]
Ula_ts <- data.frame(t(Ula_agg))
Ula_ts <- Ula_ts[-1,]
names(Ula_ts) <- nomi
Ula_ts <- Ula_ts[1:16,]
Produttivita_ts <- data.frame(t(Produttivita_agg))
Produttivita_ts <- Produttivita_ts[-1,]
names(Produttivita_ts) <- nomi
Produttivita_ts <- Produttivita_ts[1:16,]
ValAgg_ts[] <- lapply(ValAgg_ts, function(x) if(is.character(x)) as.numeric(as.character(x)) else x)
Ula_ts[] <- lapply(Ula_ts, function(x) if(is.character(x)) as.numeric(as.character(x)) else x)
Produttivita_ts[] <- lapply(Produttivita_ts, function(x) if(is.character(x)) as.numeric(as.character(x)) else x)

# Passiamo alla prossima variabile da convertire in serie storica
# Mancano i dati sulle spese dei turisti
# Partiamo dalle spese che vengono effettuate nelle varie regioni
# Come prima cosa pulizia dai trimestri, essendoci già i dati annuali
SpesaRegione <- SpesaRegione[SpesaRegione$...2 == "1° trim.",]
SpesaRegione <- SpesaRegione[,-2]
SpesaRegione <- SpesaRegione[-22,]
# Questi dati vanno aggregati in modo diverso
SpesaMotivo <- SpesaMotivo[-85:-88,]
SpesaMotivo <- SpesaMotivo[,3:6]
SpesaMotivo$Totale <- as.numeric(SpesaMotivo$Totale)
# divide il dataframe in 21 gruppi di 4 osservazioni
split_df <- split(SpesaMotivo, rep(1:21, each = 4)) 
# somma le osservazioni all'interno di ogni gruppo
sum_df <- lapply(split_df, colSums) 
# ricombina i risultati in un nuovo dataframe
SpesaMotivo_ts <- data.frame(do.call(rbind, sum_df)) 
# Assegno i nomi alle colonne
names(SpesaMotivo_ts) <- names(SpesaMotivo)
# Riaggiungo la colonna dell'anno
SpesaMotivo_ts <- cbind(anno, SpesaMotivo_ts)
  
# Ora solito discorso devo creare gli aggregati
NordEst <- SpesaRegione$`FRIULI VENEZIA GIULIA` + SpesaRegione$VENETO +
  SpesaRegione$`EMILIA ROMAGNA` + SpesaRegione$`TRENTINO ALTO ADIGE`
NordOvest <- SpesaRegione$`VALLE D'AOSTA` + SpesaRegione$PIEMONTE + SpesaRegione$LOMBARDIA +
  SpesaRegione$LIGURIA
Centro <- SpesaRegione$TOSCANA + SpesaRegione$MARCHE + SpesaRegione$LAZIO + 
  SpesaRegione$UMBRIA
Sud <- SpesaRegione$ABRUZZO + SpesaRegione$MOLISE + SpesaRegione$CAMPANIA + 
  SpesaRegione$BASILICATA + SpesaRegione$PUGLIA + SpesaRegione$CALABRIA
Isole <- SpesaRegione$SICILIA + SpesaRegione$SARDEGNA
# Salvo nel dataframe prima di sovrascrivere le variabili
SpesaRegione_ts <- data.frame(anno, NordEst, NordOvest, Centro, Sud, Isole)
# I dati di spesa motivo dovrebbero andare bene così

# Faccio i soliti plot per vedere un po' i dati
SpesaRegione_ts_plot <- ggplot(SpesaRegione_ts, aes(x = anno, y = NordEst, color = "NE")) +
  geom_line() +
  geom_line(aes(y = NordOvest, color = "NO")) +
  geom_line(aes(y = Centro, color = "CE")) +
  geom_line(aes(y = Sud, color = "SU")) +
  geom_line(aes(y = Isole, color = "IS")) +
  xlab("Anni") +
  ylab("Valori") +
  ggtitle("Spesa nelle Regioni")
# Mamma mia che dati strani
# Vediamo le spese per motivo
SpesaMotivo_ts_plot <- ggplot(SpesaMotivo_ts, aes(x = anno, y = Lavoro, color = "Lavoro")) +
  geom_line() +
  geom_line(aes(y = Personali, color = "Personali")) +
  geom_line(aes(y = Vacanza, color = "Vacanza")) +
  geom_line(aes(y = Totale, color = "Totale")) +
  xlab("Anni") +
  ylab("Valori") +
  ggtitle("Spesa in base al motivo del viaggio")

# Ora pensiamo alla popolazione
# Quello che voglio intanto è una serie storica aggregata
# Intanto creo gli aggregati
NordEst <- Popolazione$FriuliVeneziaGiulia + Popolazione$Veneto +
  Popolazione$EmiliaRomagna + Popolazione$TrentinoAltoAdige
NordOvest <- Popolazione$ValledAosta + Popolazione$Piemonte + Popolazione$Lombardia +
  Popolazione$Liguria
Centro <- Popolazione$Toscana + Popolazione$Marche + Popolazione$Lazio + 
  Popolazione$Umbria
Sud <- Popolazione$Abruzzo + Popolazione$Molise + Popolazione$Campania + 
  Popolazione$Basilicata + Popolazione$Puglia + Popolazione$Calabria
Isole <- Popolazione$Sicilia + Popolazione$Sardegna
# Ora creo il dataframe
Popolazione_ts <- data.frame(anno, NordEst, NordOvest, Centro, Sud, Isole)
# Solita stampa per stare sereni
Popolazione_ts_plot <- ggplot(Popolazione_ts, aes(x = anno, y = NordEst, color = "NE")) +
  geom_line() +
  geom_line(aes(y = NordOvest, color = "NO")) +
  geom_line(aes(y = Centro, color = "CE")) +
  geom_line(aes(y = Sud, color = "SU")) +
  geom_line(aes(y = Isole, color = "IS")) +
  xlab("Anni") +
  ylab("Valori") +
  ggtitle("Popolazione")

# Sistemo i dati su movimenti e passeggeri del 2000

Abruzzo <- as.numeric(X2000[X2000$Aeroporto == "Pescara",]$Movimenti)
Basilicata <- 0
Calabria <- as.numeric(X2000[X2000$Aeroporto == "Lamezia T.",]$Movimenti) + as.numeric(X2000[X2000$Aeroporto == "Reggio Cal.",]$Movimenti) +   as.numeric(X2000[X2000$Aeroporto == "Crotone",]$Movimenti)
Campania <- as.numeric(X2000[X2000$Aeroporto == "Napoli",]$Movimenti)
EmiliaRomagna <- as.numeric(X2000[X2000$Aeroporto == "Bologna",]$Movimenti) + as.numeric(X2000[X2000$Aeroporto == "Forli'",]$Movimenti) +
  as.numeric(X2000[X2000$Aeroporto == "Parma",]$Movimenti) + as.numeric(X2000[X2000$Aeroporto == "Rimini",]$Movimenti)
FriuliVeneziaGiulia <- as.numeric(X2000[X2000$Aeroporto == "Ronchi dei L.",]$Movimenti)
Lazio <- as.numeric(X2000[X2000$Aeroporto == "Roma CIA",]$Movimenti) + as.numeric(X2000[X2000$Aeroporto == "Roma FCO",]$Movimenti)
Liguria <- as.numeric(X2000[X2000$Aeroporto == "Genova",]$Movimenti)
Lombardia <- as.numeric(X2000[X2000$Aeroporto == "Bergamo",]$Movimenti) + as.numeric(X2000[X2000$Aeroporto == "Brescia",]$Movimenti) + 
  as.numeric(X2000[X2000$Aeroporto == "Milano LIN",]$Movimenti) + as.numeric(X2000[X2000$Aeroporto == "Milano MXP",]$Movimenti)
Marche <- as.numeric(X2000[X2000$Aeroporto == "Ancona",]$Movimenti)
Molise <- 0
Piemonte <- as.numeric(X2000[X2000$Aeroporto == "Cuneo",]$Movimenti) + as.numeric(X2000[X2000$Aeroporto == "Torino",]$Movimenti)
Puglia <- as.numeric(X2000[X2000$Aeroporto == "Bari",]$Movimenti) + as.numeric(X2000[X2000$Aeroporto == "Brindisi",]$Movimenti) +
  as.numeric(X2000[X2000$Aeroporto == "Foggia",]$Movimenti)
Sardegna <- as.numeric(X2000[X2000$Aeroporto == "Alghero",]$Movimenti) + as.numeric(X2000[X2000$Aeroporto == "Olbia",]$Movimenti) + 
  as.numeric(X2000[X2000$Aeroporto == "Cagliari",]$Movimenti)
Sicilia <- as.numeric(X2000[X2000$Aeroporto == "Catania",]$Movimenti) + as.numeric(X2000[X2000$Aeroporto == "Palermo",]$Movimenti) 
Toscana <- as.numeric(X2000[X2000$Aeroporto == "Firenze",]$Movimenti) 
TrentinoAltoAdige <- as.numeric(X2000[X2000$Aeroporto == "Bolzano",]$Movimenti)
Umbria <- as.numeric(X2000[X2000$Aeroporto == "Perugia",]$Movimenti)
ValledAosta <- 0
Veneto <- as.numeric(X2000[X2000$Aeroporto == "Venezia",]$Movimenti) + as.numeric(X2000[X2000$Aeroporto == "Verona",]$Movimenti) +
  as.numeric(X2000[X2000$Aeroporto == "Treviso",]$Movimenti)
mov2000 <- data.frame(Abruzzo, Basilicata, Calabria, Campania,
                      EmiliaRomagna, FriuliVeneziaGiulia, Lazio,
                      Liguria, Lombardia, Marche, Molise, Piemonte,
                      Puglia, Sardegna, Sicilia, Toscana, 
                      TrentinoAltoAdige, Umbria, ValledAosta, Veneto)
Abruzzo <- as.numeric(X2000[X2000$Aeroporto == "Pescara",]$Passeggeri)
Basilicata <- 0
Calabria <- as.numeric(X2000[X2000$Aeroporto == "Lamezia T.",]$Passeggeri) + as.numeric(X2000[X2000$Aeroporto == "Reggio Cal.",]$Passeggeri) +   as.numeric(X2000[X2000$Aeroporto == "Crotone",]$Passeggeri)
Campania <- as.numeric(X2000[X2000$Aeroporto == "Napoli",]$Passeggeri)
EmiliaRomagna <- as.numeric(X2000[X2000$Aeroporto == "Bologna",]$Passeggeri) + as.numeric(X2000[X2000$Aeroporto == "Forli'",]$Passeggeri) +
  as.numeric(X2000[X2000$Aeroporto == "Parma",]$Passeggeri) + as.numeric(X2000[X2000$Aeroporto == "Rimini",]$Passeggeri)
FriuliVeneziaGiulia <- as.numeric(X2000[X2000$Aeroporto == "Ronchi dei L.",]$Passeggeri)
Lazio <- as.numeric(X2000[X2000$Aeroporto == "Roma CIA",]$Passeggeri) + as.numeric(X2000[X2000$Aeroporto == "Roma FCO",]$Passeggeri)
Liguria <- as.numeric(X2000[X2000$Aeroporto == "Genova",]$Passeggeri)
Lombardia <- as.numeric(X2000[X2000$Aeroporto == "Bergamo",]$Passeggeri) + as.numeric(X2000[X2000$Aeroporto == "Brescia",]$Passeggeri) + 
  as.numeric(X2000[X2000$Aeroporto == "Milano LIN",]$Passeggeri) + as.numeric(X2000[X2000$Aeroporto == "Milano MXP",]$Passeggeri)
Marche <- as.numeric(X2000[X2000$Aeroporto == "Ancona",]$Passeggeri)
Molise <- 0
Piemonte <- as.numeric(X2000[X2000$Aeroporto == "Cuneo",]$Passeggeri) + as.numeric(X2000[X2000$Aeroporto == "Torino",]$Passeggeri)
Puglia <- as.numeric(X2000[X2000$Aeroporto == "Bari",]$Passeggeri) + as.numeric(X2000[X2000$Aeroporto == "Brindisi",]$Passeggeri) +
  as.numeric(X2000[X2000$Aeroporto == "Foggia",]$Passeggeri)
Sardegna <- as.numeric(X2000[X2000$Aeroporto == "Alghero",]$Passeggeri) + as.numeric(X2000[X2000$Aeroporto == "Olbia",]$Passeggeri) + 
  as.numeric(X2000[X2000$Aeroporto == "Cagliari",]$Passeggeri)
Sicilia <- as.numeric(X2000[X2000$Aeroporto == "Catania",]$Passeggeri) + as.numeric(X2000[X2000$Aeroporto == "Palermo",]$Passeggeri) 
Toscana <- as.numeric(X2000[X2000$Aeroporto == "Firenze",]$Passeggeri) 
TrentinoAltoAdige <- as.numeric(X2000[X2000$Aeroporto == "Bolzano",]$Passeggeri)
Umbria <- as.numeric(X2000[X2000$Aeroporto == "Perugia",]$Passeggeri)
ValledAosta <- 0
Veneto <- as.numeric(X2000[X2000$Aeroporto == "Venezia",]$Passeggeri) + as.numeric(X2000[X2000$Aeroporto == "Verona",]$Passeggeri) +
  as.numeric(X2000[X2000$Aeroporto == "Treviso",]$Passeggeri)
pas2000 <- data.frame(Abruzzo, Basilicata, Calabria, Campania,
                      EmiliaRomagna, FriuliVeneziaGiulia, Lazio,
                      Liguria, Lombardia, Marche, Molise, Piemonte,
                      Puglia, Sardegna, Sicilia, Toscana, 
                      TrentinoAltoAdige, Umbria, ValledAosta, Veneto)

# Idem per 2010, 2019, 2020
Abruzzo <- as.numeric(X2010[X2010$Aeroporto == "Pescara",]$Movimenti)
Basilicata <- 0
Calabria <- as.numeric(X2010[X2010$Aeroporto == "Lamezia T.",]$Movimenti) + as.numeric(X2010[X2010$Aeroporto == "Reggio Cal.",]$Movimenti) +   as.numeric(X2010[X2010$Aeroporto == "Crotone",]$Movimenti)
Campania <- as.numeric(X2010[X2010$Aeroporto == "Napoli",]$Movimenti)
EmiliaRomagna <- as.numeric(X2010[X2010$Aeroporto == "Bologna",]$Movimenti) + as.numeric(X2010[X2010$Aeroporto == "Forli'",]$Movimenti) +
  as.numeric(X2010[X2010$Aeroporto == "Parma",]$Movimenti) + as.numeric(X2010[X2010$Aeroporto == "Rimini",]$Movimenti)
FriuliVeneziaGiulia <- as.numeric(X2010[X2010$Aeroporto == "Trieste - Ronchi dei L.",]$Movimenti)
Lazio <- as.numeric(X2010[X2010$Aeroporto == "Roma CIA",]$Movimenti) + as.numeric(X2010[X2010$Aeroporto == "Roma FCO",]$Movimenti)
Liguria <- as.numeric(X2010[X2010$Aeroporto == "Genova",]$Movimenti)
Lombardia <- as.numeric(X2010[X2010$Aeroporto == "Bergamo",]$Movimenti) + as.numeric(X2010[X2010$Aeroporto == "Brescia",]$Movimenti) + 
  as.numeric(X2010[X2010$Aeroporto == "Milano LIN",]$Movimenti) + as.numeric(X2010[X2010$Aeroporto == "Milano MXP",]$Movimenti)
Marche <- as.numeric(X2010[X2010$Aeroporto == "Ancona",]$Movimenti)
Molise <- 0
Piemonte <- as.numeric(X2010[X2010$Aeroporto == "Cuneo",]$Movimenti) + as.numeric(X2010[X2010$Aeroporto == "Torino",]$Movimenti)
Puglia <- as.numeric(X2010[X2010$Aeroporto == "Bari",]$Movimenti) + as.numeric(X2010[X2010$Aeroporto == "Brindisi",]$Movimenti) +
  as.numeric(X2010[X2010$Aeroporto == "Foggia",]$Movimenti)
Sardegna <- as.numeric(X2010[X2010$Aeroporto == "Alghero",]$Movimenti) + as.numeric(X2010[X2010$Aeroporto == "Olbia",]$Movimenti) + 
  as.numeric(X2010[X2010$Aeroporto == "Cagliari",]$Movimenti)
Sicilia <- as.numeric(X2010[X2010$Aeroporto == "Catania",]$Movimenti) + as.numeric(X2010[X2010$Aeroporto == "Palermo",]$Movimenti) 
Toscana <- as.numeric(X2010[X2010$Aeroporto == "Firenze",]$Movimenti) 
TrentinoAltoAdige <- as.numeric(X2010[X2010$Aeroporto == "Bolzano",]$Movimenti)
Umbria <- as.numeric(X2010[X2010$Aeroporto == "Perugia",]$Movimenti)
ValledAosta <- 0
Veneto <- as.numeric(X2010[X2010$Aeroporto == "Venezia",]$Movimenti) + as.numeric(X2010[X2010$Aeroporto == "Verona",]$Movimenti) +
  as.numeric(X2010[X2010$Aeroporto == "Treviso",]$Movimenti)
mov2010 <- data.frame(Abruzzo, Basilicata, Calabria, Campania,
                      EmiliaRomagna, FriuliVeneziaGiulia, Lazio,
                      Liguria, Lombardia, Marche, Molise, Piemonte,
                      Puglia, Sardegna, Sicilia, Toscana, 
                      TrentinoAltoAdige, Umbria, ValledAosta, Veneto)
Abruzzo <- as.numeric(X2010[X2010$Aeroporto == "Pescara",]$Passeggeri)
Basilicata <- 0
Calabria <- as.numeric(X2010[X2010$Aeroporto == "Lamezia T.",]$Passeggeri) + as.numeric(X2010[X2010$Aeroporto == "Reggio Cal.",]$Passeggeri) +   as.numeric(X2010[X2010$Aeroporto == "Crotone",]$Passeggeri)
Campania <- as.numeric(X2010[X2010$Aeroporto == "Napoli",]$Passeggeri)
EmiliaRomagna <- as.numeric(X2010[X2010$Aeroporto == "Bologna",]$Passeggeri) + as.numeric(X2010[X2010$Aeroporto == "Forli'",]$Passeggeri) +
  as.numeric(X2010[X2010$Aeroporto == "Parma",]$Passeggeri) + as.numeric(X2010[X2010$Aeroporto == "Rimini",]$Passeggeri)
FriuliVeneziaGiulia <- as.numeric(X2010[X2010$Aeroporto == "Trieste - Ronchi dei L.",]$Passeggeri)
Lazio <- as.numeric(X2010[X2010$Aeroporto == "Roma CIA",]$Passeggeri) + as.numeric(X2010[X2010$Aeroporto == "Roma FCO",]$Passeggeri)
Liguria <- as.numeric(X2010[X2010$Aeroporto == "Genova",]$Passeggeri)
Lombardia <- as.numeric(X2010[X2010$Aeroporto == "Bergamo",]$Passeggeri) + as.numeric(X2010[X2010$Aeroporto == "Brescia",]$Passeggeri) + 
  as.numeric(X2010[X2010$Aeroporto == "Milano LIN",]$Passeggeri) + as.numeric(X2010[X2010$Aeroporto == "Milano MXP",]$Passeggeri)
Marche <- as.numeric(X2010[X2010$Aeroporto == "Ancona",]$Passeggeri)
Molise <- 0
Piemonte <- as.numeric(X2010[X2010$Aeroporto == "Cuneo",]$Passeggeri) + as.numeric(X2010[X2010$Aeroporto == "Torino",]$Passeggeri)
Puglia <- as.numeric(X2010[X2010$Aeroporto == "Bari",]$Passeggeri) + as.numeric(X2010[X2010$Aeroporto == "Brindisi",]$Passeggeri) +
  as.numeric(X2010[X2010$Aeroporto == "Foggia",]$Passeggeri)
Sardegna <- as.numeric(X2010[X2010$Aeroporto == "Alghero",]$Passeggeri) + as.numeric(X2010[X2010$Aeroporto == "Olbia",]$Passeggeri) + 
  as.numeric(X2010[X2010$Aeroporto == "Cagliari",]$Passeggeri)
Sicilia <- as.numeric(X2010[X2010$Aeroporto == "Catania",]$Passeggeri) + as.numeric(X2010[X2010$Aeroporto == "Palermo",]$Passeggeri) 
Toscana <- as.numeric(X2010[X2010$Aeroporto == "Firenze",]$Passeggeri) 
TrentinoAltoAdige <- as.numeric(X2010[X2010$Aeroporto == "Bolzano",]$Passeggeri)
Umbria <- as.numeric(X2010[X2010$Aeroporto == "Perugia",]$Passeggeri)
ValledAosta <- 0
Veneto <- as.numeric(X2010[X2010$Aeroporto == "Venezia",]$Passeggeri) + as.numeric(X2010[X2010$Aeroporto == "Verona",]$Passeggeri) +
  as.numeric(X2010[X2010$Aeroporto == "Treviso",]$Passeggeri)
pas2010 <- data.frame(Abruzzo, Basilicata, Calabria, Campania,
                      EmiliaRomagna, FriuliVeneziaGiulia, Lazio,
                      Liguria, Lombardia, Marche, Molise, Piemonte,
                      Puglia, Sardegna, Sicilia, Toscana, 
                      TrentinoAltoAdige, Umbria, ValledAosta, Veneto)

Abruzzo <- as.numeric(X2020[X2020$Aeroporto == "Pescara",]$Movimenti)
Basilicata <- 0
Calabria <- as.numeric(X2020[X2020$Aeroporto == "Lamezia Terme",]$Movimenti) + as.numeric(X2020[X2020$Aeroporto == "Reggio Calabria",]$Movimenti) +
  as.numeric(X2020[X2020$Aeroporto == "Crotone",]$Movimenti)
Campania <- as.numeric(X2020[X2020$Aeroporto == "Napoli",]$Movimenti)
EmiliaRomagna <- as.numeric(X2020[X2020$Aeroporto == "Bologna",]$Movimenti) +
  as.numeric(X2020[X2020$Aeroporto == "Parma",]$Movimenti) + as.numeric(X2020[X2020$Aeroporto == "Rimini",]$Movimenti)
FriuliVeneziaGiulia <- as.numeric(X2020[X2020$Aeroporto == "Trieste",]$Movimenti)
Lazio <- as.numeric(X2020[X2020$Aeroporto == "Roma Ciampino",]$Movimenti) + as.numeric(X2020[X2020$Aeroporto == "Roma Fiumicino",]$Movimenti)
Liguria <- as.numeric(X2020[X2020$Aeroporto == "Genova",]$Movimenti)
Lombardia <- as.numeric(X2020[X2020$Aeroporto == "Bergamo",]$Movimenti) + as.numeric(X2020[X2020$Aeroporto == "Brescia",]$Movimenti) + 
  as.numeric(X2020[X2020$Aeroporto == "Milano Linate",]$Movimenti) + as.numeric(X2020[X2020$Aeroporto == "Milano Malpensa",]$Movimenti)
Marche <- as.numeric(X2020[X2020$Aeroporto == "Ancona",]$Movimenti)
Molise <- 0
Piemonte <- as.numeric(X2020[X2020$Aeroporto == "Cuneo",]$Movimenti) + as.numeric(X2020[X2020$Aeroporto == "Torino",]$Movimenti)
Puglia <- as.numeric(X2020[X2020$Aeroporto == "Bari",]$Movimenti) + as.numeric(X2020[X2020$Aeroporto == "Brindisi",]$Movimenti) +
  as.numeric(X2020[X2020$Aeroporto == "Foggia",]$Movimenti)
Sardegna <- as.numeric(X2020[X2020$Aeroporto == "Alghero",]$Movimenti) + as.numeric(X2020[X2020$Aeroporto == "Olbia",]$Movimenti) + 
  as.numeric(X2020[X2020$Aeroporto == "Cagliari",]$Movimenti)
Sicilia <- as.numeric(X2020[X2020$Aeroporto == "Catania",]$Movimenti) + as.numeric(X2020[X2020$Aeroporto == "Palermo",]$Movimenti) 
Toscana <- as.numeric(X2020[X2020$Aeroporto == "Firenze",]$Movimenti) 
TrentinoAltoAdige <- as.numeric(X2020[X2020$Aeroporto == "Bolzano",]$Movimenti)
Umbria <- as.numeric(X2020[X2020$Aeroporto == "Perugia",]$Movimenti)
ValledAosta <- 0
Veneto <- as.numeric(X2020[X2020$Aeroporto == "Venezia",]$Movimenti) + as.numeric(X2020[X2020$Aeroporto == "Verona",]$Movimenti) +
  as.numeric(X2020[X2020$Aeroporto == "Treviso",]$Movimenti)
mov2020 <- data.frame(Abruzzo, Basilicata, Calabria, Campania,
                      EmiliaRomagna, FriuliVeneziaGiulia, Lazio,
                      Liguria, Lombardia, Marche, Molise, Piemonte,
                      Puglia, Sardegna, Sicilia, Toscana, 
                      TrentinoAltoAdige, Umbria, ValledAosta, Veneto)
Abruzzo <- as.numeric(X2020[X2020$Aeroporto == "Pescara",]$Passeggeri)
Basilicata <- 0
Calabria <- as.numeric(X2020[X2020$Aeroporto == "Lamezia Terme",]$Passeggeri) + as.numeric(X2020[X2020$Aeroporto == "Reggio Calabria",]$Passeggeri) +   as.numeric(X2020[X2020$Aeroporto == "Crotone",]$Passeggeri)
Campania <- as.numeric(X2020[X2020$Aeroporto == "Napoli",]$Passeggeri)
EmiliaRomagna <- as.numeric(X2020[X2020$Aeroporto == "Bologna",]$Passeggeri) +
  as.numeric(X2020[X2020$Aeroporto == "Parma",]$Passeggeri) + as.numeric(X2020[X2020$Aeroporto == "Rimini",]$Passeggeri)
FriuliVeneziaGiulia <- as.numeric(X2020[X2020$Aeroporto == "Trieste",]$Passeggeri)
Lazio <- as.numeric(X2020[X2020$Aeroporto == "Roma Ciampino",]$Passeggeri) + as.numeric(X2020[X2020$Aeroporto == "Roma Fiumicino",]$Passeggeri)
Liguria <- as.numeric(X2020[X2020$Aeroporto == "Genova",]$Passeggeri)
Lombardia <- as.numeric(X2020[X2020$Aeroporto == "Bergamo",]$Passeggeri) + as.numeric(X2020[X2020$Aeroporto == "Brescia",]$Passeggeri) + 
  as.numeric(X2020[X2020$Aeroporto == "Milano Linate",]$Passeggeri) + as.numeric(X2020[X2020$Aeroporto == "Milano Malpensa",]$Passeggeri)
Marche <- as.numeric(X2020[X2020$Aeroporto == "Ancona",]$Passeggeri)
Molise <- 0
Piemonte <- as.numeric(X2020[X2020$Aeroporto == "Cuneo",]$Passeggeri) + as.numeric(X2020[X2020$Aeroporto == "Torino",]$Passeggeri)
Puglia <- as.numeric(X2020[X2020$Aeroporto == "Bari",]$Passeggeri) + as.numeric(X2020[X2020$Aeroporto == "Brindisi",]$Passeggeri) +
  as.numeric(X2020[X2020$Aeroporto == "Foggia",]$Passeggeri)
Sardegna <- as.numeric(X2020[X2020$Aeroporto == "Alghero",]$Passeggeri) + as.numeric(X2020[X2020$Aeroporto == "Olbia",]$Passeggeri) + 
  as.numeric(X2020[X2020$Aeroporto == "Cagliari",]$Passeggeri)
Sicilia <- as.numeric(X2020[X2020$Aeroporto == "Catania",]$Passeggeri) + as.numeric(X2020[X2020$Aeroporto == "Palermo",]$Passeggeri) 
Toscana <- as.numeric(X2020[X2020$Aeroporto == "Firenze",]$Passeggeri) 
TrentinoAltoAdige <- as.numeric(X2020[X2020$Aeroporto == "Bolzano",]$Passeggeri)
Umbria <- as.numeric(X2020[X2020$Aeroporto == "Perugia",]$Passeggeri)
ValledAosta <- 0
Veneto <- as.numeric(X2020[X2020$Aeroporto == "Venezia",]$Passeggeri) + as.numeric(X2020[X2020$Aeroporto == "Verona",]$Passeggeri) +
  as.numeric(X2020[X2020$Aeroporto == "Treviso",]$Passeggeri)
pas2020 <- data.frame(Abruzzo, Basilicata, Calabria, Campania,
                      EmiliaRomagna, FriuliVeneziaGiulia, Lazio,
                      Liguria, Lombardia, Marche, Molise, Piemonte,
                      Puglia, Sardegna, Sicilia, Toscana, 
                      TrentinoAltoAdige, Umbria, ValledAosta, Veneto)

Abruzzo <- as.numeric(X2019[X2019$Aeroporto == "Pescara",]$Movimenti)
Basilicata <- 0
Calabria <- as.numeric(X2019[X2019$Aeroporto == "Lamezia Terme",]$Movimenti) + as.numeric(X2019[X2019$Aeroporto == "Reggio Calabria",]$Movimenti) +   as.numeric(X2019[X2019$Aeroporto == "Crotone",]$Movimenti)
Campania <- as.numeric(X2019[X2019$Aeroporto == "Napoli",]$Movimenti)
EmiliaRomagna <- as.numeric(X2019[X2019$Aeroporto == "Bologna",]$Movimenti)  +
  as.numeric(X2019[X2019$Aeroporto == "Parma",]$Movimenti) + as.numeric(X2019[X2019$Aeroporto == "Rimini",]$Movimenti)
FriuliVeneziaGiulia <- as.numeric(X2019[X2019$Aeroporto == "Trieste",]$Movimenti)
Lazio <- as.numeric(X2019[X2019$Aeroporto == "Roma Ciampino",]$Movimenti) + as.numeric(X2019[X2019$Aeroporto == "Roma Fiumicino",]$Movimenti)
Liguria <- as.numeric(X2019[X2019$Aeroporto == "Genova",]$Movimenti)
Lombardia <- as.numeric(X2019[X2019$Aeroporto == "Bergamo",]$Movimenti) + as.numeric(X2019[X2019$Aeroporto == "Brescia",]$Movimenti) + 
  as.numeric(X2019[X2019$Aeroporto == "Milano Linate (*)",]$Movimenti) + as.numeric(X2019[X2019$Aeroporto == "Milano Malpensa",]$Movimenti)
Marche <- as.numeric(X2019[X2019$Aeroporto == "Ancona",]$Movimenti)
Molise <- 0
Piemonte <- as.numeric(X2019[X2019$Aeroporto == "Cuneo",]$Movimenti) + as.numeric(X2019[X2019$Aeroporto == "Torino",]$Movimenti)
Puglia <- as.numeric(X2019[X2019$Aeroporto == "Bari",]$Movimenti) + as.numeric(X2019[X2019$Aeroporto == "Brindisi",]$Movimenti) +
  as.numeric(X2019[X2019$Aeroporto == "Foggia",]$Movimenti)
Sardegna <- as.numeric(X2019[X2019$Aeroporto == "Alghero",]$Movimenti) + as.numeric(X2019[X2019$Aeroporto == "Olbia",]$Movimenti) + 
  as.numeric(X2019[X2019$Aeroporto == "Cagliari",]$Movimenti)
Sicilia <- as.numeric(X2019[X2019$Aeroporto == "Catania",]$Movimenti) + as.numeric(X2019[X2019$Aeroporto == "Palermo",]$Movimenti) 
Toscana <- as.numeric(X2019[X2019$Aeroporto == "Firenze",]$Movimenti) 
TrentinoAltoAdige <- as.numeric(X2019[X2019$Aeroporto == "Bolzano",]$Movimenti)
Umbria <- as.numeric(X2019[X2019$Aeroporto == "Perugia",]$Movimenti)
ValledAosta <- 0
Veneto <- as.numeric(X2019[X2019$Aeroporto == "Venezia",]$Movimenti) + as.numeric(X2019[X2019$Aeroporto == "Verona",]$Movimenti) +
  as.numeric(X2019[X2019$Aeroporto == "Treviso",]$Movimenti)
mov2019 <- data.frame(Abruzzo, Basilicata, Calabria, Campania,
                      EmiliaRomagna, FriuliVeneziaGiulia, Lazio,
                      Liguria, Lombardia, Marche, Molise, Piemonte,
                      Puglia, Sardegna, Sicilia, Toscana, 
                      TrentinoAltoAdige, Umbria, ValledAosta, Veneto)
Abruzzo <- as.numeric(X2019[X2019$Aeroporto == "Pescara",]$Passeggeri)
Basilicata <- 0
Calabria <- as.numeric(X2019[X2019$Aeroporto == "Lamezia Terme",]$Passeggeri) + as.numeric(X2019[X2019$Aeroporto == "Reggio Calabria",]$Passeggeri) +   as.numeric(X2019[X2019$Aeroporto == "Crotone",]$Passeggeri)
Campania <- as.numeric(X2019[X2019$Aeroporto == "Napoli",]$Passeggeri)
EmiliaRomagna <- as.numeric(X2019[X2019$Aeroporto == "Bologna",]$Passeggeri)  +
  as.numeric(X2019[X2019$Aeroporto == "Parma",]$Passeggeri) + as.numeric(X2019[X2019$Aeroporto == "Rimini",]$Passeggeri)
FriuliVeneziaGiulia <- as.numeric(X2019[X2019$Aeroporto == "Trieste",]$Passeggeri)
Lazio <- as.numeric(X2019[X2019$Aeroporto == "Roma Ciampino",]$Passeggeri) + as.numeric(X2019[X2019$Aeroporto == "Roma Fiumicino",]$Passeggeri)
Liguria <- as.numeric(X2019[X2019$Aeroporto == "Genova",]$Passeggeri)
Lombardia <- as.numeric(X2019[X2019$Aeroporto == "Bergamo",]$Passeggeri) + as.numeric(X2019[X2019$Aeroporto == "Brescia",]$Passeggeri) + 
  as.numeric(X2019[X2019$Aeroporto == "Milano Linate (*)",]$Passeggeri) + as.numeric(X2019[X2019$Aeroporto == "Milano Malpensa",]$Passeggeri)
Marche <- as.numeric(X2019[X2019$Aeroporto == "Ancona",]$Passeggeri)
Molise <- 0
Piemonte <- as.numeric(X2019[X2019$Aeroporto == "Cuneo",]$Passeggeri) + as.numeric(X2019[X2019$Aeroporto == "Torino",]$Passeggeri)
Puglia <- as.numeric(X2019[X2019$Aeroporto == "Bari",]$Passeggeri) + as.numeric(X2019[X2019$Aeroporto == "Brindisi",]$Passeggeri) +
  as.numeric(X2019[X2019$Aeroporto == "Foggia",]$Passeggeri)
Sardegna <- as.numeric(X2019[X2019$Aeroporto == "Alghero",]$Passeggeri) + as.numeric(X2019[X2019$Aeroporto == "Olbia",]$Passeggeri) + 
  as.numeric(X2019[X2019$Aeroporto == "Cagliari",]$Passeggeri)
Sicilia <- as.numeric(X2019[X2019$Aeroporto == "Catania",]$Passeggeri) + as.numeric(X2019[X2019$Aeroporto == "Palermo",]$Passeggeri) 
Toscana <- as.numeric(X2019[X2019$Aeroporto == "Firenze",]$Passeggeri) 
TrentinoAltoAdige <- as.numeric(X2019[X2019$Aeroporto == "Bolzano",]$Passeggeri)
Umbria <- as.numeric(X2019[X2019$Aeroporto == "Perugia",]$Passeggeri)
ValledAosta <- 0
Veneto <- as.numeric(X2019[X2019$Aeroporto == "Venezia",]$Passeggeri) + as.numeric(X2019[X2019$Aeroporto == "Verona",]$Passeggeri) +
  as.numeric(X2019[X2019$Aeroporto == "Treviso",]$Passeggeri)
pas2019 <- data.frame(Abruzzo, Basilicata, Calabria, Campania,
                      EmiliaRomagna, FriuliVeneziaGiulia, Lazio,
                      Liguria, Lombardia, Marche, Molise, Piemonte,
                      Puglia, Sardegna, Sicilia, Toscana, 
                      TrentinoAltoAdige, Umbria, ValledAosta, Veneto)


# Preparo i dati finali

# Mi servono due tipi di database: in serie storica e istantanei in 5 anni
# I dati in serie storica li voglio aggregati per macroaree, annuali e destagionalizzati
# Parto da questi
# Dataframe dei tassi turistici
#Tur_ts
# Dataframe dei movimenti aerei e dei passeggeri
#mov_ts 
#pas_ts 
# Dataframe del Pil
#Pil_ts
# Valore aggiunto del lavoro nel turismo NB dati dal 2000 al 2015
# Lo stesso discorso vale per produttività e unità lavorative
#ValAgg_ts
#Produttivita_ts
#Ula_ts
# Ora la spesa effettuata in ciascuna regione e per i vari motivo
#SpesaRegione_ts
#SpesaMotivo_ts
# Infine la popolazione
#Popolazione_ts

# Creo il vettore delle regioni
Regioni <- c("Abruzzo", "Basilicata", "Calabria", "Campania", "EmiliaRomagna", "FriuliVeneziaGiulia",
             "Lazio", "Liguria", "Lombardia", "Marche", "Molise", "Piemonte", "Puglia",
             "Sardegna", "Sicilia", "Toscana", "TrentinoAltoAdige", "Umbria",
             "ValledAosta", "Veneto")

# Mi occupo del secondo tipo di database: sono istantanei, divisi per regione
# Questi database fanno riferimento a più periodi temporali
# 2000, 2010, 2019, 2020
# Devono quindi essere 4 database, ciascuno che fa riferimento ad un momento preciso
# Creo il database del 2000
Turismo <- Turismo[order(Turismo$Regione),]
BlueFlag <- BlueFlag[order(BlueFlag$Regione),]
Parchi <- Parchi[order(Parchi$Regione),]
Pil <- Pil[order(Pil$`Seleziona periodo`),]
Produttivita <- Produttivita[order(Produttivita$...2),]
colnames(SpesaRegione) <- sort(colnames(SpesaRegione))
Superficie <- Superficie[order(Superficie$`Regione/Provincia`),]
Ula <- Ula[order(Ula$...2),]
Unesco <- Unesco[order(Unesco$Regioni),]
ValAgg <- ValAgg[order(ValAgg$...2),]

data2000 <- data.frame(Regioni, turismo = as.vector(Turismo[c(6:11,13:26),2]), bandiereblu = BlueFlag[1:20,2],
                       parchiIn = Parchi$Parchi.propri, parchiTot = Parchi$Parchi.totali, 
                       pil = Pil[,2], produttivita = Produttivita[,3],
                        ula = Ula[,3], unescoIn= Unesco$Patrimoni.Propri,
                       unescoTot = Unesco$Patrimoni.Condivisi, valagg = ValAgg[,3])
names(data2000)[c(2,6:8,11)] <- c("turismo", "pil", "produttivita", "ula", "valagg")
data2000 <- cbind(data2000, movimenti = as.numeric(t(mov2000[1,])), passeggeri = as.numeric(t(pas2000[1,])),
                  popolazione = as.numeric(t(Popolazione[1,2:21])), spesa = as.numeric(t(SpesaRegione[1,2:21])) )
data2000$turismo <- as.numeric(data2000$turismo)
data2010 <- data.frame(Regioni, turismo = as.vector(Turismo[c(6:11,13:26),12]), bandiereblu = BlueFlag[1:20,2],
                       parchiIn = Parchi$Parchi.propri, parchiTot = Parchi$Parchi.totali, 
                       pil = Pil[,12], produttivita = Produttivita[,13],
                       ula = Ula[,13], unescoIn= Unesco$Patrimoni.Propri,
                       unescoTot = Unesco$Patrimoni.Condivisi, valagg = ValAgg[,13])
names(data2010)[c(2,6:8,11)] <- c("turismo", "pil", "produttivita", "ula", "valagg")
data2010 <- cbind(data2010, movimenti = as.numeric(t(mov2010[1,])), passeggeri = as.numeric(t(pas2010[1,])),
                  popolazione = as.numeric(t(Popolazione[11,2:21])), spesa = as.numeric(t(SpesaRegione[11,2:21])) )
data2010$turismo <- as.numeric(data2010$turismo)
data2019 <- data.frame(Regioni, turismo = as.vector(Turismo[c(6:11,13:26),21]), bandiereblu = BlueFlag[1:20,2],
                       parchiIn = Parchi$Parchi.propri, parchiTot = Parchi$Parchi.totali, 
                       pil = Pil[,21], produttivita = Produttivita[,22],
                       ula = Ula[,22], unescoIn= Unesco$Patrimoni.Propri,
                       unescoTot = Unesco$Patrimoni.Condivisi, valagg = ValAgg[,22])
names(data2019)[c(2,6:8,11)] <- c("turismo", "pil", "produttivita", "ula", "valagg")
data2019 <- cbind(data2019, movimenti = as.numeric(t(mov2019[1,])), passeggeri = as.numeric(t(pas2019[1,])),
                  popolazione = as.numeric(t(Popolazione[20,2:21])), spesa = as.numeric(t(SpesaRegione[20,2:21])) )
data2019$turismo <- as.numeric(data2019$turismo)
data2020 <- data.frame(Regioni, turismo = as.vector(Turismo[c(6:11,13:26),22]), bandiereblu = BlueFlag[1:20,2],
                       parchiIn = Parchi$Parchi.propri, parchiTot = Parchi$Parchi.totali, 
                       pil = Pil[,22], produttivita = Produttivita[,23],
                       ula = Ula[,23], unescoIn= Unesco$Patrimoni.Propri,
                       unescoTot = Unesco$Patrimoni.Condivisi, valagg = ValAgg[,23])
names(data2020)[c(2,6:8,11)] <- c("turismo", "pil", "produttivita", "ula", "valagg")
data2020 <- cbind(data2020, movimenti = as.numeric(t(mov2000[1,])), passeggeri = as.numeric(t(pas2020[1,])),
                  popolazione = as.numeric(t(Popolazione[21,2:21])), spesa = as.numeric(t(SpesaRegione[21,2:21])) )
data2020$turismo <- as.numeric(data2020$turismo)
# Elimino tutte le variabili da workspace tranne quelle sopraelencate
keep <- c("Tur_ts", "mov_ts", "pas_ts", "Pil_ts", "Val_agg_ts", "Produttivita_ts", 
          "Ula_ts", "SpesaRegione_ts", "SpesaMotivo_ts", "Popolazione_ts", "data2000", 
          "data2010", "data2019", "data2020")
rm(list=setdiff(ls(), keep))
