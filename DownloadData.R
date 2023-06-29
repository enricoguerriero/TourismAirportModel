
# SCARICO TUTTI I DATI UTILI PER IL PROGETTO

# Come prima cosa scarico i dati sul traffico aereo e li analizzo
# Setto la working directory
setwd("C:/Users/enric/Desktop/Progetto Modelli/AirData")
# Adesso scarico i dati
# Visto che sono 272 database diversi, cerco di gestire il download con un ciclo for
# Per prima cosa importo la libreria per leggere i file excel
library(readxl)
#Un'altra libreria per selezionare righe dei dataframe (per usare %<%)
library(dplyr)
# scarico 2 database solo per salvare il vettore degli aeroporti
SettingDataFrame1 <- read_excel("2000_01.xls", skip = 1)
# estraggo il vettore con il nome degli aeroporti
Aeroporto <- SettingDataFrame1$Aeroporto
# preparo i 3 dataframe che mi andrò a salvare dal ciclo for
# mov sono i movimenti
# pas sono i passeggeri
# car sono i cargo in tonnellate
# aggiungo alle colonne anche la colonna data
mov <- data.frame(matrix(ncol = 37))
colnames(mov)[1:37] <- c("Data", Aeroporto)
pas <- data.frame(matrix(ncol = 37))
colnames(pas)[1:37] <- c("Data", Aeroporto)
car <- data.frame(matrix(ncol = 37))
colnames(car)[1:37] <- c("Data", Aeroporto)
# la prima riga la devo salvare in sostituzione agli NA
startingmov <- SettingDataFrame1$Movimenti
mov[1,] <- c("1/2000",startingmov)
startingpas <- SettingDataFrame1$Passeggeri
pas[1,] <- c("1/2000",startingpas)
startingcar <- SettingDataFrame1$`Cargo (tons)`
car[1,] <- c("1/2000",startingcar)
# ora vado con il ciclo for
# è un doppio ciclo, la variabile y è per l'anno, la m è per il mese
for (y in 2000:2022) {
  for (m in 1:12){
    # come prima cosa salto la prima iterazione essendo già stata compiuta
    if (y == 2000 & m == 1){
      next
    }
    # i dati sono salvati in file del tipo "2000_01.xls"
    # provo a gestirli come stringhe
    if (m<10){
      FileName <- paste(y, "_0", m, ".xls", sep = "")
    }
    else {
      FileName <- paste(y, "_", m, ".xls", sep = "")
    }
    # quando nascono nuovi aeroporti vanno aggiunti al dataframe
    # aggiungo chirurgicamente i nuovi aeroporti
    if (y == 2001 & m == 4) {
      mycolname <- "Trapani"
      mycol <- rep(NA, nrow(mov))
      mov <- data.frame(mov[,1:33], mycol, mov[,34:36])
      colnames(mov)[34] <- mycolname
      pas <- data.frame(pas[,1:33], mycol, pas[,34:36])
      colnames(pas)[34] <- mycolname
      car <- data.frame(car[,1:33], mycol, car[,34:36])
      colnames(car)[34] <- mycolname
    }
    if (y == 2005 & m == 1) {
      mycolname <- "Siena"
      mycol <- rep(NA, nrow(mov))
      mov <- data.frame(mov[,1:31], mycol, mov[,32:37])
      colnames(mov)[32] <- mycolname
      pas <- data.frame(pas[,1:31], mycol, pas[,32:37])
      colnames(pas)[32] <- mycolname
      car <- data.frame(car[,1:31], mycol, car[,32:37])
      colnames(car)[32] <- mycolname
      }
    if (y == 2012 & m == 1) {
      mycolname <- "Grosseto"
      mycol <- rep(NA, nrow(mov))
      mov <- data.frame(mov[,1:17], mycol, mov[,18:38])
      colnames(mov)[18] <- mycolname
      pas <- data.frame(pas[,1:17], mycol, pas[,18:38])
      colnames(pas)[18] <- mycolname
      car <- data.frame(car[,1:17], mycol, car[,18:38])
      colnames(car)[18] <- mycolname
      }
    # Giugno 2001 e dicembre 2022 non esistono, quindi evito il loro conteggio
    if ((m == 6 & y == 2001) | (m == 12 & y == 2022)) {
      # Creo il nome della nuova colonna dei dataframe
      namerow <- paste(m, "/", y, sep="")
      # Creo la colonna vuota
      myrow <- rep(NA,ncol(mov))
      # Aggiungo la colonna vuota ai dataframe
      mov <- rbind(mov, myrow)
      pas <- rbind(pas, myrow)
      car <- rbind(car, myrow)
      # Cambio il nome della colonna con la data
      mov[nrow(mov),1] = namerow
      pas[nrow(pas),1] = namerow
      car[nrow(car),1] = namerow
      # a questo punto l'iterazione può finire
      next
    }
    else {
      # Ad una certa alcuni aeroporti chiudono, quindi bisogna sostituire degli NA ai loro valori
      # Creo una prima iterazione per il periodo in cui ancora non aveva chiuso nessun aeroporto
      if (y < 2014) {
        # Ho salvato il nome del file, ora creo la table che viene sovrascritta ogni iterazione
      MyFrame <- read_excel(FileName, skip = 1)
      # Creo il nome della nuova colonna dei dataframe
      namerow <- paste(m, "/", y, sep="")
      # Aggiungo le colonne ai dataframe
      mov <- rbind(mov,c(namerow, MyFrame$Movimenti))
      pas <- rbind(pas,c(namerow, MyFrame$Passeggeri))
      car <- rbind(car,c(namerow, MyFrame$`Cargo (tons)`))
      # Cambio il nome della colonna con la data
      #mov[nrow(mov),1] = namerow
      }
      # fino al 2014 il ciclo funziona, ora bisogna gestire gli aeroporti chiusi
      # a gennaio 2014 mancano:
      # Crotone (11)
      # Forlì (15)
      # Siena (32)
      # vanno quindi riempite le righe 11, 15 e 32 con NA fino a ulteriori novità
      if (y >= 2014) {
        # Scarico i dati come sempre
        MyFrame <- read_excel(FileName, skip = 1)
        # prendo i dati in un vettore
        vecmov <- MyFrame$Movimenti
        vecpas <- MyFrame$Passeggeri
        veccar <- MyFrame$`Cargo (tons)`
        # Aggiungo al vettore gli NA nelle posizioni 11, 15 e 32
        vecmov <- c(vecmov[1:11], NA, vecmov[12:14], NA, vecmov[15:30], NA, vecmov[31:39])
        vecpas <- c(vecpas[1:11], NA, vecpas[12:14], NA, vecpas[15:30], NA, vecpas[31:39])
        veccar <- c(veccar[1:11], NA, veccar[12:14], NA, veccar[15:30], NA, veccar[31:39])
        # Creo il nome della nuova colonna dei dataframe
        namerow <- paste(m, "/", y, sep="")
        # Cambio il nome della colonna con la data
        #mov[nrow(mov),1] = namerow
        # Ora posso aggiungere questo vettore al database
        mov <- rbind(mov, c(namerow, vecmov))
        pas <- rbind(pas, c(namerow, vecpas))
        car <- rbind(car, c(namerow, veccar))
      }
    }
  }
}

# Ora è il momento di scaricare i dati sul turismo
# Come prima cosa setto la directory
setwd("C:/Users/enric/Desktop/Progetto Modelli/TourismData")
# Ora è necessario scaricare i dati
# Devo scegliere quali variabili esplicative "estrarre"
# Partiamo dal dataset del'istat
# come prima cosa prendo l'indice di turisticità
Turismo <- read_excel("Turismo.xls", 
                      sheet = "Ind. 105", skip = 7)
# Tiro giù i dati aggregati
Tur_agg <- Turismo[c(25,26,27,30,31),]
Tur_agg <- Tur_agg[,c(-1,-3:-7,-24,-25)]
# Ora questo database va ripulito da tutti i dati inutili
Turismo <- Turismo %>%  filter(!row_number() %in% c(5, 6, 24, 28, 29, 32:49))
# Togliamo i dati dell'altro millennio
Turismo <- Turismo[, -3:-7]
# Rimuoviamo le ultime due colonne perché sono vuote
Turismo <- Turismo[, -24:-25]
# Anche la prima colonna è inutile
Turismo <- Turismo[, -1]
# Cambio il nome della colonna delle regioni
names(Turismo)[1] <- "Regione"
# Questo indice è pronto all'utilizzo
# La seconda variabile è la produttività del lavoro nel turismo
Produttivita <- read_excel("Turismo.xls", 
                      sheet = "Ind. 132", skip = 7)
# Tiro giù i dati aggregati
Produttivita_agg <- Produttivita[c(25,26,27,30,31),]
Produttivita_agg <- Produttivita_agg[,c(-1,-3:-7,-24,-25)]
# Ora questo database va ripulito da tutti i dati inutili
Produttivita <- Produttivita %>%  filter(!row_number() %in% c(5, 6, 23:49))
# Togliamo i dati dell'altro millennio
Produttivita <- Produttivita[, -3:-7]
# Rimuoviamo le ultime due colonne perché sono vuote
Produttivita <- Produttivita[, -24:-25]
# Purtroppo i dati di questa variabile sono scarsi, arrivano fino al 2016
# Proviamo anche il valore aggiunto del turismo
ValAgg <- read_excel("Turismo.xls", 
                           sheet = "Dati 932", skip = 7)
# Tiro giù i dati aggregati
ValAgg_agg <- ValAgg[c(26,27,28,31,32),]
ValAgg_agg <- ValAgg_agg[,c(-1,-3:-7,-24,-25)]
# Ora questo database va ripulito da tutti i dati inutili
ValAgg <- ValAgg %>%  filter(!row_number() %in% c(5, 6, 23:49))
# Togliamo i dati dell'altro millennio
ValAgg <- ValAgg[, -3:-7]
# Rimuoviamo le ultime due colonne perché sono vuote
ValAgg <- ValAgg[, -24:-25]
# Anche in questo caso i dati arrivano fino al 2016
# Come ultimo proviamo con le unità di lavoro nel settore del turismo
Ula <- read_excel("Turismo.xls", 
                     sheet = "Dati 932", skip = 7)
# Tiro giù i dati aggregati
Ula_agg <- Ula[c(26,27,28,31,32),]
Ula_agg <- Ula_agg[,c(-1,-3:-7,-24,-25)]
# Ora questo database va ripulito da tutti i dati inutili
Ula <- Ula %>%  filter(!row_number() %in% c(5, 6, 23:49))
# Togliamo i dati dell'altro millennio
Ula <- Ula[, -3:-7]
# Rimuoviamo le ultime due colonne perché sono vuote
Ula <- Ula[, -24:-25]
# Inutile sottolineare che anche questi dati arrivano fino al 2016
# Con questo database abbiamo finito

# Passiamo al database della Banca d'Italia
# Come prima variabile prendo la spesa per regione di destinazione dei viaggiatori in Italia
SpesaRegione <- read_excel("Turismo_TS_1997-2021.xlsx", 
                            sheet = "TS2-S-S", skip = 4)
# Rimuoviamo le colonne che riguardano aggregati che non ci interessano
SpesaRegione <- SpesaRegione[,-3]
SpesaRegione <- SpesaRegione[,-7]
SpesaRegione <- SpesaRegione[,-11]
SpesaRegione <- SpesaRegione[,-15]
SpesaRegione <- SpesaRegione[,-23:-24]
# Rimuoviamo i dati dell'altro millennio (le prime 12 righe)
SpesaRegione <- SpesaRegione %>% filter(!row_number() %in% c(1:12, 101:102))
# Dataframe pronto
# Il prossimo è la spesa per motivo del viaggio
SpesaMotivo <- read_excel("Turismo_TS_1997-2021.xlsx", 
                           sheet = "TS3-S-S", skip = 4)
# Rimuoviamo i dati dell'altro millennio (le prime 12 righe)
SpesaMotivo <- SpesaMotivo %>% filter(!row_number() %in% c(1:12, 101:102))
# Rinominiamo le colonne per essere più comprensibili
names(SpesaMotivo) <- c("Anno", "Trimestre","Lavoro","Personali","Vacanza","Totale")
# Dataframe pronto

# Adesso è il momento di occuparsi dei dati riguardanti le regioni
# Dai ultimi tipo 5 database non manca molto
# Settiamo la directory
setwd("C:/Users/enric/Desktop/Progetto Modelli/RegionData")
# Dati sulle bandiere blu
BlueFlag <- read.csv("BlueFlag.csv")
# Dati sui siti unesco
Unesco <- read.csv("Unesco.csv")
# Dati sui parchi naturali
Parchi <- read.csv("Parchi.csv")
# mamma mia easy questi 3
# Ora passiamo al PIL delle regioni, si torna ai file excel
Pil <- read_excel("PilRegionale.xlsx")
# Rimuovo la seconda colonna che sono solo NA
Pil <- Pil[,-2]
# Salvo in una variabile il Pil aggregato
Pil_agg <- Pil[c(5, 10, 17, 23, 30),]
# Rimuovo sempre le stesse maledette righe con gli aggregati
Pil <- Pil %>% filter(!row_number() %in% c(1:5,10,12,13,17,22,23,30,33))

# Dai ultimo database
Superficie <- read_excel("Dati comunali e provinciali.xlsx")
# Prendo la popolazione
#Popolazione <- Superficie[,c(2,5)]
# Ora prendo solo le righe che mi interessano
#Popolazione <- Popolazione[c(1,10,12,25,28,36,41,46, 56, 67,70,76,82,87,90,96,103,106,112,122),]
# Ora levo prima di tutto le colonne che non mi interessano
Superficie <- Superficie[,c(2,4)]
# Ora prendo solo le righe che mi interessano
Superficie <- Superficie[c(1,10,12,25,28,36,41,46, 56, 67,70,76,82,87,90,96,103,106,112,122),]
# Fatto

# La popolazione tirata giù precedente però non è in serie storica
# ora mi scarico la serie storica
Popolazione0219csv <- read.csv("Popolazione0219csv.txt")
# Salvo nei vettori i vari valori della popolazione
Abruzzo <- Popolazione0219csv[Popolazione0219csv$Territorio == "Abruzzo",]$Value[145:162]
Basilicata <- Popolazione0219csv[Popolazione0219csv$Territorio == "Basilicata",]$Value[109:126]
Calabria <- Popolazione0219csv[Popolazione0219csv$Territorio == "Calabria",]$Value[127:144]
Campania <- Popolazione0219csv[Popolazione0219csv$Territorio == "Campania",]$Value[127:144]
EmiliaRomagna <- Popolazione0219csv[Popolazione0219csv$Territorio == "Emilia-Romagna",]$Value[127:144]
FriuliVeneziaGiulia <- Popolazione0219csv[Popolazione0219csv$Territorio == "Friuli-Venezia Giulia",]$Value[127:144]
Lazio <- Popolazione0219csv[Popolazione0219csv$Territorio == "Lazio",]$Value[109:126]
Liguria <- Popolazione0219csv[Popolazione0219csv$Territorio == "Liguria",]$Value[109:126]
Lombardia <- Popolazione0219csv[Popolazione0219csv$Territorio == "Lombardia",]$Value[127:144]
Marche <- Popolazione0219csv[Popolazione0219csv$Territorio == "Marche",]$Value[127:144]
Molise <- Popolazione0219csv[Popolazione0219csv$Territorio == "Molise",]$Value[145:162]
Piemonte <- Popolazione0219csv[Popolazione0219csv$Territorio == "Piemonte",]$Value[127:144]
Puglia <- Popolazione0219csv[Popolazione0219csv$Territorio == "Puglia",]$Value[145:162]
Sardegna <- Popolazione0219csv[Popolazione0219csv$Territorio == "Sardegna",]$Value[109:126]
Sicilia <- Popolazione0219csv[Popolazione0219csv$Territorio == "Sicilia",]$Value[109:126]
Toscana <- Popolazione0219csv[Popolazione0219csv$Territorio == "Toscana",]$Value[127:144]
TrentinoAltoAdige <- Popolazione0219csv[Popolazione0219csv$Territorio == "Trentino Alto Adige / Südtirol",]$Value[145:162]
Umbria <- Popolazione0219csv[Popolazione0219csv$Territorio == "Umbria",]$Value[127:144]
ValledAosta <- Popolazione0219csv[Popolazione0219csv$Territorio == "Valle d'Aosta / Vallée d'Aoste",]$Value[289:306]
Veneto <- Popolazione0219csv[Popolazione0219csv$Territorio == "Veneto",]$Value[127:144]
# Creo il vettore della data
Anno <- 2000:2020
# Creo il dataframe definitivo
Popolazione <- data.frame(Abruzzo, Basilicata, Calabria, Campania, EmiliaRomagna, FriuliVeneziaGiulia,
                          Lazio, Liguria, Lombardia, Marche, Molise, Piemonte, Puglia, Sardegna,
                          Sicilia, Toscana, TrentinoAltoAdige, Umbria, ValledAosta, Veneto)
# Adesso mancano i dati dei primi due anni
Popolazione0001csv <- read.csv("Popolazione0001csv.txt")
# E ora stessa roba di prima
Abruzzo <- Popolazione0001csv[Popolazione0001csv$Territorio == "Abruzzo",]$Value
Basilicata <- Popolazione0001csv[Popolazione0001csv$Territorio == "Basilicata",]$Value
Calabria <- Popolazione0001csv[Popolazione0001csv$Territorio == "Calabria",]$Value
Campania <- Popolazione0001csv[Popolazione0001csv$Territorio == "Campania",]$Value
EmiliaRomagna <- Popolazione0001csv[Popolazione0001csv$Territorio == "Emilia-Romagna",]$Value
FriuliVeneziaGiulia <- Popolazione0001csv[Popolazione0001csv$Territorio == "Friuli-Venezia Giulia",]$Value
Lazio <- Popolazione0001csv[Popolazione0001csv$Territorio == "Lazio",]$Value
Liguria <- Popolazione0001csv[Popolazione0001csv$Territorio == "Liguria",]$Value
Lombardia <- Popolazione0001csv[Popolazione0001csv$Territorio == "Lombardia",]$Value
Marche <- Popolazione0001csv[Popolazione0001csv$Territorio == "Marche",]$Value
Molise <- Popolazione0001csv[Popolazione0001csv$Territorio == "Molise",]$Value
Piemonte <- Popolazione0001csv[Popolazione0001csv$Territorio == "Piemonte",]$Value
Puglia <- Popolazione0001csv[Popolazione0001csv$Territorio == "Puglia",]$Value
Sardegna <- Popolazione0001csv[Popolazione0001csv$Territorio == "Sardegna",]$Value
Sicilia <- Popolazione0001csv[Popolazione0001csv$Territorio == "Sicilia",]$Value
Toscana <- Popolazione0001csv[Popolazione0001csv$Territorio == "Toscana",]$Value
TrentinoAltoAdige <- Popolazione0001csv[Popolazione0001csv$Territorio == "Trentino Alto Adige / Südtirol",]$Value
Umbria <- Popolazione0001csv[Popolazione0001csv$Territorio == "Umbria",]$Value
ValledAosta <- Popolazione0001csv[Popolazione0001csv$Territorio == "Valle d'Aosta / Vallée d'Aoste",]$Value
Veneto <- Popolazione0001csv[Popolazione0001csv$Territorio == "Veneto",]$Value
# Piccolo dataframe di appoggio
Popolazione2 <- data.frame(Abruzzo, Basilicata, Calabria, Campania, EmiliaRomagna, FriuliVeneziaGiulia,
                           Lazio, Liguria, Lombardia, Marche, Molise, Piemonte, Puglia, Sardegna,
                           Sicilia, Toscana, TrentinoAltoAdige, Umbria, ValledAosta, Veneto)
Popolazione2 <- Popolazione2[5:6,]
# Adesso mancano i dati dei primi due anni
Popolazione0001csv <- read.csv("Popolazione0001csv.txt")
# E ora stessa roba di prima
Abruzzo <- Popolazione0001csv[Popolazione0001csv$Territorio == "Abruzzo",]$Value
Basilicata <- Popolazione0001csv[Popolazione0001csv$Territorio == "Basilicata",]$Value
Calabria <- Popolazione0001csv[Popolazione0001csv$Territorio == "Calabria",]$Value
Campania <- Popolazione0001csv[Popolazione0001csv$Territorio == "Campania",]$Value
EmiliaRomagna <- Popolazione0001csv[Popolazione0001csv$Territorio == "Emilia-Romagna",]$Value
FriuliVeneziaGiulia <- Popolazione0001csv[Popolazione0001csv$Territorio == "Friuli-Venezia Giulia",]$Value
Lazio <- Popolazione0001csv[Popolazione0001csv$Territorio == "Lazio",]$Value
Liguria <- Popolazione0001csv[Popolazione0001csv$Territorio == "Liguria",]$Value
Lombardia <- Popolazione0001csv[Popolazione0001csv$Territorio == "Lombardia",]$Value
Marche <- Popolazione0001csv[Popolazione0001csv$Territorio == "Marche",]$Value
Molise <- Popolazione0001csv[Popolazione0001csv$Territorio == "Molise",]$Value
Piemonte <- Popolazione0001csv[Popolazione0001csv$Territorio == "Piemonte",]$Value
Puglia <- Popolazione0001csv[Popolazione0001csv$Territorio == "Puglia",]$Value
Sardegna <- Popolazione0001csv[Popolazione0001csv$Territorio == "Sardegna",]$Value
Sicilia <- Popolazione0001csv[Popolazione0001csv$Territorio == "Sicilia",]$Value
Toscana <- Popolazione0001csv[Popolazione0001csv$Territorio == "Toscana",]$Value
TrentinoAltoAdige <- Popolazione0001csv[Popolazione0001csv$Territorio == "Trentino Alto Adige / Südtirol",]$Value
Umbria <- Popolazione0001csv[Popolazione0001csv$Territorio == "Umbria",]$Value
ValledAosta <- Popolazione0001csv[Popolazione0001csv$Territorio == "Valle d'Aosta / Vallée d'Aoste",]$Value
Veneto <- Popolazione0001csv[Popolazione0001csv$Territorio == "Veneto",]$Value
# Piccolo dataframe di appoggio
Popolazione2 <- data.frame(Abruzzo, Basilicata, Calabria, Campania, EmiliaRomagna, FriuliVeneziaGiulia,
                           Lazio, Liguria, Lombardia, Marche, Molise, Piemonte, Puglia, Sardegna,
                           Sicilia, Toscana, TrentinoAltoAdige, Umbria, ValledAosta, Veneto)
Popolazione2 <- Popolazione2[5:6,]
# Adesso mancano i dati dei primi due anni
Popolazione2021csv <- read.csv("Popolazione2021csv.txt")
# E ora stessa roba di prima
Abruzzo <- Popolazione2021csv[Popolazione2021csv$Territorio == "Abruzzo",]
Abruzzo <- Abruzzo[Abruzzo$ETA1=="TOTAL" & Abruzzo$Sesso == "totale" & Abruzzo$Stato.civile=="totale",]$Value
Basilicata <- Popolazione2021csv[Popolazione2021csv$Territorio == "Basilicata",]
Basilicata <- Basilicata[Basilicata$ETA1=="TOTAL" & Basilicata$Sesso == "totale" & Basilicata$Stato.civile=="totale",]$Value
Calabria <- Popolazione2021csv[Popolazione2021csv$Territorio == "Calabria",]
Calabria <- Calabria[Calabria$ETA1=="TOTAL" & Calabria$Sesso == "totale" & Calabria$Stato.civile=="totale",]$Value
Campania <- Popolazione2021csv[Popolazione2021csv$Territorio == "Campania",]
Campania <- Campania[Campania$ETA1=="TOTAL" & Campania$Sesso == "totale" & Campania$Stato.civile=="totale",]$Value
EmiliaRomagna <- Popolazione2021csv[Popolazione2021csv$Territorio == "Emilia-Romagna",]
EmiliaRomagna <- EmiliaRomagna[EmiliaRomagna$ETA1=="TOTAL" & EmiliaRomagna$Sesso == "totale" & EmiliaRomagna$Stato.civile=="totale",]$Value
FriuliVeneziaGiulia <- Popolazione2021csv[Popolazione2021csv$Territorio == "Friuli-Venezia Giulia",]
FriuliVeneziaGiulia <- FriuliVeneziaGiulia[FriuliVeneziaGiulia$ETA1=="TOTAL" & FriuliVeneziaGiulia$Sesso == "totale" & FriuliVeneziaGiulia$Stato.civile=="totale",]$Value
Lazio <- Popolazione2021csv[Popolazione2021csv$Territorio == "Lazio",]
Lazio <- Lazio[Lazio$ETA1=="TOTAL" & Lazio$Sesso == "totale" & Lazio$Stato.civile=="totale",]$Value
Liguria <- Popolazione2021csv[Popolazione2021csv$Territorio == "Liguria",]
Liguria <- Liguria[Liguria$ETA1=="TOTAL" & Liguria$Sesso == "totale" & Liguria$Stato.civile=="totale",]$Value
Lombardia <- Popolazione2021csv[Popolazione2021csv$Territorio == "Lombardia",]
Lombardia <- Lombardia[Lombardia$ETA1=="TOTAL" & Lombardia$Sesso == "totale" & Lombardia$Stato.civile=="totale",]$Value
Marche <- Popolazione2021csv[Popolazione2021csv$Territorio == "Marche",]
Marche <- Marche[Marche$ETA1=="TOTAL" & Marche$Sesso == "totale" & Marche$Stato.civile=="totale",]$Value
Molise <- Popolazione2021csv[Popolazione2021csv$Territorio == "Molise",]
Molise <- Molise[Molise$ETA1=="TOTAL" & Molise$Sesso == "totale" & Molise$Stato.civile=="totale",]$Value
Piemonte <- Popolazione2021csv[Popolazione2021csv$Territorio == "Piemonte",]
Piemonte <- Piemonte[Piemonte$ETA1=="TOTAL" & Piemonte$Sesso == "totale" & Piemonte$Stato.civile=="totale",]$Value
Puglia <- Popolazione2021csv[Popolazione2021csv$Territorio == "Puglia",]
Puglia <- Puglia[Puglia$ETA1=="TOTAL" & Puglia$Sesso == "totale" & Puglia$Stato.civile=="totale",]$Value
Sardegna <- Popolazione2021csv[Popolazione2021csv$Territorio == "Sardegna",]
Sardegna <- Sardegna[Sardegna$ETA1=="TOTAL" & Sardegna$Sesso == "totale" & Sardegna$Stato.civile=="totale",]$Value
Sicilia <- Popolazione2021csv[Popolazione2021csv$Territorio == "Sicilia",]
Sicilia <- Sicilia[Sicilia$ETA1=="TOTAL" & Sicilia$Sesso == "totale" & Sicilia$Stato.civile=="totale",]$Value
Toscana <- Popolazione2021csv[Popolazione2021csv$Territorio == "Toscana",]
Toscana <- Toscana[Toscana$ETA1=="TOTAL" & Toscana$Sesso == "totale" & Toscana$Stato.civile=="totale",]$Value
TrentinoAltoAdige <- Popolazione2021csv[Popolazione2021csv$Territorio == "Trentino Alto Adige / Südtirol",]
TrentinoAltoAdige <- TrentinoAltoAdige[TrentinoAltoAdige$ETA1=="TOTAL" & TrentinoAltoAdige$Sesso == "totale" & TrentinoAltoAdige$Stato.civile=="totale",]$Value
Umbria <- Popolazione2021csv[Popolazione2021csv$Territorio == "Umbria",]
Umbria <- Umbria[Umbria$ETA1=="TOTAL" & Umbria$Sesso == "totale" & Umbria$Stato.civile=="totale",]$Value
ValledAosta <- Popolazione2021csv[Popolazione2021csv$Territorio == "Valle d'Aosta / Vallée d'Aoste",]
ValledAosta <- ValledAosta[ValledAosta$ETA1=="TOTAL" & ValledAosta$Sesso == "totale" & ValledAosta$Stato.civile=="totale",]$Value[c(1,2)]
Veneto <- Popolazione2021csv[Popolazione2021csv$Territorio == "Veneto",]
Veneto <- Veneto[Veneto$ETA1=="TOTAL" & Veneto$Sesso == "totale" & Veneto$Stato.civile=="totale",]$Value
# Piccolo dataframe di appoggio
Popolazione3 <- data.frame(Abruzzo, Basilicata, Calabria, Campania, EmiliaRomagna, FriuliVeneziaGiulia,
                           Lazio, Liguria, Lombardia, Marche, Molise, Piemonte, Puglia, Sardegna,
                           Sicilia, Toscana, TrentinoAltoAdige, Umbria, ValledAosta, Veneto)
Popolazione <- rbind(Popolazione2,Popolazione, Popolazione3)
# Qui eventualmente poi mi calcolerò la popolazione media per ogni anno
# Per ora rimuovo il 2021
# Aggiungo il vettore delle date
Popolazione <- cbind(Anno, Popolazione[1:21,])

# Non ci credo finalmente ho finito
# Ora elimino dal workspace tutte le variabili che non voglio importare nel file definitivo
# Prima i dataframe
rm("MyFrame","SettingDataFrame1", "Aeroporto", "FileName", "m", "mycol", "mycolname", "myrow",
            "namerow", "startingcar", "startingmov", "startingpas", "veccar", "vecmov", "vecpas", "y",
  "Popolazione0001csv", "Popolazione0219csv", "Popolazione2", "Abruzzo", "Anno", "Basilicata",
  "Calabria", "Campania", "EmiliaRomagna", "FriuliVeneziaGiulia", "Lazio", "Liguria", "Lombardia",
  "Marche", "Molise", "Piemonte", "Puglia", "Sardegna", "Sicilia", "Toscana", "TrentinoAltoAdige","Umbria",
  "ValledAosta", "Veneto")
