
################################

# PROGETTO DI MODELLI STATISTICI

################################


# Proviamo a vedere la correlazione tra il traffico aereo e vari indici e dati
# sul turismo nelle varie regioni italiane

# Come prima cosa setto la working directory
setwd("C:/Users/enric/Desktop/Progetto Modelli")
# Ora importo tutti i dati sulle serie storiche 
source("ElaborateData.R")
# Pacchetto per i 3d plot
library(plotly)


# Parto con l'osservazione della variabile risposta, ovvero il tasso di turisticità
# Ricordo il tasso di turisticità essere il rapporto tra giornate di permanenza di un turista
# in un territorio e residenti del territorio
# Stampo un grafico della variabile risposta
plot_tur <-  ggplot(Tur_ts, aes(x = anno, y= NordEst, color = "NE")) +
  geom_line(size = 1) +
  geom_line(aes(y = NordOvest, color = "NO"), size = 1) +
  geom_line(aes(y = Centro, color = "CE"), size = 1) +
  geom_line(aes(y = Sud, color = "SU"), size = 1) +
  geom_line(aes(y = Isole, color = "IS"), size = 1) +
  xlab("Anni") +
  ylab("Valori") +
  ggtitle("Tasso di turisticità") +
  scale_color_manual(name = "Regione",
                     values = c("NE" = "blue", "NO" = "green", "CE" = "purple", "SU" = "red", "IS" = "orange"),
                     labels = c("Nord Est", "Nord Ovest", "Centro", "Sud", "Isole")) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  geom_text(aes(x = 2020, y = 15 , label = "Covid 19"), color = 'black', size = 4) +
  geom_segment(x = 2020, xend = 2020, y = 0, yend = 15, linetype = "dotted", color = "black", size = 1)

# Come prima cosa effettuo la regressione del tasso di turisticità sul tempo
# Parto con il Nord Est solo perché è dove vivo io
# Prima creo un dataframe per il nordest per poi aggiungere le variabili quando mi servono
NordEst_ts <- data.frame(anno = Tur_ts$anno, turismo =Tur_ts$NordEst)
# Ora effettuo la regressione
EasiestModel_NE <- lm(turismo ~ anno-1, data = NordEst_ts)
# Visualizzo il modello
sum_EasiestModel <- summary(EasiestModel_NE)
# La regressione è riuscita malissimo
# R quadro = 0.004131
# R quadro aggiustato = -0.04828
# Proviamo a stampare
plot_EasiestModel <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(EasiestModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Già in questo momento il problema è facilmente individuabile
# Passo ai residui per completezza
plot_EasiestModel_resid <- ggplot(NordEst_ts, aes(x = anno, y = resid(EasiestModel_NE))) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Anno", y = "Residui") + 
  theme_classic()
# Qui è decisamente evidente come il dato del 2020 sia un problema intrascurabile per il modello
# Rimuovo l'ultima riga del dataframe
#NordEst_ts <- NordEst_ts[-21,]
# Riprovo (sovrascrivo le variabili, tanto queste sono inutili)
EasiestModel_NE <- lm(turismo ~ anno, data = NordEst_ts)
# Vedo il summary
sum_EasiestModel <- summary(EasiestModel_NE)
# Non molto buono ma sicuramente molto meglio
# R quadro = 0.4714
# R quadro aggiustato = 0.3833
# Vediamo il grafico
plot_EasiestModel <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(EasiestModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Già ora, diminuendo il range sull'asse y, si nota un trend più evidente
# Ora i residui
plot_EasiestModel_resid <- ggplot(NordEst_ts, aes(x = anno, y = resid(EasiestModel_NE))) + 
  geom_line() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Anno", y = "Residui") + 
  theme_classic()
# I residui soffrono di un'evidente sistematicità
# Sembra quasi una forma di ciclicità di medio termine
# Proviamo a vedere i residui standardizzati e studentizzati
# standardizzati
plot_EasiestModel_resid_stand <- ggplot(NordEst_ts, aes(x = anno, y = scale(resid(EasiestModel_NE)))) + 
  geom_line() + 
  ggtitle("Residui Standardizzati") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Anno", y = "Residui") + 
  theme_classic()
# studentizzati
plot_EasiestModel_resid_stud <- ggplot(NordEst_ts, aes(x = anno, y = rstudent(EasiestModel_NE))) + 
  geom_line() + 
  ggtitle("Residui Studentizzati") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Anno", y = "Residui") + 
  theme_classic()
# Provo a stampare anche il boxplot
boxplot_EasiestModel_resid <- ggplot(data.frame(residuals = resid(EasiestModel_NE)), aes(x = "", y = residuals)) + 
  geom_boxplot() + 
  ggtitle("Boxplot dei Residui") +
  labs(x = NULL, y = "Valori dei Residui") + 
  theme_classic() + 
  scale_x_discrete(limits = c(""))
# Ora stampo i residui standardizzati
boxplot_EasiestModel_resid_stand <- ggplot(data.frame(standard.residuals = scale(resid(EasiestModel_NE))),
                                           aes(x = "", y = standard.residuals)) + 
  geom_boxplot() + 
  ggtitle("Boxplot dei Residui Standardizzati") +
  labs(x = NULL, y = "Valori dei Residui") + 
  theme_classic() + 
  scale_x_discrete(limits = c(""))
# Infine i residui studentizzati
boxplot_EasiestModel_resid_stud <- ggplot(data.frame(student.residuals = rstudent(EasiestModel_NE)),
                                           aes(x = "", y = student.residuals)) + 
  geom_boxplot() + 
  ggtitle("Boxplot dei Residui Studentizzati") +
  labs(x = NULL, y = "Valori dei Residui") + 
  theme_classic() + 
  scale_x_discrete(limits = c(""))
# A occhio i tre grafici si assomigliano
# Vediamo l'istogramma con la curva della normale
histplot_EasiestModel_resid_stud <- ggplot(data.frame(rstudent = rstudent(EasiestModel_NE)), aes(x = rstudent)) + 
  geom_histogram(aes(y = ..density..), fill = "blue", alpha = 0.5, bins = 10) +
  geom_density(color = "red") +
  ggtitle("Istogramma dei residui studentizzati") +
  xlab("Studentized Residuals") +
  ylab("Density") + 
  theme_classic() 
# Continuo con i test sulla normalità solo per farli tutti,
# ma è evidente che le ipotesi non sono rispettate
# Adesso faccio la prova con il grafico dei quantili
qqplot_EasiestModel_resid <- ggplot(data.frame(resid = resid(EasiestModel_NE)), aes(sample = resid)) + 
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("Q-Q Plot dei Residui") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_classic() 
# Provo il plot generale 
#plot(EasiestModel_NE)
# Rimane commentato così quando runno il programma non stampa

# Introduco una seconda variabile, su cui effettuerò la prima regressione (semplice)
# Provo con i passeggeri che transitano sulle regioni interessati
# Prima piccolo plot dei passeggeri
plot_pas <-  ggplot(pas_ts, aes(x = anno, y= NordEst, color = "NE")) +
  geom_line(size = 1) +
  geom_line(aes(y = NordOvest, color = "NO"), size = 1) +
  geom_line(aes(y = Centro, color = "CE"), size = 1) +
  geom_line(aes(y = Sud, color = "SU"), size = 1) +
  geom_line(aes(y = Isole, color = "IS"), size = 1) +
  xlab("Anni") +
  ylab("Valori") +
  ggtitle("Numero di passeggeri") +
  scale_color_manual(name = "Regione",
                     values = c("NE" = "blue", "NO" = "green", "CE" = "purple", "SU" = "red", "IS" = "orange"),
                     labels = c("Nord Est", "Nord Ovest", "Centro", "Sud", "Isole")) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  geom_text(aes(x = 2020, y = 6*10^8 , label = "Covid"), color = 'black', size = 4) +
  geom_segment(x = 2020, xend = 2020, y = 0, yend = 6*10^8, linetype = "dotted", color = "black", size = 1)
# Il numero di passeggeri è molto più pazzerello rispetto al tasso di turisticità

# Provo come prima cosa a fare un modello di regressione del tasso sui passeggeri
# Prima devo aggiungere al dataframe di NordEst la variabile passeggeri
NordEst_ts <- cbind(NordEst_ts, passeggeri = pas_ts$NordEst)
# Ora faccio la prima regressione
PasModel_NE <- lm(turismo ~ passeggeri, data = NordEst_ts)
# vediamo il summary
sum_PasModel_NE <- summary(PasModel_NE)
# l'R quadro non è bassissimo (0.4524)
# anche l'R quadro aggiustato bene (0.4321)
# il coefficiente è significativo, però ha un valore molto basso
# Stampo i dati e vediamo che è
plot_PasModel_NE <- ggplot(NordEst_ts, aes(x=passeggeri, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PasModel_NE)), color="red") +
  xlab("Passeggeri") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Ci sta
# Vediamo se con i residui sono più fortunato
plot_PasModel_NE_resid <- ggplot(NordEst_ts, aes(x = passeggeri, y = resid(PasModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Passeggeri", y = "Residui") + 
  theme_classic()
# I residui non sembrano niente male
# Come modello di partenza è molto buono

# Adesso è arrivato quel momento
# Modello lineare multiplo
# Incrocio il tempo e il numero di passeggeri e vediamo che viene
PasTimeModel_NE <- lm(turismo ~ anno + passeggeri, data = NordEst_ts)
# Vediamo come esce sto modello
sum_PasTimeModel_NE <- summary(PasTimeModel_NE)
# L'R quadro non è migliorato molto rispetto ai modelli semplici (0.5301)
# Idem per l'R quadro aggiustato (0.4748)
# Probelmone: I coefficienti non sono per niente significativi
# Provo a togliere l'intercetta
PasTimeModel_NE <- lm(turismo ~ anno + passeggeri -1, data = NordEst_ts)
# Provo a fare un plot sull'asse temporale
plot_PasTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PasTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Il grafico non sembra male dai
# Stampo i residui
plot_PasTimeModel_NE_resid1 <- ggplot(NordEst_ts, aes(x = anno, y = resid(PasTimeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Passeggeri", y = "Residui") + 
  theme_classic()
plot_PasTimeModel_NE_resid2 <- ggplot(NordEst_ts, aes(x = passeggeri, y = resid(PasTimeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Passeggeri", y = "Residui") + 
  theme_classic()

# Vediamo come va il modello se al posto dei passeggeri metto i movimenti
NordEst_ts <- cbind(NordEst_ts, movimenti = mov_ts$NordEst)
# Faccio la regressione solo sui movimenti
MovModel_NE <- lm(turismo ~ movimenti, data = NordEst_ts)
# Solito summary
sum_MovModel_NE <- summary(MovModel_NE)
# L'R quadro è molto basso e la variabile è poco significativa
# Provo a vedere cosa esce da un plot
plot_MovModel_NE <- ggplot(NordEst_ts, aes(x=movimenti, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(MovModel_NE)), color="red") +
  xlab("Movimenti") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Si vede che i dati dei movimenti non sono buoni come quelli dei passeggeri
# Plotto i residui
plot_MovModel_NE_resid <- ggplot(NordEst_ts, aes(x = movimenti, y = resid(MovModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Movimenti", y = "Residui") + 
  theme_classic()
# I residui fanno visibilmente schifo
# Stampo la variabile per capire cosa sto studiando
plot_mov <-  ggplot(mov_ts, aes(x = anno, y= NordEst, color = "NE")) +
  geom_line(size = 1) +
  geom_line(aes(y = NordOvest, color = "NO"), size = 1) +
  geom_line(aes(y = Centro, color = "CE"), size = 1) +
  geom_line(aes(y = Sud, color = "SU"), size = 1) +
  geom_line(aes(y = Isole, color = "IS"), size = 1) +
  xlab("Anni") +
  ylab("Valori") +
  ggtitle("Numero di movimenti aerei") +
  scale_color_manual(name = "Regione",
                     values = c("NE" = "blue", "NO" = "green", "CE" = "purple", "SU" = "red", "IS" = "orange"),
                     labels = c("Nord Est", "Nord Ovest", "Centro", "Sud", "Isole")) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  geom_text(aes(x = 2020 , y=5*10^6, label = "Covid"), color = 'black', size = 4) +
  geom_segment(x = 2020, xend = 2020, y=0 , yend = 5*10^6, linetype = "dotted", color = "black", size = 1)
# Boh da capire perché tanta differenza con il modello con i passeggeri

# Verifico che il modello combinato con il tempo magari funziona
MovTimeModel_NE <- lm(turismo ~ anno + movimenti, data = NordEst_ts)
# Summary
sum_MovTimeModel_NE <- summary(MovTimeModel_NE)
# Stesso problema dei passeggeri, provo a togliere l'intercetta per le variabili poco significative
MovTimeModel_NE <- lm(turismo ~ anno + movimenti -1, data = NordEst_ts)
# Summary
sum_MovTimeModel_NE <- summary(MovTimeModel_NE)
# I movimenti restano poco significativi e con un coefficiente molto basso, 
# ma l'R quadro è molto alto (0.99)
# Provo a stampare per vederci più chiaro
plot_MovTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(MovTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Effettivamente il grafico non sembra niente male
# Proviamo con i residui
plot_MovTimeModel_NE_resid1 <- ggplot(NordEst_ts, aes(x = anno, y = resid(MovTimeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Anno", y = "Residui") + 
  theme_classic()
plot_MovTimeModel_NE_resid2 <- ggplot(NordEst_ts, aes(x = movimenti, y = resid(PasTimeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Movimenti", y = "Residui") + 
  theme_classic()
# Posso ritenermi piuttosto soddisfatto

# I modelli con le variabili combinate sembrano riuscire a spiegare meglio i valori anomali nel tempo
# Secondo me è arrivato il momento di provare ad aggiungere il 2020 al database
# Sicuramente la variabile temporale perderà significatività
NordEst_ts <- rbind(NordEst_ts, c(2020, Tur_ts[21,2],pas_ts[21,2], mov_ts[21,2]))
# Proviamo a ri applicare le regressioni combinate
PasTimeModel_NE <-lm(turismo ~ anno + passeggeri -1, data = NordEst_ts)
MovTimeModel_NE <- lm(turismo ~ anno + movimenti -1, data = NordEst_ts)
# Vediamo i summary e vediamo se sono peggiorati
sum_PasTimeModel_NE <- summary(PasTimeModel_NE)
sum_MovTimeModel_NE <- summary(MovTimeModel_NE)
# lievi differenze nell'R quadro, il modello con i movimenti sembra continuare a spiegare benissimo il fenomeno
# I coefficienti continuano ad essere esigui
# Proviamo un plot
plot_PasTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PasTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
plot_MovTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(MovTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Spiegano i dati incredibilmente bene
# Il modello con i passeggeri ha un R quadro aggiustato sensibilmente più basso
# Graficamente sembra evidente che i dati sono descritti meglio dal modello con i passeggeri però
# Confronto dei residui
# Residui passeggeri:
plot_PasTimeModel_NE_resid1 <- ggplot(NordEst_ts, aes(x = anno, y = resid(PasTimeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Passeggeri", y = "Residui") + 
  theme_classic()
plot_PasTimeModel_NE_resid2 <- ggplot(NordEst_ts, aes(x = passeggeri, y = resid(PasTimeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Passeggeri", y = "Residui") + 
  theme_classic()
# Residui movimenti
plot_MovTimeModel_NE_resid1 <- ggplot(NordEst_ts, aes(x = anno, y = resid(MovTimeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Anno", y = "Residui") + 
  theme_classic()
plot_MovTimeModel_NE_resid2 <- ggplot(NordEst_ts, aes(x = movimenti, y = resid(PasTimeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Movimenti", y = "Residui") + 
  theme_classic()
# I modelli sembrano entrambi non di immediata interpretazione ma buoni
# A questo punto direi che c'è una base da cui partire

# Quel momento è arrivato: provo ad aggiungere una variabile al modello
# La variabile prescelta è il PIL
# Sappiamo il Pil essere correlato al turismo, ed avere una correlazione con "direzione"
# Infatti il valore aggiunto del turismo accresce il PIL in modo diretto
# Tuttavia includo il Pil come variabile esplicativa poiché è spiegato, mediamente, per l'1% circa dal turismo
# Pertanto sfrutto il Pil come indicatore di ricchezza del territorio
# Visualizziamo il PIL delle 5 macro aree come prima cosa:
plot_pil <-  ggplot(Pil_ts, aes(x = anno, y= NordEst, color = "NE")) +
  geom_line(size = 1) +
  geom_line(aes(y = NordOvest, color = "NO"), size = 1) +
  geom_line(aes(y = Centro, color = "CE"), size = 1) +
  geom_line(aes(y = Sud, color = "SU"), size = 1) +
  geom_line(aes(y = Isole, color = "IS"), size = 1) +
  xlab("Anni") +
  ylab("PIL") +
  ggtitle("Prodotto Interno Lordo") +
  scale_color_manual(name = "Regione",
                     values = c("NE" = "blue", "NO" = "green", "CE" = "purple", "SU" = "red", "IS" = "orange"),
                     labels = c("Nord Est", "Nord Ovest", "Centro", "Sud", "Isole")) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  geom_text(aes(x = 2020, y = 6.2*10^5 , label = "Covid"), color = 'black', size = 4) +
  geom_segment(x = 2020, xend = 2020, y = 0, yend = 6.2*10^5, linetype = "dotted", color = "black", size = 1)
# Il covid ha avuto un impatto istantaneo meno evidente sul PIL,
# Tuttavia la crescita costante spiega molto bene la crescita del turismo nel tempo
# Può avere senso aggiungere il PIL nel modello
# Prima provo la regressione semplice per vedere come si comporta
# come prima cosa lo aggiungo al dataframe
NordEst_ts <- cbind(NordEst_ts, pil = Pil_ts[,2])
# Ora regredisco
PilModel_NE <- lm(turismo ~ pil, data = NordEst_ts)
# Vediamo cos'è uscito con il solito ordine
# summary
sum_PilModel_NE <- summary(PilModel_NE)
# La regressione è terribile
# Il coefficiente è assolutamente non significativo e l'R quadro aggiustato negativo
# Proviamo a stampare per capirci qualcosa
plot_PilModel_NE <- ggplot(NordEst_ts, aes(x=pil, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PilModel_NE)), color="red") +
  xlab("PIL") + ylab("Tasso") +
  ggtitle("Prodotto Interno Lordo") +
  theme_classic()
# Residui
plot_PilModel_NE_resid <- ggplot(NordEst_ts, aes(x = pil, y = resid(PilModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "PIL", y = "Residui") + 
  theme_classic()
# In realtà i residui non sono così tragici se non fosse per un dato
# Quindi potrebbe essere buona con altre variabili come no

# Come prima cosa aggiungo al PIL il tempo
PilTimeModel_NE <-lm(turismo ~ anno + pil, data = NordEst_ts)
# Vediamo il summary
sum_PilTimeModel_NE <- summary(PilTimeModel_NE)
# Il summary è terribile, variabili poco significative e R quadro basso
# Provo a togliere l'intercetta
PilTimeModel_NE <-lm(turismo ~ anno + pil -1, data = NordEst_ts)
# Vediamo il summary
sum_PilTimeModel_NE <- summary(PilTimeModel_NE)
# Questo summary ha un R^2 grandioso ma il PIL non è per niente significativo nel modello
# Vediamolo giusto al volo graficamente con l'asse temporale
plot_PilTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PilTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# In effetti si vede che non incide quasi niente sul fit

# Provo a combinare il Pil con i Passeggeri
PilPasTimeModel_NE <-lm(turismo ~ anno + passeggeri + pil -1, data = NordEst_ts)
# Vediamo il summary
sum_PilPasTimeModel_NE <- summary(PilPasTimeModel_NE)
# L'R quadro è molto buono e tutto, ma il PIL continua ad essere non significativo
# Penso che a questo punto abbia senso levarlo
# Proviamo prima a stampare sempre sull'asse temporale
plot_PilPasTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PilPasTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Direi assolutamente bocciato, a tratti sembra peggiorare il modello senza PIL

# Provo a combinare il PIL con i movimenti, prima di scartarlo
PilMovTimeModel_NE <-lm(turismo ~ anno + movimenti + pil -1, data = NordEst_ts)
# Vediamo il summary
sum_PilMovTimeModel_NE <- summary(PilMovTimeModel_NE)
# Incredibile R quadro aggiustato, e in questo modello il PIL è significativo
# Proviamo prima a stampare sempre sull'asse temporale
plot_PilMovTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PilMovTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Sembra fittare meglio il trend iniziale, c'è sempre overfitting sull'ultimo dato

# Riassumendo, PIL bocciato nel modello con i passeggeri, promosso nel modello con i movimenti

# Aggiungo una nuova variabile: può essere interessante iniziare a parlare di spesa 
# Partiamo dalla spesa
# Ci sono più serie storiche di spesa da considerare: 
# La prima è la spesa totale effettuata in ogni regione
# Questa variabile è correlata al PIL, in qualche modello potremmo essere costretti a rinunciare a una delle due
# Aggiungo la variabile al database
NordEst_ts <- cbind(NordEst_ts, spesa = SpesaRegione_ts[,2])
# Ora faccio la solita regressione del tasso sulla sola variabile
SpeModel_NE <- lm(turismo ~ spesa, data = NordEst_ts)
# solito summary
sum_SpeModel_NE <- summary(SpeModel_NE)
# R quadro bruttino  e variabile nontanto significativa
# Stampo per vedere com'è
plot_SpeModel_NE <- ggplot(NordEst_ts, aes(x=spesa, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(SpeModel_NE)), color="red") +
  xlab("Spesa") + ylab("Tasso") +
  ggtitle("Tasso di Turisticità") +
  theme_classic()
# Si vede come questo modello non fitti bene il 2020, ma il resti sì abbastanza bene
# Residui
plot_SpeModel_NE_resid <- ggplot(NordEst_ts, aes(x = spesa, y = resid(SpeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "PIL", y = "Residui") + 
  theme_classic()
# Non malissimo
# Meglio del PIL come base di partenza
# Stampo la variabile per vederci un po' meglio
plot_spe <- ggplot(SpesaRegione_ts, aes(x = anno, y= NordEst, color = "NE")) +
  geom_line(size = 1) +
  geom_line(aes(y = NordOvest, color = "NO"), size = 1) +
  geom_line(aes(y = Centro, color = "CE"), size = 1) +
  geom_line(aes(y = Sud, color = "SU"), size = 1) +
  geom_line(aes(y = Isole, color = "IS"), size = 1) +
  xlab("Anni") +
  ylab("Spesa") +
  ggtitle("Spesa per macro - aree geografiche") +
  scale_color_manual(name = "Regione",
                     values = c("NE" = "blue", "NO" = "green", "CE" = "purple", "SU" = "red", "IS" = "orange"),
                     labels = c("Nord Est", "Nord Ovest", "Centro", "Sud", "Isole")) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  geom_text(aes(x = 2020, y = 2200 , label = "Covid"), color = 'black', size = 4) +
  geom_segment(x = 2020, xend = 2020, y = 0, yend = 2200, linetype = "dotted", color = "black", size = 1)
# Si vedono grosse differenze tra nord e sud per questa variabile
# Sia per il comportamento della variabile stessa, sia per il valore in termini assoluti

# Adesso si confronta la variabile spesa con la variabile PIL nei vari modelli
# Primo solito modello spesa e tempo
SpeTimeModel_NE <- lm(turismo ~ anno + spesa, data = NordEst_ts)
# Adesso vediamo il summary
sum_SpeTimeModel_NE <- summary(SpeTimeModel_NE)
# Modello terribile, come sempre tolgo l'intercetta e vediamo se migliora
SpeTimeModel_NE <- lm(turismo ~ anno + spesa -1, data = NordEst_ts)
# Summary
sum_SpeTimeModel_NE <- summary(SpeTimeModel_NE)
# Comunque i coefficienti continuano ad essere non eccessivamente significativi
# Vediamo graficamente il modello sull'asse temporale
plot_SpeTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(SpeTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# In verità il fit non sembra malissimo, ma ha evidente bisogno di essere aggiustato

# Vediamo come funziona combinato con movimenti e passeggeri
SpePasTimeModel_NE <-lm(turismo ~ anno + passeggeri + spesa -1, data = NordEst_ts)
# Vediamo il summary
sum_SpePasTimeModel_NE <- summary(SpePasTimeModel_NE)
# L'R quadro è molto buono e tutto, ma le variabili non sono per niente significative
# Se metto l'intercetta il modello peggiora
# Proviamo prima a stampare sempre sull'asse temporale
plot_SpePasTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(SpePasTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Strano non sembra così male il fit
# Tuttavia i dati parlano, con i passseggeri non includiamo variabili economiche
# Mi riservo per dopo di valutare le componenti della spesa

# Valuto se, analogamente al pil, la spesa funzioni meglio con i movimenti
SpeMovTimeModel_NE <-lm(turismo ~ anno + movimenti + spesa -1, data = NordEst_ts)
# Vediamo il summary
sum_SpeMovTimeModel_NE <- summary(SpeMovTimeModel_NE)
# Incredibilmente questo modello funziona molto bene
# Proviamo prima a stampare sempre sull'asse temporale
plot_SpeMovTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(SpeMovTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# In realtà non sembra molto buono, tutti i modelli con i movimenti tendono a overfittare l'ultimo dato
# Guadagnano molto in R^2 ma perdono molto in stima purtroppo
# Graficamente, a tratti la stima sembra migliore, a tratti peggiore
# Rispetto al modello uguale ma con il PIL
# Per ora quello che mi piace di più è tempo + passeggeri

# Entra in gioco una nuova variabile: la popolazione
# La popolazione sarà molto incisiva, poiché il tasso dipende matematicamente da questa
# Mi aspetto sia significativa con il numero di passeggeri e mi aspetto possa migliorare il modello
# Tuttavia mi aspetto anche che il suo ruolo sia di interesse maggiore per il modello longitudinale
# Aggiungo la variabile al database di NordEst
NordEst_ts <- cbind(NordEst_ts, popolazione = Popolazione_ts[,2])
# Fatto
# Ora stampo le time series della popolazione e vedo che succede
plot_pop <- ggplot(Popolazione_ts, aes(x = anno, y= NordEst, color = "NE")) +
  geom_line(size = 1) +
  geom_line(aes(y = NordOvest, color = "NO"), size = 1) +
  geom_line(aes(y = Centro, color = "CE"), size = 1) +
  geom_line(aes(y = Sud, color = "SU"), size = 1) +
  geom_line(aes(y = Isole, color = "IS"), size = 1) +
  xlab("Anni") +
  ylab("Persone") +
  ggtitle("Popolazione") +
  scale_color_manual(name = "Regione",
                     values = c("NE" = "blue", "NO" = "green", "CE" = "purple", "SU" = "red", "IS" = "orange"),
                     labels = c("Nord Est", "Nord Ovest", "Centro", "Sud", "Isole")) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  geom_text(aes(x = 2020, y = 1.7*10^7 , label = "Covid 19"), color = 'black', size = 4) +
  geom_segment(x = 2020, xend = 2020, y = 0, yend = 1.7*10^7, linetype = "dotted", color = "black", size = 1)
# La vera differenza tra questa variabile e le altre è che la popolazione non ha risentito del covid
# Non significativamente
# Può essere una cosa buona, soprattutto per i modelli che overfittano sul 2020
# Proviamo la solita regressione lineare

# Regressione del tasso sulla sola popolazione 
PopModel_NE <- lm(turismo ~ popolazione, data = NordEst_ts)
# Vediamo come sempre il summary
sum_PopModel_NE <- summary(PopModel_NE)
# è il modello peggiore che io abbia mai fatto
# Plot solo perché lo faccio sempre
plot_PopModel_NE <- ggplot(NordEst_ts, aes(x=popolazione, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PopModel_NE)), color="red") +
  xlab("Popolazione") + ylab("Tasso") +
  ggtitle("Tasso di Turisticità") +
  theme_classic()
# Assurda anche la distribuzione della popolazione rispetto al tasso
# Non può funzionare

# Vedo se la popolazione affiancata alla time series può funzionare
PopTimeModel_NE <- lm(turismo ~ anno + popolazione, data = NordEst_ts)
# Vediamo il summary
sum_PopTimeModel_NE <- summary(PopTimeModel_NE)
# Terrificante sotto tutti gli aspetti
# Provo a togliere l'intercetta, senza prospettive di miglioramento
# Anche perché l'intercetta è non significativa
PopTimeModel_NE <- lm(turismo ~ anno + popolazione -1, data = NordEst_ts)
# Vediamo questo summary
sum_PopTimeModel_NE <- summary(PopTimeModel_NE)
# Continua ad essere terribile, anche se inspiegabilmente l'R quadro è alto
# I coefficienti non sono significativi
# Proviamo a plottare
plot_PopTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PopTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Il modello è terribile, sembra cercare di correggere senza riuscirci il trend lineare sul tempo
# Bocciatissimo

# Provo a combinare la popolazione nei modelli migliori
# Modello MovTime 
PopMovTimeModel_NE <-lm(turismo ~ anno + movimenti + popolazione -1, data = NordEst_ts)
# Vediamo il summary
sum_PopMovTimeModel_NE <- summary(PopMovTimeModel_NE)
# inserendo la popolazione, perdono di significatività sia popolazione sia tempo
# male male
# Provo un plot
plot_PopMovTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PopMovTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Praticamente identico a prima
# Modello da scartare

# Provo ad aggiungere la popolazione e la spesa contemporaneamente
# Forse potrebbe essere necessario rimuovere la variabile tempo
PopSpeMovTimeModel_NE <-lm(turismo ~ anno + movimenti + spesa + popolazione -1, data = NordEst_ts)
# Vediamo il summary
sum_PopSpeMovTimeModel_NE <- summary(PopSpeMovTimeModel_NE)
# E infatti anno e popolazione sono entrambi non significativi
# Proviamo prima a stampare sempre sull'asse temporale
plot_PopSpeMovTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PopSpeMovTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Non sembra essere per niente migliorato

# Provo per la prima volta a togliere la componente temporale
# Il modello perderà molto in interpretabilità
PopSpeMovModel_NE <-lm(turismo ~  movimenti + spesa + popolazione -1, data = NordEst_ts)
# Vediamo il summary
sum_PopSpeMovModel_NE <- summary(PopSpeMovModel_NE)
# Incredibilmente la spesa perde tantissima significatività
# Proviamo prima a stampare sempre sull'asse temporale
plot_PopSpeMovModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PopSpeMovModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Il modello non è comunque soddisfacente

# Provo a rimuovere anche la spesa per vedere che succede
PopMovModel_NE <-lm(turismo ~  movimenti + popolazione -1, data = NordEst_ts)
# Vediamo il summary
sum_PopMovModel_NE <- summary(PopMovModel_NE)
# Assurdo le due variabili sono molto significative
# Proviamo prima a stampare sempre sull'asse temporale
plot_PopMovModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PopMovModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()
# Okay in realtà è sufficiente vedere il grafico nel tempo per rendersi conto che è uguale a MovTime
# Scarto la variabile popolazione per i modelli con i movimenti aerei

# Proviamo la variabile popolazione nel modello con i passeggeri
PopPasTimeModel_NE <-lm(turismo ~ anno + log(passeggeri) + popolazione -1, data = NordEst_ts)
# Vediamo il summary
sum_PopPasTimeModel_NE <- summary(PopPasTimeModel_NE)
# la popolazione non è significativa in questo modello
# Provo un plot ma sono portato a scartarla
plot_PopPasTimeModel_NE <- ggplot(NordEst_ts, aes(x=anno, y=turismo)) +
  geom_point(color="blue") +
  geom_line(aes(y=fitted(PopPasTimeModel_NE)), color="red") +
  xlab("Anno") + ylab("Tasso") +
  ggtitle("Tasso di turisticità nel Nord-Est") +
  theme_classic()

# plot della popolazione
plot_pop

# Purtroppo gli altri dati sono incompleti
# Più avanti vedo come gestirli

# Per ora il modello migliore risulta essere il modello PasTimeModel
# Vediamo come si comporta nello specifico
# sum_PasTimeModel_NE
# plot_PasTimeModel_NE
# plot_PasTimeModel_NE_resid1
# plot_PasTimeModel_NE_resid2
# Okay non sembra malissimo
# Provo a stampare i residui congiuntamente
plot_PasTimeModel_NE_resid <- ggplot(NordEst_ts, aes(x = fitted(PasTimeModel_NE), y = resid(PasTimeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Valori fittati", y = "Residui") + 
  theme_classic()
# Provo a plottare il modello 3d
# Ci proverò più avanti, per ora scarsi risultati

# C'è stato un errore nei dati, vanno rifatte delle valutazioni

# vediamo i residui del modello della popolazione

plot_PopPasTimeModel_NE_resid <- ggplot(NordEst_ts, aes(x = fitted(PopPasTimeModel_NE), y = resid(PopPasTimeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Valori fittati", y = "Residui") + 
  theme_classic()
histplot_PopPasTimeModel_resid <- ggplot(data.frame(resid = resid(PopPasTimeModel_NE)), aes(x = resid)) + 
  geom_histogram(aes(y = ..density..), fill = "blue", alpha = 0.5, bins = 10) +
  geom_density(color = "red") +
  ggtitle("Istogramma dei residui") +
  xlab("Studentized Residuals") +
  ylab("Density") + 
  theme_classic() 
qqplot_PopPasTimeModel_resid <- ggplot(data.frame(resid = resid(EasiestModel_NE)), aes(sample = resid)) + 
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("Q-Q Plot dei Residui") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_classic() 

plot_PopPasTimeModel_NE_resid1 <- ggplot(NordEst_ts, aes(x = anno, y = resid(PopPasTimeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Anno", y = "Residui") + 
  theme_classic()
plot_PopPasTimeModel_NE_resid2 <- ggplot(NordEst_ts, aes(x = passeggeri, y = resid(PopPasTimeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Passeggeri", y = "Residui") + 
  theme_classic()
plot_PopPasTimeModel_NE_resid3 <- ggplot(NordEst_ts, aes(x = popolazione, y = resid(PopPasTimeModel_NE))) + 
  geom_point() + 
  ggtitle("Residui") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Popolazione", y = "Residui") + 
  theme_classic()
