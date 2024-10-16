# ANALISI DEI DATI PER LA TESI #

library(readxl)
library(olsrr)

# Pratica ----

## Caricamento dei dati ----

pratica_regione <- read_excel("Dati/Pratica/Praticanti sport per regione 2020.xlsx")
pratica_professione <- read_excel("Dati/Pratica/Praticanti sportivi per condizione professionale 2020.xlsx")
pratica_età <- read_excel("Dati/Pratica/Praticanti sportivi per età 2020.xlsx")
pratica_studio <- read_excel("Dati/Pratica/Praticanti sport per titolo di studio 2020.xlsx")
pratica_totale <- read_excel("Dati/Pratica/Praticanti sportivi totali.xlsx")
summary(pratica_totale)

## Analisi ----
## 
### Serie storica ----
# Serie storica dei totali di ogni anno con media mobile e magari modello di regressione lineare multipla 
x1 <- pratica_totale$`Praticano sport in modo continuativo`
x2 <- pratica_totale$`Praticano sport in modo saltuario`
x3 <- pratica_totale$`Praticano sport solo qualche attività fisica`
x4 <- pratica_totale$`Non praticano sport, né attività fisica`
anno <- pratica_totale$Anno

summary(pratica_totale$`Praticano sport in modo continuativo`)
summary(pratica_totale$`Praticano sport in modo saltuario`)
summary(pratica_totale$`Praticano sport solo qualche attività fisica`)
summary(pratica_totale$`Non praticano sport, né attività fisica`)

plot(anno, x1, col = 2, lwd = 3, pch = 19, bty = 'l', type = "b", xlab = "Anno", 
     ylab = "Persone", main = "Praticano sport in modo continuativo")
plot(anno, x2, col = 3, lwd = 3, pch = 19, bty = 'l', type = "b", xlab = "Anno", 
     ylab = "Persone", main = "Praticano sport in modo saltuario")
plot(anno, x3, col = 4, lwd = 3, pch = 19, bty = 'l', type = "b", xlab = "Anno", 
     ylab = "Persone", main = "Praticano solo qualche attività fisica")
plot(anno, x4, col = 6, lwd = 3, pch = 19, bty = 'l', type = "b", xlab = "Anno", 
     ylab = "Persone", main = "Non praticano sport, né attività fisica")

pratica_totale$`Non praticano sport, né attività fisica`[1]
pratica_totale$`Non praticano sport, né attività fisica`[19]
crescita <- ((x4[19] - x4[1]) / x4[1]) * 100

# Spesa ----

## Caricamento dei dati ----

spesa_2010 <- read_excel("Dati/Spesa/Spesa in articoli sportivi 2010.xlsx")
spesa_2015 <- read_excel("Dati/Spesa/Spesa in articoli sportivi 2015.xlsx")
summary(spesa_2010)
summary(spesa_2015)

## Analisi ----

## I dati come unità di misura utilizzano lo Standard di potere d'acquisto di Eurostat

## Europa ----
### Spesa e crescita totale ----
spesa_tot_eu_2010 <- sum(spesa_2010$`Articoli e servizi sportivi`) + sum(spesa_2010$`Principali beni durevoli per ricreazione all'aperto`) + 
  sum(spesa_2010$`Principali beni durevoli per ricreazione al chiuso`) + sum(spesa_2010$`Equipaggiamento per sport, campeggio e attivita all'aria aperta`)
# Il valore per la spesa totale in articoli sportivi nel 2010 in Europa è 9513.4
spesa_tot_eu_2015 <- sum(spesa_2015$`Articoli e servizi sportivi`) + sum(spesa_2015$`Principali beni durevoli per ricreazione all'aperto`) + 
  sum(spesa_2015$`Principali beni durevoli per ricreazione al chiuso`) + sum(spesa_2015$`Equipaggiamento per sport, campeggio e attivita all'aria aperta`)
# Il valore per la spesa totale in articoli sportivi nel 2010 in Europa è 19099.3
# Il valore per la spesa in articoli e servizi sportivi è aumentata dal 2010 al 2015 con una crescita del 100,76% quindi il valore è più che raddoppiato
diff_spesa_art_sport_eu <- spesa_tot_eu_2015 - spesa_tot_eu_2010
crescita_spesa_art_sport_eu <- (diff_spesa_art_sport_eu / spesa_tot_eu_2010) * 100

spesa_tot_eu_2010
spesa_tot_eu_2015
diff_spesa_art_sport_eu
crescita_spesa_art_sport_eu

### Composizione spesa europea ----

europa_2010 <- c(sum(spesa_2010$`Articoli e servizi sportivi`), 
                 sum(spesa_2010$`Principali beni durevoli per ricreazione all'aperto`), 
                 sum(spesa_2010$`Principali beni durevoli per ricreazione al chiuso`),
                 sum(spesa_2010$`Equipaggiamento per sport, campeggio e attivita all'aria aperta`))
europa_2015 <- c(sum(spesa_2015$`Articoli e servizi sportivi`), 
                 sum(spesa_2015$`Principali beni durevoli per ricreazione all'aperto`), 
                 sum(spesa_2015$`Principali beni durevoli per ricreazione al chiuso`),
                 sum(spesa_2015$`Equipaggiamento per sport, campeggio e attivita all'aria aperta`))

titoli <- c("Articoli e servizi sportivi", 
            "Principali beni durevoli per ricreazione all'aperto", 
            "Principali beni durevoli per ricreazione al chiuso", 
            "Equipaggiamento per sport, campeggio e attivita all'aria aperta")

percentuali_eu_2010 <- round((europa_2010/sum(europa_2010)*100),1)
percentuali_eu_2015 <- round((europa_2015/sum(europa_2015)*100),1)
titolo_eu_2010 <- paste(percentuali_eu_2010, "%", sep = " ")
titolo_eu_2015 <- paste(percentuali_eu_2015, "%", sep = " ")
pie(t(europa_2010), col = c(4, 3, 5, 6), labels = titolo_eu_2010, 
    main = "Composizione della spesa delle famiglie europee in articoli sportivi nel 2010", 
    radius = 0.57, init.angle = 240)
legend(-2.1, -0.6, cex = 0.85, legend = titoli, fill = c(4, 3, 5, 6))


pie(t(europa_2015), col = c(4, 3, 5, 6), labels = titolo_eu_2015,
    main = "Composizione della spesa delle famiglie europee in articoli sportivi nel 2015",
    radius = 0.57, init.angle = 270)
legend(-2.1, -0.6, cex = 0.85, legend = titoli, fill = c(4, 3, 5, 6))

## Italia ----
### Spesa e crescita totale ----
spesa_tot_ita_2010 <- spesa_2010$`Articoli e servizi sportivi`[12] + spesa_2010$`Principali beni durevoli per ricreazione all'aperto`[12] +
  spesa_2010$`Principali beni durevoli per ricreazione al chiuso`[12] + spesa_2010$`Equipaggiamento per sport, campeggio e attivita all'aria aperta`[12]
spesa_tot_ita_2015 <- spesa_2015$`Articoli e servizi sportivi`[12] + spesa_2015$`Principali beni durevoli per ricreazione all'aperto`[12] +
  spesa_2015$`Principali beni durevoli per ricreazione al chiuso`[12] + spesa_2015$`Equipaggiamento per sport, campeggio e attivita all'aria aperta`[12]


diff_spesa_tot_ita <- spesa_tot_ita_2015 - spesa_tot_ita_2010
crescita_spesa_tot_ita <- (diff_spesa_tot_ita / spesa_tot_ita_2010) * 100
# In Italia nel 2010 il valore per la spesa in articoli sportivi è 277.2 mentre nel 2015 è 540.7 
# anche in questo caso il valore per la spesa è aumentato con una crescita del 95,06%
# La crescita in Italia è minore rispetto al valore europeo.

spesa_tot_ita_2010 
spesa_tot_ita_2015
diff_spesa_tot_ita
crescita_spesa_tot_ita

### Composizione spesa italiana ----
italia_2010 <- spesa_2010[12, 2:5]
italia_2015 <- spesa_2015[12, 2:5]
titoli <- c("Articoli e servizi sportivi", 
            "Principali beni durevoli per ricreazione all'aperto", 
            "Principali beni durevoli per ricreazione al chiuso", 
            "Equipaggiamento per sport, campeggio e attivita all'aria aperta")

percentuali_ita_2010 <- round((italia_2010/sum(italia_2010)*100),1)
percentuali_ita_2015 <- round((italia_2015/sum(italia_2015)*100),1)
titolo_ita_2010 <- paste(percentuali_ita_2010, "%", sep = " ")
titolo_ita_2015 <- paste(percentuali_ita_2015, "%", sep = " ")
pie(t(italia_2010), col = c(4, 3, 5, 6), labels = titolo_ita_2010, 
    main = "Composizione della spesa delle famiglie italiane in articoli sportivi nel 2010", radius = 0.6, 
    init.angle = 270)
legend(0.2, 1.0, cex = 0.85, legend = titoli, fill = c(4, 3, 5, 6))


pie(t(italia_2015), col = c(4, 3, 5, 6), labels = titolo_ita_2015,
    main = "Composizione della spesa delle famiglie italiane in articoli sportivi nel 2015",
    radius = 0.6, init.angle = 270)
legend(0.2, 1.0, cex = 0.85, legend = titoli, fill = c(4, 3, 5, 6))

italia_2010 <- c(italia_2010[,1], italia_2010[,2], italia_2010[,3], italia_2010[,4])
italia_2015 <- c(italia_2015[,1], italia_2015[,2], italia_2015[,3], italia_2015[,4])
italia_2010
italia_2015

# Strutture ----

## Caricamento dei dati ----

strutture_2021 <- read_excel("Dati/Strutture/Strutture sportive 2021.xlsx")
popolazione_2021 <- read_excel("Dati/Strutture/Popolazione regioni 2021.xlsx")
summary(strutture_2021)
# Il numero totale delle strutture sportive in Italia nel 2021 è 6445
# Nel 2021 ci sono una media di 322 strutture sportive per ogni regione
# In ogni regione ci sono almeno 9 strutture sportive e massimo 1271
strutture_2021
# La regione con meno strutture sportive è il Molise mentre la regione con più strutture sportive è la Lombardia
# è interessante però che la regione con meno strutture sportive non è la regione con meno residenti 
# ovvero la valle d'aosta che ha ben 103 strutture sportive rispetto alle solo 9 del Molise

dati <- data.frame(strutture_2021, popolazione_2021[,2])
summary(dati)
dati


regione <- dati$Regione
strutture_sportive <-  strutture_2021$`Strutture sportive`
popolazione <- dati$Popolazione

## Analisi ----

x <- dati$Strutture.sportive # Strutture sportive
y <- dati$Popolazione        # Popolazione

## Modello di regressione lineare ----

mod <- lm(y ~ x)
summary(mod)
# Rifiuto l'ipotesi nulla con un p-value molto piccolo
# Il numero di persone residenti in una regione spiega il 47% della variabilità delle strutture sportive
# Le strutture sportive aumentano di uno ogni 5486 residenti in una regione
plot(x, y, xlab = 'Strutture sportive', ylab = 'Popolazione residente nella regione', col = 4, lwd = 3, 
     main = "Analisi della popolazione e delle strutture sportive", pch = 19, bty = "l")
abline(v = mean(x), lty = 2, lwd = 3, col = 6)
abline(h = mean(y), lty = 2, lwd = 3, col = 6)
abline(col = 2, lwd = 4, reg = mod )
lines(lowess(x,y), col = 3, lwd = 4)
titolo <- c('Regressione lineare', 'Media mobile', 'Media regionale')
legend("bottomright", legend = titolo , fill = (col = c(2, 3, 6)))

## Controllo delle ipotesi classiche ----

## Controllo dell'ipotesi di normalità distributiva dei residui ----

# controllo grafico
ols_plot_resid_qq(mod)
# siamo soddisfatti quando i punti sono sulla linea rossa
ols_plot_resid_hist(mod) # istogramma dei residui

# test di ipotesi
ols_test_normality(mod)
# accettiamo se il pvalue è alto
# l'ipotesi di normalità distributiva è verificata

### Leverage, outliers e osservazioni influenti ----

# ricerca di eventuali outliers o punnti di leva
ols_plot_resid_lev(mod)






## Inferenza ----


# Ipotesi da testare: la relazione che lega la popolazione (x) 
# al numero delle strutture sportive (y) è significativa?

# Calcolo delle stime dei minimi quadrati di
# beta_0 e beta_1
x_bar <- mean(x)
y_bar <- mean(y)

cov_xy <- sum((x - x_bar) * (y - y_bar))
var_x <- sum((x - x_bar)^2)

# stima dei minimi quadrati di beta_1
b_1_hat <- cov_xy / var_x

# stima dei minimi quadrati di beta_0
b_0_hat <- y_bar - b_1_hat * x_bar

# Prima di tutto raccogliamo la stima di sigma_2
# Calcolo dei residui
n <- length(y)
y_hat <- (b_0_hat + b_1_hat * x)
e_hat <- y - y_hat
s_2 <- sum(e_hat ^ 2) / (n - 2)
plot(e_hat, y_hat)
# Valore osservato
t_oss <- b_1_hat / sqrt(s_2 / var_x)
pnorm(t_oss)
# Fissiamo il livello di significatività (alpha)
alpha <- 0.05

# Calcoliamo il valore critico
valore_critico <- qt(1- alpha/2, n - 2)

# decisione tra H_0 e H_1
abs(t_oss) > valore_critico
# Essendo il valore osservato maggiore del valore critico rifiuto l'ipotesi nulla



## Altro ----

strutture_sportive_tot <-  sum(strutture_sportive) 
strutture_sportive_tot
popolazione_tot <- sum(popolazione)
popolazione_tot
strutture_sportive_bar <- mean(strutture_sportive) 
# Nel 2021 ci sono una media di 322 strutture sportive per ogni regione