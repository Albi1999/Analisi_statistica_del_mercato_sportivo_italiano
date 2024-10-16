# ANALISI PRATICA SPORTIVA #

library(readxl)

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





# Analizzare le diverse componenti della pratica sportiva
### Regione ----
summary(pratica_regione[,2:5])
pratica_regione_tot <- c(sum(pratica_regione$`In modo continuativo`), 
                         sum(pratica_regione$`In modo saltuario`),
                         sum(pratica_regione$`Solo qualche attività fisica`),
                         sum(pratica_regione$`Non praticano sport`))

### Età ----
summary(pratica_età[,2:5])
pratica_età_tot <- c(sum(pratica_età$`In modo continuativo`), 
                         sum(pratica_età$`In modo saltuario`),
                         sum(pratica_età$`Solo qualche attività fisica`),
                         sum(pratica_età$`Non praticano sport`))

### Condizione professionale ----
summary(pratica_professione[,2:5])
pratica_professione_tot <- c(sum(pratica_professione$`In modo continuativo`), 
                         sum(pratica_professione$`In modo saltuario`),
                         sum(pratica_professione$`Solo qualche attività fisica`),
                         sum(pratica_professione$`Non praticano sport`))

### Titolo di studio ----
summary(pratica_studio[,2:5])
pratica_studio_tot <- c(sum(pratica_studio$`In modo continuativo`), 
                         sum(pratica_studio$`In modo saltuario`),
                         sum(pratica_studio$`Solo qualche attività fisica`),
                         sum(pratica_studio$`Non praticano sport`))

pratica_tot_2020 <- c(pratica_totale[19,1:4])
pratica_tot_2020



