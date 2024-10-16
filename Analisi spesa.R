# ANALISI SPESA IN ARTICOLI SPORTIVI #
library(readxl)

# Caricamento dei dati ----

spesa_2010 <- read_excel("Dati/Spesa/Spesa in articoli sportivi 2010.xlsx")
spesa_2015 <- read_excel("Dati/Spesa/Spesa in articoli sportivi 2015.xlsx")
summary(spesa_2010)
summary(spesa_2015)

# Analisi ----

# I dati come unità di misura utilizzano lo Standard di potere d'acquisto di Eurostat

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