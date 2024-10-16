# ANALISI DELLE STRUTTURE SPORTIVE ITALIANE #

library(readxl)
library(olsrr)

# Caricamento dei dati ----

strutture_2021 <- read_excel("Dati/Strutture/Strutture sportive 2021.xlsx")
popolazione_2020 <- read_excel("Dati/Strutture/Popolazione 2020.xlsx")
pratica_2020 <- read_excel("Dati/Strutture/Pratica sportiva 2020.xlsx")
stato_salute_2020 <- read_excel("Dati/Strutture/Salute 2020.xlsx")
povertà_2020 <- read_excel("Dati/Strutture/Povertà 2020.xlsx")
pil_reddito_2020 <- read_excel("Dati/Strutture/PIL e Reddito Valori pro capite 2020.xlsx")

strutture <- strutture_2021[,2]

media <- sum(strutture)/20
media_no_lomb <- (sum(strutture) - strutture[9,1])/20
mean(strutture)








summary(strutture_2021)
# Il numero totale delle strutture sportive in Italia nel 2021 è 6445
# Nel 2021 ci sono una media di 322 strutture sportive per ogni regione
# In ogni regione ci sono almeno 9 strutture sportive e massimo 1271
strutture_2021
# La regione con meno strutture sportive è il Molise mentre la regione con più strutture sportive è la Lombardia
# è interessante però che la regione con meno strutture sportive non è la regione con meno residenti 
# ovvero la valle d'aosta che ha ben 103 strutture sportive rispetto alle solo 9 del Molise

dati <- data.frame(strutture_2021, popolazione_2020[,2], pratica_2020[,2], stato_salute_2020[, 2], povertà_2020[,2], pil_reddito_2020[,2:3])
summary(dati)
dati

# Analisi ----

y <- dati$Strutture.sportive

## Modello di regressione lineare ----

mod_completo <- lm(y ~ ., data = dati[,3:8])
summary(mod_completo)
mod_nullo <- lm(y ~ 1, data = dati)

## Modello di regressione lineare olsrr ----

# Backward
backward <- ols_step_backward_p(mod_completo, prem = 0.05)
summary(backward$model)
mod_backward <- backward$model

# Stepwise
stepwise <- ols_step_both_p(mod_completo, prem = 0.05, pent = 0.05)
summary(stepwise$model)
mod_stepwise <- stepwise$model

# Forward
forward <- ols_step_forward_p(mod_completo, penter = 0.05)
summary(forward$model)
mod_forward <- forward$model

## Analisi dei residui ----

# Linearità
ols_plot_resid_fit(mod_forward)

ols_plot_resid_fit(mod_backward)

# normalità distributiva dei residui

# controllo grafico
ols_plot_resid_qq(mod_forward)

ols_plot_resid_qq(mod_backward)
# siamo soddisfatti quando i punti sono sulla linea rossa

# istogramma dei residui
ols_plot_resid_hist(mod_forward)

ols_plot_resid_hist(mod_backward)

# test di ipotesi
ols_test_normality(mod_forward)

ols_test_normality(mod_backward)

plot(mod_forward)

# accettiamo se il pvalue è alto
# l'ipotesi di normalità distributiva è verificata

# Leverage, outliers e osservazioni influenti

# ricerca di eventuali outliers
# grafico dei residui standardizzati
ols_plot_resid_stand(mod_forward)

ols_plot_resid_stand(mod_backward)

# grafico dei residui studentizzati
ols_plot_resid_stud(mod_forward)

ols_plot_resid_stud(mod_backward)

# ricerca di eventuali punti di leva (leverage effect)
ols_plot_resid_lev(mod_forward) # in ascissa c'è hii

ols_plot_resid_lev(mod_backward)

# ricerca di eventuali osservazioni influenti

ols_plot_cooksd_chart(mod_forward)

ols_plot_cooksd_chart(mod_backward)

# le osservazioni 4 e 5 sono molto influenti

# grafico costruto da beta capello i con beta cappello
# per tutte le osservazioni di tutte le varie variabili
ols_plot_dfbetas(mod_forward)

ols_plot_dfbetas(mod_backward)







## Controllo delle ipotesi classiche ----

# Controllo dell'ipotesi di normalità distributiva dei residui

### Backward ----
# controllo grafico
ols_plot_resid_qq(step_back_olsrr$model)
# siamo soddisfatti quando i punti sono sulla linea rossa
ols_plot_resid_hist(step_back_olsrr$model) # istogramma dei residui

# test di ipotesi
ols_test_normality(step_back_olsrr$model)
# accettiamo se il pvalue è alto
# l'ipotesi di normalità distributiva è verificata

# Leverage, outliers e osservazioni influenti 

# ricerca di eventuali outliers o punnti di leva
ols_plot_resid_lev(step_back_olsrr$model)


### Foreward ----
# controllo grafico
ols_plot_resid_qq(step_forward_olsrr$model)
# siamo soddisfatti quando i punti sono sulla linea rossa
ols_plot_resid_hist(step_forward_olsrr$model) # istogramma dei residui

# test di ipotesi
ols_test_normality(step_forward_olsrr$model)
# accettiamo se il pvalue è alto
# l'ipotesi di normalità distributiva è verificata

### Leverage, outliers e osservazioni influenti ----

# ricerca di eventuali outliers o punnti di leva
ols_plot_resid_lev(step_forward_olsrr$model)























x <- dati$Popolazione
mod <- lm(y ~ x)
summary(mod)
# Rifiuto l'ipotesi nulla con un p-value molto piccolo
# Il numero di persone residenti in una regione spiega il 47% della variabilità delle strutture sportive
# Le strutture sportive aumentano di uno ogni 5486 residenti in una regione


plot(x, y, ylab = 'Strutture sportive', xlab = 'Popolazione residente nella regione', col = 4, lwd = 3, 
     main = "Analisi della popolazione e delle strutture sportive", pch = 19, bty = "l")
abline(v = mean(x), lty = 2, lwd = 3, col = 6)
abline(h = mean(y), lty = 2, lwd = 3, col = 6)
abline(col = 2, lwd = 4, reg = mod )
lines(lowess(x, y), col = 3, lwd = 4)
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
