library(car)

df <- read.table("Konzentrationsdaten.txt", header=TRUE)
head(df)
dim(df)

#Überprüfung auf NA-Werte
sum(is.na(df))
#0

#Betrachtung der Verteilung der Daten
summary(df)
attach(df)

#Bearbeitungszeit
hist(B, main = "")

#
par(mfrow = c(1,1))
par(mar = c(4.2, 4, 1, 1))
hist(AR, main = "")
hist(AA, main = "")
hist(AF, main = "")

# Vorbereitung von Daten:

# Umwandlung der Variablen in Faktoren:
gruppe <- as.factor(gruppe) 
id <- as.factor(id)
durchgang <- as.factor(durchgang)
test_typ <- as.factor(test_typ)

detach(df)

# Aufgabe 1
# Zwei unverbundene Stichproben

df_durchgang1 <- subset(df, durchgang == 1)

par(mfrow = c(1,1))
boxplot(df_durchgang1$KL ~ df_durchgang1$test_typ, col = "lightblue",
        xlab = "Test Typ", ylab = "Konzentrationsscores")

# Entfernen der Zeile mit Extremwerten:
df <- df[df$id != "14",]  
df_durchgang1 <- subset(df, durchgang == 1)

erster_durchgang_gu_kl <- df_durchgang1[df_durchgang1$test_typ == "gu", ]$KL
erster_durchgang_ug_kl <- df_durchgang1[df_durchgang1$test_typ == "ug", ]$KL

boxplot(erster_durchgang_gu_kl,
        erster_durchgang_ug_kl,
        names = c("gu", "ug"),
        ylab = "KL",
        xlab = "Aufgabe für den ersten Durchgang", horizontal = FALSE, col = "lightblue")

hist(erster_durchgang_gu_kl, xlab = "KL für gu", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

hist(erster_durchgang_ug_kl, xlab = "KL für ug", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

# Rechne Kolmogorov-Smirnov-Test auf Normalverteilung
# Berechne dazu arithmetisches Mittel als e-treuen Schätzer für mu und 
# Stichprobenvarianz als e-treuen Schätzer der Varianz

mu <- mean(df_durchgang1$KL)
# 11.0475
sigma2 <- var(df_durchgang1$KL)
# 14.45692

# ks.test aus base R als Test auf Normalverteilung
ks.test(df_durchgang1$KL, y="pnorm", mean=mu, sd=sqrt(sigma2))

# Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  df_durchgang1$KL
# D = 0.08848, p-value = 0.9129
# alternative hypothesis: two-sided

# Wir verwerfen die H0 nicht, dass sie aus einer N(mu, sqrt(sigma2))-Verteilung 
# stammen -> nichts spricht dagegen, dass die Daten normalverteilt sind

# Zusätzlich können wir das QQ-Plot erstellen, um visuell die NV-Annahme zu überprüfen
qqnorm(df_durchgang1$KL, main = "Q-Q Plot für KL")
qqline(df_durchgang1$KL, col = "red", lwd = 2)

# Diese Funktion überprüft schon impizit, ob Varianzen gleich sind (Welch’s t-test):
t.test(erster_durchgang_gu_kl, erster_durchgang_ug_kl)
# oder alternativ: t.test(KL ~ gruppe, data = df_durchgang1)

# p-value = 0.7348 => die Unterschiede sind nicht signifikant


# Aufgabe 2
# Zwei verbundene Stichproben

df_ohne_gruppe <- df[, -c(1, 3)]

# a)
erster_durchgang_kl <- df_ohne_gruppe[df_ohne_gruppe$durchgang == "1", ]$KL
zweiter_durchgang_kl <- df_ohne_gruppe[df_ohne_gruppe$durchgang == "2", ]$KL

# Deskriptive Analyse
boxplot(erster_durchgang_kl,
        zweiter_durchgang_kl,
        names = c("1. Durchgang", "2. Durchgang"),
        ylab = "KL",
        xlab = "", horizontal = FALSE, col = "lightblue")

hist(erster_durchgang_kl, xlab = "KL für 1. Durchgang", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

hist(zweiter_durchgang_kl, xlab = "KL für 2. Durchgang", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

differences <- zweiter_durchgang_kl - erster_durchgang_kl

qqnorm(differences)
qqline(differences, col = "red", lwd = 2)
# Differenzen sind nicht normalverteilt => wir können t-test nicht verwenden

# -> Nichtparametrischer Wilcoxon-Vorzeichen-Rangtest:
wilcox.test(zweiter_durchgang_kl, erster_durchgang_kl, paired = TRUE, 
            alternative = "greater")
# p-value = 2.153e-05 => Wir lehnen H0 ab, die Konzentrationsscore 
# verbessert sich durch einen Wiederholungseffekt

# b)
erster_durchgang_zeit <- df_ohne_gruppe[df_ohne_gruppe$durchgang == "1", ]$B
zweiter_durchgang_zeit <- df_ohne_gruppe[df_ohne_gruppe$durchgang == "2", ]$B

# Deskriptive Analyse
boxplot(erster_durchgang_zeit,
        zweiter_durchgang_zeit,
        names = c("1. Durchgang", "2. Durchgang"),
        ylab = "Bearbeitungzeit",
        xlab = "", horizontal = FALSE, col = "lightblue")

hist(erster_durchgang_zeit, xlab = "Bearbeitungzeit für 1. Durchgang", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

hist(zweiter_durchgang_zeit, xlab = "Bearbeitungzeit für 2. Durchgang", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

differences <- zweiter_durchgang_zeit - erster_durchgang_zeit

qqnorm(differences)
qqline(differences, col = "red", lwd = 2)
# Differenzen sind nicht normalverteilt => wir können t-test nicht verwenden

# -> Nichtparametrischer Wilcoxon-Vorzeichen-Rangtest:
wilcox.test(zweiter_durchgang_zeit, erster_durchgang_zeit, paired = TRUE, 
            alternative = "less")
# p-value = 3.877e-05 => wie lehnen H0 ab, die Bearbeitungszeit verbessert sich 
# durch einen Wiederholungseffekt
