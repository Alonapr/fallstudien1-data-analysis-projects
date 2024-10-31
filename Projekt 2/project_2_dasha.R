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
        xlab = "Test Typ", ylab = "Konzentrationsscore")

# Entfernen der Zeile mit Extremwerten:
df <- df[df$id != "14",]  
df_durchgang1 <- subset(df, durchgang == 1)

erster_durchgang_gu_kl <- df_durchgang1[df_durchgang1$test_typ == "gu", ]$KL
erster_durchgang_ug_kl <- df_durchgang1[df_durchgang1$test_typ == "ug", ]$KL

boxplot(erster_durchgang_gu_kl,
        erster_durchgang_ug_kl,
        names = c("gu", "ug"),
        ylab = "Konzentrationsscore",
        xlab = "Test Typ", col = "lightblue")

hist(erster_durchgang_gu_kl, xlab = "Konzentrationsscore für gu", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

hist(erster_durchgang_ug_kl, xlab = "Konzentrationsscore für ug", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

# Rechne Kolmogorov-Smirnov-Test auf Normalverteilung
# Berechne dazu arithmetisches Mittel als e-treuen Schätzer für mu und 
# Stichprobenvarianz als e-treuen Schätzer der Varianz

# GU Test:

mu <- mean(erster_durchgang_gu_kl)
# 11.255
sigma2 <- var(erster_durchgang_gu_kl)
# 16.22261

# ks.test aus base R als Test auf Normalverteilung
ks.test(erster_durchgang_gu_kl, y="pnorm", mean=mu, sd=sqrt(sigma2))
# p-value = 0.6115


# UG Test:

mu <- mean(erster_durchgang_ug_kl)
# 10.84
sigma2 <- var(erster_durchgang_ug_kl)
# 13.36147

# ks.test aus base R als Test auf Normalverteilung
ks.test(erster_durchgang_ug_kl, y="pnorm", mean=mu, sd=sqrt(sigma2))
# p-value = 0.8863

# Wir verwerfen die H0 bei beiden Testen nicht, dass sie aus einer N(mu, sqrt(sigma2))-Verteilung 
# stammen -> nichts spricht dagegen, dass die Daten normalverteilt sind

# Zusätzlich können wir das QQ-Plot erstellen, um visuell die NV-Annahme zu überprüfen
qqnorm(erster_durchgang_gu_kl, main = "Q-Q Plot für KL, GU")
qqline(erster_durchgang_gu_kl, col = "red", lwd = 2)

qqnorm(erster_durchgang_ug_kl, main = "Q-Q Plot für KL, UG")
qqline(erster_durchgang_ug_kl, col = "red", lwd = 2)

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

mu <- mean(differences)
# 2.7125
sigma2 <- var(differences)
# 10.26369

ks.test(differences, y="pnorm", mean=mu, sd=sqrt(sigma2))

qqnorm(differences)
qqline(differences, col = "red", lwd = 2)
# Differenzen sind normalverteilt => wir können t-test nicht verwenden

t.test(erster_durchgang_kl, zweiter_durchgang_kl, paired = TRUE, alternative = "less")

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
