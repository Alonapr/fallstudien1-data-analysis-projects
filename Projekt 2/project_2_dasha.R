library(car)

df <- read.table("Konzentrationsdaten.txt", header=TRUE)
head(df)
dim(df)

# Überprüfung auf NA-Werte
sum(is.na(df)) # 0

# Betrachtung der Verteilung der KL nach Gruppe
par(mar = c(4.2, 4, 1, 1))
boxplot(df$KL ~ df$gruppe, col = "lightblue",
        xlab = "Gruppe", ylab = "Konzentrationsleistung", 
        cex.axis = 1.5, cex.lab = 1.5)
# Häufigkeitsverteilung der AF
table(df$AF)

# Entfernen der Zeile mit Extremwerten:
df <- df[df$id != "14",]

attach(df)
# Betrachtung der Verteilung der Daten nach Entfernung der Extremwerten
par(mar = c(4.2, 4, 1, 1), mfrow = c(1, 2))
barplot(table(AR), main = "", ylab = "Anzahl", xlab = "AR", 
     cex.axis = 1.1, cex.lab = 1.3, ylim = c(0,25)) 
barplot(table(AA), main = "", ylab = "Anzahl", xlab = "AA",
     cex.axis = 1.1, cex.lab = 1.3, ylim = c(0,25))

hist(B, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3, ylim = c(0, 0.02))
hist(KL, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3, ylim = c(0, 0.1))

num_stat <- function(data) {
  var_name <- c("B", "KL", "AR", "AA")
  result <- matrix(NA, nrow = length(data), ncol = 8)
  colnames(result) <- c("Merkmal", "min", "q0.25", "q0.5", 
                        "Arith. Mittel", "q0.75", "max", "sd")
  for(i in 1:length(data)) {
    var_data <- data[[i]]
    
    q1 <- quantile(var_data, 0.25, type = 2)
    q3 <- quantile(var_data, 0.75, type = 2)
    
    result[i, 1] <- var_name[i]
    result[i, 2] <- sprintf("%.3f", min(var_data))
    result[i, 3] <- sprintf("%.3f", q1)
    result[i, 4] <- sprintf("%.3f", median(var_data))
    result[i, 5] <- sprintf("%.3f", mean(var_data))
    result[i, 6] <- sprintf("%.3f", q3)
    result[i, 7] <- sprintf("%.3f", max(var_data))
    result[i, 8] <- sprintf("%.3f", sd(var_data))
  } 
  return(as.data.frame(result))
}
num_stat(df[,c(4, 8, 5, 6)])

par(mfrow = c(1, 1)) 

# Umwandlung der Variablen in Faktoren:
gruppe <- as.factor(gruppe) 
id <- as.factor(id)
durchgang <- as.factor(durchgang)
test_typ <- as.factor(test_typ)

detach(df)

# AUFGABE 1
# Zwei unverbundene Stichproben

df_durchgang1 <- subset(df, durchgang == 1)
durchgang1_gu_kl <- df_durchgang1[df_durchgang1$test_typ == "gu", ]$KL
durchgang1_ug_kl <- df_durchgang1[df_durchgang1$test_typ == "ug", ]$KL

# Vergleich der Konzentrationsleistung zwischen GU- und UG-Test im ersten Durchgang
boxplot(df_durchgang1$KL ~ df_durchgang1$test_typ, col = "lightblue",
        xlab = "Test Typ", ylab = "Konzentrationsleistung")

# Verteilung der Konzentrationsleistung für GU im ersten Durchgang
hist(durchgang1_gu_kl, xlab = "Konzentrationsleistung für GU",
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

# Verteilung der Konzentrationsleistung für UG im ersten Durchgang
hist(durchgang1_ug_kl, xlab = "Konzentrationsleistung für UG",
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

# Rechne Kolmogorov-Smirnov-Test auf Normalverteilung
# GU Test:
mu <- mean(durchgang1_gu_kl) # 11.255
sigma2 <- var(durchgang1_gu_kl) # 16.22261
ks.test(durchgang1_gu_kl, y="pnorm", mean=mu, sd=sqrt(sigma2))
# p-value = 0.6115

# UG Test:
mu <- mean(durchgang1_ug_kl) # 10.84
sigma2 <- var(durchgang1_ug_kl) # 13.36147
ks.test(durchgang1_ug_kl, y="pnorm", mean=mu, sd=sqrt(sigma2))
# p-value = 0.8863

# Wir verwerfen die H0 bei beiden Testen nicht, dass die Daten aus einer 
# N(mu, sqrt(sigma2))-Verteilung stammen -> nichts spricht dagegen, 
# dass die Daten normalverteilt sind

# Zusätzlich erstellen wir das QQ-Plot, um visuell die NV-Annahme zu überprüfen:
# QQ-Plot zur Überprüfung der Normalverteilung der Konzentrationsleistung (KL) (GU-Test)
qqnorm(durchgang1_gu_kl, main = "",  xlab = "Theoretisches Quantil", ylab = "Empirisches Quantil")
qqline(durchgang1_gu_kl, col = "red", lwd = 2)

# QQ-Plot zur Überprüfung der Normalverteilung der Konzentrationsleistung (KL) (UG-Test)
qqnorm(durchgang1_ug_kl, main = "", xlab = "Theoretisches Quantil", ylab = "Empirisches Quantil")
qqline(durchgang1_ug_kl, col = "red", lwd = 2)
# Die Q-Q Plots zeigen, dass die Daten für beide Testgruppen (GU und UG) 
# annähernd normalverteilt sind. Die Anzahl an Beobachtungen ist aber niedrig. 

# Zusätzlich: Shapiro-Wilk-Normalitätstest verwirft die H0 auch nicht:
shapiro.test(durchgang1_gu_kl) # p-value = 0.5617
shapiro.test(durchgang1_ug_kl) # p-value = 0.3286

var.test(durchgang1_gu_kl, durchgang1_ug_kl)
# p-value = 0.6767 => die Varianzen sind nicht signifikant unterschiedlich

# Verwenden wir den t-Test zur Überprüfung der Unterschiede 
# in Konzentrationsscores zwischen GU und UG Test:
t.test(durchgang1_gu_kl, durchgang1_ug_kl, var.equal = TRUE)
# oder alternativ: t.test(KL ~ gruppe, data = df_durchgang1)
# p-value = 0.7348 => die Unterschiede sind nicht signifikant!


# AUFGABE 2
# Zwei verbundene Stichproben

# a) Verbesserung des Konzentrationsscores durch Wiederholungseffekt
durchgang1_kl <- df[df$durchgang == "1", ]$KL
durchgang2_kl <- df[df$durchgang == "2", ]$KL

# Verbesserung des Konzentrationsscores durch Wiederholungseffekt
boxplot(durchgang1_kl,
        durchgang2_kl,
        names = c("1. Durchgang", "2. Durchgang"),
        ylab = "Konzentrationsleistung",
        xlab = "", horizontal = FALSE, col = "lightblue")

par(mfrow = c(1, 2)) 

# Verteilung des Konzentrationsscores im 1. Durchgang
hist(durchgang1_kl, xlab = "KL für 1. Durchgang", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")
lines(density(durchgang1_kl), col = "blue", lwd = 2)
abline(v = median(durchgang1_kl), col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Dichte", "Median"), 
       col = c("blue", "red"), lwd = 2, lty = c(1, 2), bty = "n")

# Verteilung des Konzentrationsscores im 2. Durchgang
hist(durchgang2_kl, xlab = "KL für 2. Durchgang", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "", breaks = 9)
lines(density(durchgang2_kl), col = "blue", lwd = 2)
abline(v = median(durchgang2_kl), col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Dichte", "Median"), 
       col = c("blue", "red"), lwd = 2, lty = c(1, 2), bty = "n")
par(mfrow = c(1, 1)) 

differences <- durchgang2_kl - durchgang1_kl

mu <- mean(differences) # 2.7125
sigma2 <- var(differences) # 10.26369

ks.test(differences, y="pnorm", mean=mu, sd=sqrt(sigma2)) 
# p-value = 0.676 => wir können die H0 beibehalten

# Zusätlich:
shapiro.test(differences) # p-value = 0.1214 => H0 wird beibehalten

# QQ-Plot der Differenzen im Konzentrationsscore
qqnorm(differences, main="", xlab = "Theoretisches Quantil", ylab = "Empirisches Quantil")
qqline(differences, col = "red", lwd = 2)
# Differenzen sind normalverteilt => wir können t-test verwenden

# t.test(zweiter_durchgang_kl, erster_durchgang_kl, paired = TRUE, alternative = "greater")
t.test(differences, alternative = "greater")
# p-value = 2.033e-06, wir lehnen H0 ab => entscheiden für H1 (2. Durchgang > 1. Durchgang)
# => die Konzentrationsscore verbessert sich durch einen Wiederholungseffekt

# b) Verbesserung der Bearbeitungszeit durch Wiederholungseffekt
durchgang1_zeit <- df[df$durchgang == "1", ]$B
durchgang2_zeit <- df[df$durchgang == "2", ]$B

# Verbesserung des Bearbeitungszeit durch Wiederholungseffekt
boxplot(durchgang1_zeit,
        durchgang2_zeit,
        names = c("1. Durchgang", "2. Durchgang"),
        ylab = "Bearbeitungzeit",
        xlab = "", horizontal = FALSE, col = "lightblue")

par(mfrow = c(1, 2)) 
# Verteilung des Bearbeitungszeit im 1. Durchgang
hist(durchgang1_zeit, xlab = "Bearbeitungszeit für 1. Durchgang", 
     ylab = "Relative Häufigkeit", probability = TRUE, 
     main = "")
lines(density(durchgang1_zeit), col = "blue", lwd = 2)
abline(v = median(durchgang1_zeit), col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Dichte", "Median"), 
       col = c("blue", "red"), lwd = 2, lty = c(1, 2), bty = "n")

# Verteilung des Bearbeitungszeit im 2. Durchgang
hist(durchgang2_zeit, xlab = "Bearbeitungszeit für 2. Durchgang", 
     ylab = "Relative Häufigkeit", probability = TRUE, 
     main = "")
lines(density(durchgang2_zeit), col = "blue", lwd = 2)
abline(v = median(durchgang2_zeit), col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Dichte", "Median"), 
       col = c("blue", "red"), lwd = 2, lty = c(1, 2), bty = "n")
par(mfrow = c(1, 1)) 

differences <- durchgang2_zeit - durchgang1_zeit

mu <- mean(differences) # -16.8175
sigma2 <- var(differences) # 635.8917

ks.test(differences, y="pnorm", mean=mu, sd=sqrt(sigma2))
# p-value = 0.4376 => wir lehnen H0 nicht ab

# Zusätlich:
shapiro.test(differences) # p-value = 0.1187=> H0 wird beibehalten

# QQ-Plot der Differenzen im Bearbeitungszeit
qqnorm(differences, main="", xlab = "Theoretisches Quantil", ylab = "Empirisches Quantil")
qqline(differences, col = "red", lwd = 2)
# Differenzen sind normalverteilt => wir können t-test verwenden

# t.test(durchgang2_zeit, durchgang1_zeit, paired = TRUE, alternative = "less")
t.test(differences, alternative = "less")
# p-value = 7.098e-05 => wie lehnen H0 ab => die Bearbeitungszeit hat sich verbessert 


# AUFGABE 3
# Ist eine potentielle Verbesserung des Konzentrationsscores durch Wiederholung 
# größer, wenn exakt der gleiche Test wiederholt wird (Gruppe 1) im Vergleich 
# dazu, dass von UG auf GU gewechselt wird (Gruppe 2)?

# Gruppe 1:
gruppe1_durchgang1_kl <- df[df$gruppe == "1" & df$durchgang == "1", ]$KL
gruppe1_durchgang2_kl <- df[df$gruppe == "1" & df$durchgang == "2", ]$KL

differences1 <- gruppe1_durchgang2_kl - gruppe1_durchgang1_kl

mu <- mean(differences1) # 2.31
sigma2 <- var(differences1) # 9.942

ks.test(differences1, y="pnorm", mean=mu, sd=sqrt(sigma2))
# p-value = 0.9548 => wir lehnen H0 nicht ab

# Zusätlich:
shapiro.test(differences1) # p-value = 0.2586 => H0 wird beibehalten

# QQ-Plot der Differenzen in Konzentrationsscores für die 1. Gruppe
qqnorm(differences1, main="", xlab = "Theoretisches Quantil", ylab = "Empirisches Quantil")
qqline(differences1, col = "red", lwd = 2)
# Differenzen sind normalverteilt => wir können t-test verwenden

t.test(gruppe1_durchgang2_kl, gruppe1_durchgang1_kl, paired = TRUE, alternative = "greater")
t.test(differences1, alternative = "greater")
# p-value = 0.001986 => die Differenzen sind signifikant, KL verbessert sich

# Gruppe 2:
gruppe2_durchgang1_kl <- df[df$gruppe == "2" & df$durchgang == "1", ]$KL
gruppe2_durchgang2_kl <- df[df$gruppe == "2" & df$durchgang == "2", ]$KL

differences2 <- gruppe2_durchgang2_kl - gruppe2_durchgang1_kl

mu <- mean(differences2) # 3.115
sigma2 <- var(differences2) # 10.7845

ks.test(differences2, y="pnorm", mean=mu, sd=sqrt(sigma2))
# p-value = 0.9467 => wir lehnen H0 nicht ab

# Zusätlich:
shapiro.test(differences2) # p-value = 0.6598 => H0 wird beibehalten

# QQ-Plot der Differenzen in Konzentrationsscores für die 2. Gruppe
qqnorm(differences2, main="", xlab = "Theoretisches Quantil", ylab = "Empirisches Quantil")
qqline(differences2, col = "red", lwd = 2)
# Differenzen sind normalverteilt => wir können t-test verwenden

t.test(gruppe2_durchgang2_kl, gruppe2_durchgang1_kl, paired = TRUE, alternative = "greater")
t.test(differences2, alternative = "greater")
# p-value = 0.0002205 => die Differenzen sind signifikant, KL verbessert sich

var.test(differences1, differences2) 
# p-value = 0.8611 => die Varianzen sind nicht signifikant unterschiedlich

t.test(differences1, differences2, alternative = "greater", var.equal = TRUE)
# p-value = 0.783