library(car)

df <- read.table("Konzentrationsdaten.txt", header=TRUE)
head(df)

# Vorbereitung von Daten

# Entfernen der Zeile mit Extremwerten:
df <- df[df$id != "14",]

# Umwandlung der Variablen in Faktoren:
df$gruppe <- as.factor(df$gruppe) 
df$id <- as.factor(df$id)
df$durchgang <- as.factor(df$durchgang)
df$test_typ <- as.factor(df$test_typ)


# Aufgabe 1

df_durchgang1 <- df[df$durchgang == "1", ]

boxplot(df_durchgang1[df_durchgang1$test_typ == "gu", ]$KL,
        df_durchgang1[df_durchgang1$test_typ == "ug", ]$KL,
        names = c("gu", "ug"),
        ylab = "KL",
        xlab = "Aufgabe für den ersten Durchgang", horizontal = FALSE, col = "lightblue")

hist(df_durchgang1[df_durchgang1$test_typ == "gu", ]$KL, xlab = "KL für gu", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

hist(df_durchgang1[df_durchgang1$test_typ == "ug", ]$KL, xlab = "KL für ug", 
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

t.test(KL ~ gruppe, data = df_durchgang1)

# Diese Funktion überprüft schon impizit, ob Varianzen gleich sind (Welch’s t-test)

# Two Sample t-test
# 
# data:  KL by gruppe
# t = 0.34122, df = 38, p-value = 0.7348
# alternative hypothesis: true difference in means between group 1 and group 2 is not equal to 0
# 95 percent confidence interval:
#   -2.047119  2.877119
# sample estimates:
#   mean in group 1 mean in group 2 
# 11.255          10.840 

# p-value = 0.7348 => die Unterschiede sind nicht signifikant




