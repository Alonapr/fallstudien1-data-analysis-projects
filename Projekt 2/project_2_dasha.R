df <- read.table("Konzentrationsdaten.txt", header=TRUE)
head(df)

# Extremwerte wegwerfen:
df <- df[df$id != "14",]

# Aufgabe 1

df_durchgang1 <- df[df$durchgang == "1", ]
df_durchgang1_gu <- df[df$durchgang == "1" & df$test_typ == "gu", ]
df_durchgang1_ug <- df[df$durchgang == "1" & df$test_typ == "ug", ]

boxplot(df_durchgang1_gu$KL,
        df_durchgang1_ug$KL,
        names = c("gu", "ug"),
        ylab = "KL",
        xlab = "Aufgabe für den ersten Durchgang", horizontal = FALSE, col = "lightblue")

hist(df_durchgang1_gu$KL, xlab = "KL für gu", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

hist(df_durchgang1_ug$KL, xlab = "KL für ug", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

# Rechne Kolmogorov-Smirnov-Test auf Normalverteilung
# Berechne dazu arithmetisches Mittel als e-treuen Schätzer für mu und 
# Stichprobenvarianz als e-treuen Schätzer der Varianz

mu <- mean(df_durchgang1$KL)
# 10.17805
stand_abw <- sd(df_durchgang1$KL)
# 45.08926

# ks.test aus base R als Test auf Normalverteilung
ks.test(df_durchgang1$KL, y="pnorm", mean=mu, sd=sqrt(sigma2))

# Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  df_durchgang1$KL
# D = 0.21325, p-value = 0.04804
# alternative hypothesis: two-sided

# Wir verwerfen die H0, dass sie aus einer N(mu, sqrt(sigma2))-Verteilung stammen
# -> die Daten sind nicht normalverteilt

wilcox.test(df_durchgang1_gu$KL, df_durchgang1_ug$KL, alternative = "two.sided")

# Wilcoxon rank sum test with continuity correction
# 
# data:  df_durchgang1_gu$KL and df_durchgang1_ug$KL
# W = 218, p-value = 0.8449
# alternative hypothesis: true location shift is not equal to 0

# -> keine signifikante Unterschiede


# Aufgabe 2
df_ohne_gruppen <- df[, -c(df$gruppe)]
head(df_ohne_gruppen)



