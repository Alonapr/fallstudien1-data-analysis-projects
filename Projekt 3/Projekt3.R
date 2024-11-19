library(car)
library(reshape2)

df <- read.table("Kuckuckseier.txt", header=TRUE)
head(df)
dim(df)
# 45 4

str(df)

# Überprüfung auf NA-Werte und Anschauen der Verteilung
summary(df)
# Es gibt NA-Werte aufgrund der unterschiedlichen Anzahl von Eimessungen pro Vogelart
# NA-Werte werden bei den statistischen Tests ignoriert

boxplot(df, main="", names=c("WP", "BP", "RK", "ZK"))

attach(df)
par(mar = c(4.2, 4, 1, 1), mfrow = c(2, 2))
hist(WP, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3)
hist(BP, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3)
hist(RK, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3)
hist(ZK, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3)

# Überprüfung auf Normalverteilung mit NQ-Plots
for (i in 1:4){
  qqnorm(df[[i]], main = colnames(df)[i], xlab = "Theoretisches Quantil", 
         ylab = "Empirisches Quantil")
  qqline(df[[i]], col = "red", lwd = 2)
}
# spricht nichts gegen eine Normalverteilung der Variablen

# Überprüfung auf Varianzhomogenität
long_df <- melt(df, variable.name = "Species", value.name = "Length")

# Levene's Test auf Varianzhomogenität
# H0: Die Varianzen in allen Gruppen sind gleich (Varianzhomogenität).
# H1: Mindestens eine Gruppe hat eine andere Varianz (keine Varianzhomogenität).
leveneTest(Length ~ Species, data = long_df)
# p-Wert = 0.6465 > 0.05, H0 kann nicht abgelehnt werden.
# Es gibt keine signifikante Unterschiede in den Varianzen. 

# Alternative: Bartlett's test
bartlett.test(Length ~ Species, data = long_df)
# p-Wert = 0.3499 > 0.05, H0 kann nicht abgelehnt werden.

#Testen auf H0: mu1 = mu2 = mu3 = mu4
anova_result <- aov(Length ~ Species, data = long_df)
summary(anova_result)
# p-Wert = 2.65e-07 < 0.05, H0 wird abgelehnt, Entscheidung für H1.
# Das bedeutet, dass es signifikante Unterschiede in den Mittelwerten der Längen
# der Kuckuckseier zwischen den Vogelarten gibt.

# Tukey HSD Test, um zu bestimmen, zwischen welchen Paaren von Vogelarten 
# die Unterschiede bestehen
TukeyHSD(anova_result)
#        p adj
# BP-WP  0.0034136 < 0.05, signifikanten Unterschied in der Eilänge zwischen Baumpieper und Wiesenpieper
# RK-WP  0.3287336 > 0.05, keinen signifikanten Unterschied
# ZK-WP  0.0011116 < 0.05, signifikanten Unterschied in der Eilänge zwischen Zaunkönig und Wiesenpieper
# RK-BP  0.4022456 > 0.05, keinen signifikanten Unterschied
# ZK-BP  0.0000002 < 0.05, signifikanten Unterschied in der Eilänge zwischen Zaunkönig und Baumpieper
# ZK-RK  0.0000818 < 0.05, signifikanten Unterschied in der Eilänge zwischen Zaunkönig und Rotkehlchen

