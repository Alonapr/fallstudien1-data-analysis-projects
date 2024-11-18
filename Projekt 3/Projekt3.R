df <- read.table("Kuckuckseier.txt", header=TRUE)
head(df)
dim(df)
#45 4

str(df)

# Überprüfung auf NA-Werte und Anschauen der Verteilung
summary(df)

par(mar = c(4.2, 4, 1, 1), mfrow = c(2, 2))
hist(WP, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3)
hist(BP, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3)
hist(RK, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3)
hist(ZK, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3)



