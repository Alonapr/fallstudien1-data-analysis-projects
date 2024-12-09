library(readxl)
library(ggplot2)

ds <- read_xlsx("Medaillen.xlsx")
head(ds)
str(ds)

summary(ds)
#keine NA-Werte

attach(ds)
#Deskriptive Analyse
par(mar = c(4.2, 4, 1, 1), mfrow = c(2, 2))
barplot(table(NrGold), main = "", ylab = "Anzahl", xlab = "NrGold", 
        cex.axis = 1.1, cex.lab = 1.3) 
barplot(table(NrSilber), main = "", ylab = "Anzahl", xlab = "NrSilber",
        cex.axis = 1.1, cex.lab = 1.3)
barplot(table(NrBronze), main = "", ylab = "Anzahl", xlab = "NrBronze",
        cex.axis = 1.1, cex.lab = 1.3)
barplot(table(Total), main = "", ylab = "Anzahl", xlab = "Total",
        cex.axis = 1.1, cex.lab = 1.3)


#1
#Abhängigkeit zwischen der Sportart und dem Land bezüglich der Gesamtanzahl an Medaillen
ggplot(ds, aes(x = Sportart, y = Total, fill = Land)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("lightsalmon", "cornsilk", "lightgreen", "lightblue", "plum1")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  
        axis.text.y = element_text(size = 14),                        
        axis.title = element_text(size = 16))                         

##QQ-Plots
opar <- par (mar = c(4, 5.5, 1.5, 0.2), lwd = 2,
             cex = 1.4, las = 1, mfrow = c(2,2))
qqnorm(NrGold, pch = 16, main = "NrGold", xlab= "", ylab = "", cex = 1.4, cex.axis = 1.4
)
title (xlab = "Theoretisches Quantil", line = 2.5, cex.lab = 1.6)
title (ylab = "Empirisches Quantil", line = 3.7, cex.lab = 1.6)
qqline(NrGold, col = "red", lwd = 2)

qqnorm(NrSilber, pch = 16, main = "NrSilber", xlab= "", ylab = "", cex = 1.4, cex.axis = 1.4
)
title (xlab = "Theoretisches Quantil", line = 2.5, cex.lab = 1.6)
title (ylab = "Empirisches Quantil", line = 3.7, cex.lab = 1.6)
qqline(NrSilber, col = "red", lwd = 2)

qqnorm(NrBronze, pch = 16, main = "NrBronze", xlab= "", ylab = "", cex = 1.4, cex.axis = 1.4
)
title (xlab = "Theoretisches Quantil", line = 2.5, cex.lab = 1.6)
title (ylab = "Empirisches Quantil", line = 3.7, cex.lab = 1.6)
qqline(NrBronze, col = "red", lwd = 2)

qqnorm(Total, pch = 16, main = "Total", xlab= "", ylab = "", cex = 1.4, cex.axis = 1.4
)
title (xlab = "Theoretisches Quantil", line = 2.5, cex.lab = 1.6)
title (ylab = "Empirisches Quantil", line = 3.7, cex.lab = 1.6)
qqline(Total, col = "red", lwd = 2)


# Chi-Quadrat-Test
# Um zu prüfen, ob es eine statistisch signifikante Abhängigkeit 
# zwischen Land und Total gibt
chisq.test(table(Land, Total))

chisq.test(table(Sportart, Total))

# ANOVA-Modell, um die Effekte von Land und Sportart auf die Medaillenanzahl zu untersuchen
anova_modell <- aov(Total ~ Land + Sportart)
summary(anova_modell)
                  

