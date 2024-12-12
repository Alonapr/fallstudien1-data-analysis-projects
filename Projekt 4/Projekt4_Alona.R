library(readxl)
library(ggplot2)
library(reshape2)

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
ggplot(ds, aes(x = Land, y = Total, fill = Land)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("lightsalmon", "cornsilk", "lightgreen", "lightblue", "plum1")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  
        axis.text.y = element_text(size = 14),                        
        axis.title = element_text(size = 16))                         


# Chi-Quadrat-Test
# Um zu prüfen, ob es eine statistisch signifikante Abhängigkeit 
# zwischen Land und Total gibt

kontingenztafel1 <- dcast(ds, Land ~ Sportart, value.var = "Total", fun.aggregate = sum)
kontingenztafel1
medaillen_matrix <- as.matrix(kontingenztafel[, -1])
chisq.test(medaillen_matrix)
#p-value = 4.189e-14 < 0.05 deutet auf signifikante Abhänigkeit hin

# ANOVA-Modell, um die Effekte von Land und Sportart auf die Medaillenanzahl zu untersuchen
anova_modell <- aov(Total ~ Land + Sportart)
summary(anova_modell)
                  

