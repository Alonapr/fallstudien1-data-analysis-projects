library(car) #Für Levene-Test

df <- read.table("Kuckuckseier.txt", header=TRUE)
head(df)
dim(df)
# 45 4

# 1 = WP,  2 = BP,  3 = RK, 4 = ZK
str(df)

# Überprüfung auf NA-Werte und Anschauen der Verteilung
summary(df)

# Es gibt NA-Werte aufgrund der unterschiedlichen Anzahl von Eimessungen pro Vogelart
# NA-Werte werden bei den statistischen Tests ignoriert

boxplot(df, main="", names=c("WP", "BP", "RK", "ZK"))
names(df) <- c("WP_1", "BP_2", "RK_3", "ZK_4")

attach(df)
par(mar = c(4.2, 4, 1, 1), mfrow = c(2, 2))
hist(WP_1, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3)
hist(BP_2, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3)
hist(RK_3, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3)
hist(ZK_4, main = "", ylab = "Empirische Dichte", probability = TRUE, 
     cex.axis = 1.1, cex.lab = 1.3)

#-----------------------------------------------------------------------------------------
#Abschlussprinzip

# Überprüfung auf Normalverteilung mit NQ-Plots
for (i in 1:4){
  qqnorm(df[[i]], main = colnames(df)[i], xlab = "Theoretisches Quantil", 
         ylab = "Empirisches Quantil")
  qqline(df[[i]], col = "red", lwd = 2)
}
# spricht nichts gegen eine Normalverteilung der Variablen

# Überprüfung auf Varianzhomogenität
long_df <- stack(df)
names(long_df) <- c("Length", "Species")  # Umbenennen der Spalten
long_df <- na.omit(long_df)


# Levene's Test auf Varianzhomogenität
# H0: Die Varianzen in allen Gruppen sind gleich (Varianzhomogenität).
# H1: Mindestens eine Gruppe hat eine andere Varianz (keine Varianzhomogenität).
leveneTest(Length ~ Species, data = long_df)
# p-Wert = 0.6465 > 0.05, H0 kann nicht abgelehnt werden.
# Es gibt keine signifikante Unterschiede in den Varianzen. 


#Testen auf H0: mu1 = mu2 = mu3 = mu4
anova_result <- aov(Length ~ Species, data = long_df)
summary(anova_result)
# p-Wert = 2.65e-07 < 0.05, H0 wird abgelehnt, Entscheidung für H1.
# Das bedeutet, dass es signifikante Unterschiede in den Mittelwerten der Längen
# der Kuckuckseier zwischen den Vogelarten gibt.

anova_result <- aov(Length ~ Species, data = long_df[long_df$Species != "ZK_4", ])
summary(anova_result) # p-Wert: 0.00305 -> signifikant

anova_result <- aov(Length ~ Species, data = long_df[long_df$Species != "WP_1", ])
summary(anova_result) # p-Wert: 4.43e-08 -> signifikant

anova_result <- aov(Length ~ Species, data = long_df[long_df$Species != "BP_2", ])
summary(anova_result) # p-Wert: 4.39e-05 -> signifikant

anova_result <- aov(Length ~ Species, data = long_df[long_df$Species != "RK_3", ])
summary(anova_result) # p-Wert: 8.56e-07 -> signifikant


#Zweistichproben-t-Test
t.test(Length ~ Species, 
       data=long_df[long_df$Species == "WP_1" | long_df$Species == "BP_2", ], var.equal = TRUE) 
# p-Wert = 0.001781 

t.test(Length ~ Species, 
       data=long_df[long_df$Species == "RK_3" | long_df$Species == "ZK_4", ], var.equal = TRUE) 
# p-Wert = 3.469e-06

t.test(Length ~ Species, 
       data=long_df[long_df$Species == "WP_1" | long_df$Species == "RK_3", ], var.equal = TRUE) 
# p-Wert = 0.1039

t.test(Length ~ Species, 
       data=long_df[long_df$Species == "BP_2" | long_df$Species == "ZK_4", ], var.equal = TRUE) 
# p-Wert = 4.595e-07

t.test(Length ~ Species, 
       data=long_df[long_df$Species == "WP_1" | long_df$Species == "ZK_4", ], var.equal = TRUE) 
# p-Wert = 0.0004502

t.test(Length ~ Species, 
       data=long_df[long_df$Species == "BP_2" | long_df$Species == "RK_3", ], var.equal = TRUE) 
# p-Wert = 0.09326


#-----------------------------------------------------------------------------------------
#Bonferroni-Holm-Verfahren

#KS-Test

#1. WP
ks.test(long_df[long_df$Species == "WP_1",1], y = "pnorm", mean = mean(long_df[long_df$Species == "WP_1",1]),
        sd = sqrt(var(long_df[long_df$Species == "WP_1",1])))
#p-Wert  = 0.2859

#2. BP
ks.test(long_df[long_df$Species == "BP_2",1], y = "pnorm", mean = mean(long_df[long_df$Species == "BP_2",1]),
        sd = sqrt(var(long_df[long_df$Species == "BP_2",1])))
#p-Wert  = 0.5755

#3. RK
ks.test(long_df[long_df$Species == "RK_3",1], y = "pnorm", mean = mean(long_df[long_df$Species == "RK_3",1]),
        sd = sqrt(var(long_df[long_df$Species == "RK_3",1])))
#p-Wert  = 0.8098

#4. ZK
ks.test(long_df[long_df$Species == "ZK_4",1], y = "pnorm", mean = mean(long_df[long_df$Species == "ZK_4",1]),
        sd = sqrt(var(long_df[long_df$Species == "ZK_4",1])))
#p-Wert  = 0.8978


#F-Test

#1-2 WP-BP
var.test(Length ~ Species, 
         data=long_df[long_df$Species == "WP_1" | long_df$Species == "BP_2", ])
#p-Wert = 0.7826

#1-3 WP-RK
var.test(Length ~ Species, 
         data=long_df[long_df$Species == "WP_1" | long_df$Species == "RK_3", ])
#p-Wert = 0.1413

#1-4 WP-ZK
var.test(Length ~ Species, 
         data=long_df[long_df$Species == "WP_1" | long_df$Species == "ZK_4", ])
#p-Wert = 0.2538

#2-3 BP-RK
var.test(Length ~ Species, 
         data=long_df[long_df$Species == "BP_2" | long_df$Species == "RK_3", ])
#p-Wert = 0.3077

#2-4 BP - ZK
var.test(Length ~ Species, 
         data=long_df[long_df$Species == "BP_2" | long_df$Species == "ZK_4", ])
#p-Wert = 0.4581

#3-4 RK - ZK
var.test(Length ~ Species, 
         data=long_df[long_df$Species == "RK_3" | long_df$Species == "ZK_4", ])
#p-Wert = 0.7906

#Zweistichproben-t-Tests wie beim Abschlussprinzip