df <- read.table("Kuckuckseier.txt", header=TRUE)
head(df)
dim(df)
# 45 4

str(df)

# Überprüfung auf NA-Werte und Anschauen der Verteilung
summary(df)

# Es gibt NA-Werte aufgrund der unterschiedlichen Anzahl von Eimessungen pro Vogelart
# NA-Werte werden bei den statistischen Tests ignoriert

attach(df)

#-------------------------------------------------------------------------------
#Grafiken und Kenzahlen (Deskription, Verteilungsannahme, Varianzhomogenität)

# kennzahlen - Funktion zur Berechnung statistischer Kennzahlen
#
# Eingabe:
#   data         - Data Frame mit metrisch skalierten Variablen
#
# Ausgabe:
#   Data Frame: Data Frame mit den Varibalen des urspruenglichen Data Frames 
#   in Zeilen und den berechneten Kennzahlen in den Spalten
#   (arithmetisches Mittel, Standardabweichung, Minimum, 25%-Quantil, Median,
#   75%-Quantil und Maximum)

kennzahlen <- function(data) {
  var_name <- names(data)
  result <- matrix(NA, nrow = length(data), ncol = 8)
  colnames(result) <- c("Merkmal", "Arithm. Mittel", "sd", "Minimum", 
                        "25% Quantil", "Median", "75% Quantil", "Maximum")
  for(i in 1:length(data)) {
    var_data <- data[[i]]
    
    q1 <- quantile(var_data, 0.25, type = 2, na.rm = TRUE)
    q3 <- quantile(var_data, 0.75, type = 2, na.rm = TRUE)
    
    result[i, 1] <- var_name[i]
    result[i, 2] <- sprintf("%.3f", mean(var_data, na.rm = TRUE))
    result[i, 3] <- sprintf("%.3f", sd(var_data, na.rm = TRUE))
    result[i, 4] <- sprintf("%.3f", min(var_data, na.rm = TRUE))
    result[i, 5] <- sprintf("%.3f", q1)
    result[i, 6] <- sprintf("%.3f", median(var_data, na.rm = TRUE))
    result[i, 7] <- sprintf("%.3f", q3)
    result[i, 8] <- sprintf("%.3f", max(var_data, na.rm = TRUE))
    
  }
  result
}

kennzahlen(df)
#Merkmal Arithm. Mittel sd      Minimum  25% Quantil Median   75% Quantil Maximum 
#[1,] "WP_1"  "22.154"       "0.979" "19.650" "21.650"    "21.950" "22.750"    "24.450"
#[2,] "BP_2"  "23.093"       "0.905" "21.050" "22.450"    "23.350" "23.850"    "24.100"
#[3,] "RK_3"  "22.594"       "0.690" "21.050" "22.100"    "22.600" "23.100"    "23.850"
#[4,] "ZK_4"  "21.127"       "0.739" "19.850" "20.800"    "21.050" "22.000"    "22.250"


#pdf("boxplot.pdf", width = 10, height = 6)
#windows(width = 10, height = 6)
opar <- par (mar = c( 4, 5, 0.2, 0.2), lwd = 2,
             cex = 1.4, las = 1, cex.axis = 1.8)
boxplot(df, main="", names=c("WP", "BP", "RK", "ZK"),
        ylab = "")
title(ylab = "Länge (in Millimetern)", line = 3.5, cex.lab = 1.8)
par(opar)
#dev.off()

#pdf("hist.pdf", width = 10, height = 6)
#windows(width = 10, height = 6)
opar <- par (mar = c( 4, 5, 0.2, 0.2), lwd = 2,
             cex = 1.4, las = 1, mfrow = c(2,2), cex.axis = 1.8)
hist(WP, freq = FALSE,
     main = "", xlim = c(19,25), ylab = "", xlab = "",
     cex.axis = 1.5, 
)
title (xlab = "WP", line = 3, cex.lab = 1.6)
title (ylab = "empirische Dichte", line = 3.5, cex.lab = 1.6)
hist(BP, freq = FALSE,
     main = "", xlim = c(19,25), ylab = "", xlab = "",
     cex.axis = 1.5
)
title (xlab = "BP", line = 3, cex.lab = 1.6)
title (ylab = "empirische Dichte", line = 3.5, cex.lab = 1.6)
hist(RK, freq = FALSE,
     main = "", xlim = c(19,25), ylab = "", xlab = "",
     cex.axis = 1.5, cex.lab = 1.6
)
title (xlab = "RK", line = 3, cex.lab = 1.6)
title (ylab = "empirische Dichte", line = 3.5, cex.lab = 1.6)
hist(ZK, freq = FALSE,
     main = "", xlim = c(19,25), ylab = "", xlab = "",
     cex.axis = 1.5
)
title (xlab = "ZK", line = 3, cex.lab = 1.6)
title (ylab = "empirische Dichte", line = 3.5, cex.lab = 1.6)
par(opar)
#dev.off()

# Überprüfung auf Normalverteilung mit NQ-Plots

#pdf("QQ.pdf", width = 10, height = 7)
#windows(width = 10, height = 7)
opar <- par (mar = c(4, 5.5, 1.5, 0.2), lwd = 2,
             cex = 1.4, las = 1, mfrow = c(2,2))
qqnorm(WP, pch = 16, main = "WP", xlab= "", ylab = "", cex = 1.4, cex.axis = 1.4
)
  title (xlab = "Theoretisches Quantil", line = 2.5, cex.lab = 1.6)
  title (ylab = "Empirisches Quantil", line = 3.7, cex.lab = 1.6)
qqline(WP, col = "red", lwd = 2)

qqnorm(BP, pch = 16, main = "BP", xlab= "", ylab = "", cex = 1.4, cex.axis = 1.4
)
title (xlab = "Theoretisches Quantil", line = 2.5, cex.lab = 1.6)
title (ylab = "Empirisches Quantil", line = 3.7, cex.lab = 1.6)
qqline(BP, col = "red", lwd = 2)

qqnorm(RK, pch = 16, main = "RK", xlab= "", ylab = "", cex = 1.4, cex.axis = 1.4
)
title (xlab = "Theoretisches Quantil", line = 2.5, cex.lab = 1.6)
title (ylab = "Empirisches Quantil", line = 3.7, cex.lab = 1.6)
qqline(RK, col = "red", lwd = 2)

qqnorm(ZK, pch = 16, main = "ZK", xlab= "", ylab = "", cex = 1.4, cex.axis = 1.4
)
title (xlab = "Theoretisches Quantil", line = 2.5, cex.lab = 1.6)
title (ylab = "Empirisches Quantil", line = 3.7, cex.lab = 1.6)
qqline(ZK, col = "red", lwd = 2)
par(opar)
#dev.off
# spricht nichts gegen eine Normalverteilung der Variablen


#-----------------------------------------------------------------------------------------
#Abschlussprinzip


# Umformen vom Datensatz
# 1 = WP,  2 = BP,  3 = RK, 4 = ZK
names(df) <- c("WP_1", "BP_2", "RK_3", "ZK_4")
long_df <- stack(df)
names(long_df) <- c("Length", "Species")  # Umbenennen der Spalten
long_df <- na.omit(long_df)

# Überprüfung auf Varianzhomogenität mit Levene's Test
# H0: Die Varianzen in allen Gruppen sind gleich (Varianzhomogenität).
# H1: Mindestens eine Gruppe hat eine andere Varianz (keine Varianzhomogenität).
leveneTest(Length ~ Species, data = long_df)
# p-Wert = 0.6465 > 0.05, H0 kann nicht abgelehnt werden.
# Es gibt keine signifikante Unterschiede in den Varianzen. 


# Testen auf H0: mu1 = mu2 = mu3 = mu4
anova_result <- aov(Length ~ Species, data = long_df)
summary(anova_result)
# p-Wert = 2.65e-07 < 0.05, H0 wird abgelehnt, Entscheidung für H1.
# Das bedeutet, dass es signifikante Unterschiede in den Mittelwerten der Längen
# der Kuckuckseier zwischen den Vogelarten gibt.

# Testen auf H0: mu1 = mu2 = mu3
anova_result <- aov(Length ~ Species, data = long_df[long_df$Species != "ZK_4", ])
summary(anova_result) # p-Wert: 0.00305 -> signifikant

# Testen auf H0: mu2 = mu3 = mu4
anova_result <- aov(Length ~ Species, data = long_df[long_df$Species != "WP_1", ])
summary(anova_result) # p-Wert: 4.43e-08 -> signifikant

# Testen auf H0: mu1 = mu3 = mu4
anova_result <- aov(Length ~ Species, data = long_df[long_df$Species != "BP_2", ])
summary(anova_result) # p-Wert: 4.39e-05 -> signifikant

# Testen auf H0: mu1 = mu2 = mu4
anova_result <- aov(Length ~ Species, data = long_df[long_df$Species != "RK_3", ])
summary(anova_result) # p-Wert: 8.56e-07 -> signifikant


#Zweistichproben-t-Test
# Testen auf H0: mu1 = mu2
t.test(Length ~ Species, 
       data=long_df[long_df$Species == "WP_1" | long_df$Species == "BP_2", ], var.equal = TRUE) 
# p-Wert = 0.001781 

# Testen auf H0: mu3 = mu4
t.test(Length ~ Species, 
       data=long_df[long_df$Species == "RK_3" | long_df$Species == "ZK_4", ], var.equal = TRUE) 
# p-Wert = 3.469e-06

# Testen auf H0: mu1 = mu3
t.test(Length ~ Species, 
       data=long_df[long_df$Species == "WP_1" | long_df$Species == "RK_3", ], var.equal = TRUE) 
# p-Wert = 0.1039

# Testen auf H0: mu2 = mu4
t.test(Length ~ Species, 
       data=long_df[long_df$Species == "BP_2" | long_df$Species == "ZK_4", ], var.equal = TRUE) 
# p-Wert = 4.595e-07

# Testen auf H0: mu1 = mu4
t.test(Length ~ Species, 
       data=long_df[long_df$Species == "WP_1" | long_df$Species == "ZK_4", ], var.equal = TRUE) 
# p-Wert = 0.0004502

# Testen auf H0: mu2 = mu3
t.test(Length ~ Species, 
       data=long_df[long_df$Species == "BP_2" | long_df$Species == "RK_3", ], var.equal = TRUE) 
# p-Wert = 0.09326


#-----------------------------------------------------------------------------------------
#Bonferroni-Holm-Verfahren


# Zweistichproben-t-Tests wie beim Abschlussprinzip

# Ordnen wir die p-Werte von Zweistichproben-t-Tests nach:
# 4.595e-07 < 3.469e-06 < 0.0004502 < 0.001781 < 0.09326 < 0.1039

4.595e-07 < 0.05/(7 - 1) # TRUE H24
3.469e-06 < 0.05/(7 - 2) # TRUE H34
0.0004502 < 0.05/(7 - 3) # TRUE H14
0.001781 < 0.05/(7 - 4) # TRUE H12
0.09326 < 0.05/(7 - 5) # FALSE H23
0.1039 < 0.05/(7 - 6) # FALSE H13

4.595e-07 * (7 - 1) # TRUE H24
3.469e-06 * (7 - 2) # TRUE H34
0.0004502 * (7 - 3) # TRUE H14
0.001781 * (7 - 4) # TRUE H12
0.09326 * (7 - 5) # FALSE H23
0.1039 * (7 - 6) # FALSE H13

p_values <- c(4.595e-07, 3.469e-06, 0.0004502, 0.001781, 0.09326, 0.1039)
adjusted_p_values <- p.adjust(p_values, method = "holm")
# 2.7570e-06 1.7345e-05 1.8008e-03 5.3430e-03 1.8652e-01 1.8652e-01

# adjusted_p_values < 0.05
# TRUE  TRUE  TRUE  TRUE FALSE FALSE
