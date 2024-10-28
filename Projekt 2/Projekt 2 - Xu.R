setwd("C:/Users/Hanji/OneDrive/Studium/WS 24-25/Fallstudien/Projekt 2")
daten = read.table(file = "Konzentrationsdaten.txt", header = TRUE)

summary(daten)

# gruppe        durchgang     test_typ               B                AR              AA       
# Min.   :1.000   Min.   :1.0   Length:82          Min.   : 58.30   Min.   :11.00   Min.   : 0.00  
# 1st Qu.:1.000   1st Qu.:1.0   Class :character   1st Qu.: 82.65   1st Qu.:22.00   1st Qu.: 0.00  
# Median :1.000   Median :1.5   Mode  :character   Median : 96.75   Median :23.50   Median : 1.50  
# Mean   :1.488   Mean   :1.5                      Mean   :105.41   Mean   :22.61   Mean   : 2.39  
# 3rd Qu.:2.000   3rd Qu.:2.0                      3rd Qu.:116.35   3rd Qu.:25.00   3rd Qu.: 3.00  
# Max.   :2.000   Max.   :2.0                      Max.   :200.60   Max.   :25.00   Max.   :14.00  
# AF               KL               id    
# Min.   : 0.000   Min.   :-36.30   Min.   : 1  
# 1st Qu.: 0.000   1st Qu.:  9.05   1st Qu.:11  
# Median : 0.000   Median : 12.00   Median :21  
# Mean   : 1.256   Mean   : 11.36   Mean   :21  
# 3rd Qu.: 0.000   3rd Qu.: 14.90   3rd Qu.:31  
# Max.   :43.000   Max.   : 25.70   Max.   :41 

#Überblick über die Verteilung der Daten
boxplot(daten$B)
hist(daten$B)
hist(daten$AR)
hist(daten$AA)
hist(daten$AF)
boxplot(daten$KL)
hist(daten$KL)

#Aufgabe 1

#Daten aus Durchgang 1
daten_durchgang1 = subset(daten, durchgang == 1)

boxplot(daten_durchgang1$KL ~ daten_durchgang1$test_typ)

summary(daten_durchgang1$KL[daten_durchgang1$test_typ == "gu"])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -24.600   7.700  10.200   9.548  14.600  17.800 

summary(daten_durchgang1$KL[daten_durchgang1$test_typ == "ug"])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.900   8.275  10.400  10.840  12.600  18.600

#Aufgabe 2

#Daten aus Durchgang 2
daten_durchgang2 = subset(daten, durchgang == 2)

#Boxplot des Scores für die beiden Durchgänge
boxplot(daten_durchgang1$KL, daten_durchgang2$KL, names = c("Durgang 1", "Durchgang 2"),
        ylab = "Score")

#Boxplot der Bearbeitungszeit für die beiden Durchgänge
boxplot(daten_durchgang1$B, daten_durchgang2$B, names = c("Durgang 1", "Durchgang 2"),
        ylab = "Bearbeitungszeit in Sekunden")

#Aufgabe 3

#Daten aus Gruppe 1
daten_Gruppe1 = subset(daten, gruppe == 1) #42 in Gruppe 1

#Daten aus Gruppe 2
daten_Gruppe2 = subset(daten, gruppe == 2) #40 in Gruppe 2

#Verbesserung des Scores in der Gruppe 1 (Score aus Durchgang 2 - Score aus Durchgang 1)
diff_Score_Gruppe1 = daten_Gruppe1$KL[daten_Gruppe1$durchgang == 2] - daten_Gruppe1$KL[daten_Gruppe1$durchgang == 1]

#Verbesserung des Scores in der Gruppe 2 (Score aus Durchgang 2 - Score aus Durchgang 1)
diff_Score_Gruppe2 = daten_Gruppe2$KL[daten_Gruppe2$durchgang == 2] - daten_Gruppe2$KL[daten_Gruppe2$durchgang == 1]

#Boxplot für die Differenzen
boxplot(diff_Score_Gruppe1, diff_Score_Gruppe2, names = c("Durchgang 1", "Durchgang 2"))
abline(h = 0, col = "red", lwd = 1, lty = 2)
