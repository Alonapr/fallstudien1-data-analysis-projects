# Bibliotheken laden
library(vcd)
library(ggplot2)

setwd("C:/Users/Hanji/OneDrive/Studium/WS 24-25/Fallstudien/Projekt 4")

df = read.csv2("Medaillen.csv", header = TRUE)

#Überblick über die Daten
head(df)
dim(df)
# 20 6

str(df)
summary(df)

#deskriptive Analyse

# Gesamtanzahl von Medaillen pro Land
# Summen berechnen
total_gold <- aggregate(NrGold ~ Land, data = df, sum, na.rm = TRUE)
total_silver <- aggregate(NrSilber ~ Land, data = df, sum, na.rm = TRUE)
total_bronze <- aggregate(NrBronze ~ Land, data = df, sum, na.rm = TRUE)
total_medaillen <- aggregate(Total ~ Land, data = df, sum, na.rm = TRUE)

# Zusammenführen der Ergebnisse
medaillen_pro_land <- merge(total_gold, total_silver, by = "Land")
medaillen_pro_land <- merge(medaillen_pro_land, total_bronze, by = "Land")
medaillen_pro_land <- merge(medaillen_pro_land, total_medaillen, by = "Land")

# Spaltennamen anpassen
colnames(medaillen_pro_land) <- c("Land", "TotalGold", "TotalSilver", "TotalBronze", "TotalMedaillen")
print(medaillen_pro_land)
# Land       TotalGold TotalSilver TotalBronze TotalMedaillen
# Australien         9          12          10             31
# Frankreich         9          11          12             32
#      Japan        12           5           8             25
#        USA        27          29          26             82
#   VR China        16          14          15             45


# Verteilung der Medaillentypen (Gold, Silber, Bronze)
gesamt_gold <- sum(df$NrGold, na.rm = TRUE)
gesamt_silber <- sum(df$NrSilber, na.rm = TRUE)
gesamt_bronze <- sum(df$NrBronze, na.rm = TRUE)

# Zusammenfassung der Ergebnisse in einem Datenrahmen
medaillen_gesamt <- data.frame(
  Medaille = c("Gold", "Silber", "Bronze"),
  Anzahl = c(gesamt_gold, gesamt_silber, gesamt_bronze)
)
print(medaillen_gesamt)
# Medaille Anzahl
#     Gold     73
#   Silber     71
#   Bronze     71


#Grafische Darstellung

# Medaillen-Daten umstrukturieren
df_long <- data.frame(
  Land = rep(df$Land, 3),
  Sportart = rep(df$Sportart, 3),
  Medaille = rep(c("Gold", "Silber", "Bronze"), each = nrow(df)),
  Anzahl = c(df$NrGold, df$NrSilber, df$NrBronze)
)

# Überprüfen der Struktur der Daten
head(df_long)

# Daten aufbereiten: Aggregation nach Land, Medaillentyp und Sportart
df_stacked <- aggregate(Anzahl ~ Land + Medaille, data = df_long, sum)

# Gestapeltes Balkendiagramm
# Reihenfolge der Medaillentypen anpassen
df_stacked$Medaille <- factor(df_stacked$Medaille, levels = c("Bronze", "Silber", "Gold"))

# Diagramm mit neuer Reihenfolge
ggplot(df_stacked, aes(x = Land, y = Anzahl, fill = Medaille)) +
  geom_bar(stat = "identity") +
  labs(title = "Medaillenverteilung nach Ländern und Medaillentypen",
       x = "Land", y = "Anzahl Medaillen") +
  scale_fill_manual(values = c("Gold" = "gold", "Silber" = "gray", "Bronze" = "brown")) +
  theme_minimal()


#------
# Daten aufbereiten
df_facet <- aggregate(Anzahl ~ Land + Sportart + Medaille, data = df_long, sum)

# Reihenfolge der Medaillentypen anpassen
df_facet$Medaille <- factor(df_stacked$Medaille, levels = c("Bronze", "Silber", "Gold"))

# Facetiertes Balkendiagramm
ggplot(df_facet, aes(x = Medaille, y = Anzahl, fill = Medaille)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Sportart) +
  labs(title = "Medaillenverteilung nach Sportarten",
       x = "Medaille", y = "Anzahl Medaillen") +
  scale_fill_manual(values = c("Gold" = "gold", "Silber" = "gray", "Bronze" = "brown")) +
  theme_minimal()


#Aufgabe 1
table_sport_land <- xtabs(Total ~ Sportart + Land, data = df)
print(table_sport_land)
#                  Land
# Sportart         
#                  Australien Frankreich Japan USA VR China
# Ballsportart            3          9     4  10       14
# Kampfsport              2         15    19   9       13
# Leichtathletik          7          1     1  34        4
# Schwimmen              19          7     1  29       14

chi_test_Aufgabe1 <- chisq.test(table_sport_land)
print(chi_test_Aufgabe1)
# Pearson's Chi-squared test
# 
# data:  table_sport_land
# X-squared = 90.369, df = 12, p-value = 4.189e-14

#H0 wird abgelehnt.

fisher_test <- fisher.test(table_sport_land, simulate.p.value = TRUE)  # Fisher Test ist zu groß
print(fisher_test)
# Fisher's Exact Test for Count Data with simulated p-value (based on 2000 replicates)
# 
# data:  table_sport_land
# p-value = 0.0004998
# alternative hypothesis: two.sided

#H0 wird abgelehnt.


#Aufgabe 2

sportarten <- c("Ballsportart", "Kampfsport", "Leichtathletik", "Schwimmen")

# Kontingenztafel erstellen
table_pro_Sportart <- xtabs(Anzahl ~ Medaille + Land + Sportart, data = df_long)
print(table_pro_Sportart)
# , , Sportart = Ballsportart
# 
# Land
# Medaille Australien Frankreich Japan USA VR China
# Bronze          1          2     3   5        0
# Gold            1          2     0   3        8
# Silber          1          5     1   2        6
# 
# , , Sportart = Kampfsport
# 
# Land
# Medaille Australien Frankreich Japan USA VR China
# Bronze          2          8     5   5        6
# Gold            0          3    11   2        3
# Silber          0          4     3   2        4
# 
# , , Sportart = Leichtathletik
# 
# Land
# Medaille Australien Frankreich Japan USA VR China
# Bronze          4          0     0   9        2
# Gold            1          0     1  14        1
# Silber          2          1     0  11        1
# 
# , , Sportart = Schwimmen
# 
# Land
# Medaille Australien Frankreich Japan USA VR China
# Bronze          3          2     0   7        7
# Gold            7          4     0   8        4
# Silber          9          1     1  14        3

#Chi-Quadrat-Test nicht sinnvoll, da viele Beobachtungen kleiner als 5 sind.

fisher_test <- fisher.test(table_pro_Sportart[,,1])
print(fisher_test)
# Fisher's Exact Test for Count Data
# 
# data:  table_pro_Sportart[, , 1]
# p-value = 0.02756
# alternative hypothesis: two.sided

# Iteriere über alle Sportarten
results <- list()

for (i in 1:4) {

  fisher_test <- fisher.test(table_pro_Sportart[,,i])
  
  # Ergebnisse speichern
  results[[sportarten[i]]] <- list(
    Sportart = sportarten[i],
    Tabelle = table_pro_Sportart[,,i],
    Test = fisher_test
  )
}

# Ergebnisse anzeigen
for (i in 1:4) {
  cat("\nSportart:", sportarten[i], "\n")
  print(results[[sportarten[i]]]$Tabelle)
  print(results[[sportarten[i]]]$Test)
}



