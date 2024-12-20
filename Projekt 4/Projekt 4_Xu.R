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

# Barplot erstellen
ggplot(df, aes(x = Land, y = Total, fill = Sportart)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    #title = "Gesamtanzahl der Medaillen pro Land und Sportart",
    x = "Land",
    y = "Gesamtanzahl der Medaillen"
  ) +
  theme(
    axis.title.x = element_text(size = 14), # X-Achsentitel vergrößern
    axis.title.y = element_text(size = 14), # Y-Achsentitel vergrößern
    axis.text.x = element_text(size = 12),  # X-Achsentext vergrößern
    axis.text.y = element_text(size = 12),  # Y-Achsentext vergrößern
    legend.title = element_text(size = 14), # Legendentitel vergrößern
    legend.text = element_text(size = 12)   # Legendentext vergrößern
  )


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
#Ballsport
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

p_values_A2 = numeric(4)

# Ergebnisse anzeigen und p-Werte speichern
for (i in 1:4) {
  cat("\nSportart:", sportarten[i], "\n")
  print(results[[sportarten[i]]]$Tabelle)
  print(results[[sportarten[i]]]$Test)
  p_values_A2[i] = results[[sportarten[i]]]$Test$p.value
}

print(p_values_A2)
# 0.02756028 0.29974928 0.53481710 0.25489817

p_values_A2 < 0.05
# TRUE FALSE FALSE FALSE

adjusted_p_values_A2 <- p.adjust(p_values_A2, method = "holm")
print(adjusted_p_values_A2)
# 0.1102411 0.7646945 0.7646945 0.7646945

adjusted_p_values_A2 < 0.05
# FALSE FALSE FALSE FALSE

#Aufgabe 3

land = c("Australien", "Frankreich", "Japan", "USA", "VR China")

# Kontingenztafel erstellen
table_pro_Land <- xtabs(Anzahl ~ Medaille + Sportart + Land, data = df_long)
print(table_pro_Land)
# , , Land = Australien
# 
# Sportart
# Medaille Ballsportart Kampfsport Leichtathletik Schwimmen
# Bronze            1          2              4         3
# Gold              1          0              1         7
# Silber            1          0              2         9
# 
# , , Land = Frankreich
# 
# Sportart
# Medaille Ballsportart Kampfsport Leichtathletik Schwimmen
# Bronze            2          8              0         2
# Gold              2          3              0         4
# Silber            5          4              1         1
# 
# , , Land = Japan
# 
# Sportart
# Medaille Ballsportart Kampfsport Leichtathletik Schwimmen
# Bronze            3          5              0         0
# Gold              0         11              1         0
# Silber            1          3              0         1
# 
# , , Land = USA
# 
# Sportart
# Medaille Ballsportart Kampfsport Leichtathletik Schwimmen
# Bronze            5          5              9         7
# Gold              3          2             14         8
# Silber            2          2             11        14
# 
# , , Land = VR China
# 
# Sportart
# Medaille Ballsportart Kampfsport Leichtathletik Schwimmen
# Bronze            0          6              2         7
# Gold              8          3              1         4
# Silber            6          4              1         3

#Chi-Quadrat-Test nicht sinnvoll, da viele Beobachtungen kleiner als 5 sind.

fisher_test <- fisher.test(table_pro_Land[,,1])
print(fisher_test)
#Australien
# Fisher's Exact Test for Count Data
# 
# data:  table_pro_Land[, , 1]
# p-value = 0.1837
# alternative hypothesis: two.sided

# Iteriere über alle Sportarten
results <- list()

for (i in 1:5) {
  
  fisher_test <- fisher.test(table_pro_Land[,,i])
  
  # Ergebnisse speichern
  results[[land[i]]] <- list(
    Land = land[i],
    Tabelle = table_pro_Land[,,i],
    Test = fisher_test
  )
}

p_values_A3 = numeric(5)

# Ergebnisse anzeigen  und p-Werte Speichern
for (i in 1:5) {
  cat("\nLand:", land[i], "\n")
  print(results[[land[i]]]$Tabelle)
  print(results[[land[i]]]$Test)
  p_values_A3[i] = results[[land[i]]]$Test$p.value
}

print(p_values_A3)
# 0.18370572 0.23344587 0.04941841 0.34310820 0.03665538

p_values_A3 < 0.05
# FALSE FALSE  TRUE FALSE  TRUE

adjusted_p_values_A3 <- p.adjust(p_values_A3, method = "holm")
print(adjusted_p_values_A3)
# 0.5511171 0.5511171 0.1976736 0.5511171 0.1832769

adjusted_p_values_A3 < 0.05
# FALSE FALSE FALSE FALSE FALSE


#To do: 
#Keine Maßzahlen, aber alle Kontingenztafeln. Barplot sinnvoll.
#Fisher-Test auf 2x2 beschreiben, aber erwähnen, dass in R für kxl Kontingenztafeln
#anders macht. ("Mehta")
