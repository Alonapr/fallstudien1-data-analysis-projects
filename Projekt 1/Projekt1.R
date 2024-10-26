library(ggplot2)

data <- read.csv("census_2022_2002.csv")

# Überblick über die Daten
dim(data)
str(data)
summary(data) # 7 NA-Werten in kardinalen Merkmalen

# Länder mit fehlenden Werten in gesamten Lebenserwartung
subset(data, is.na(Life_Expectancy_Overall), select = Country)
# Entfernung von NA-Werten
data_noNA <- na.omit(data)

# Datensatz auf Jahr 2002 und 2022 teilen
data2002 <- subset(data_noNA, Year == 2002)
dim(data2002)

data2022 <- subset(data_noNA, Year == 2022)
dim(data2022)


#1 - Haeufigkeitsverteilungen der Merkmale + Unterschiede zwischen Geschlechter
attach(data2022)

# num_stat - Funktion zur Berechnung statistischer Kennzahlen für den Datensatz
#             aus der IDB
#
# Eingabe:
#   data         - Data Frame aus dem IDB-Datensatz mit den Spalten
#                   Life_Expectancy_Overall, Life_Expectancy_Male,
#                   Life_Expectancy_Female und Total_Fertility_Rate
#
# Ausgabe:
#   Data Frame: Data Frame mit den Zeilen "Gesamte Lebenserwartung",
#   "Lebenserwartung der Männer", "Lebenserwartung der Frauen" und 
#   "Fertilitätsrate" und den berechneten Kennzahlen in den Spalten
#   (arithmetisches Mittel, Median, 25%-Quantil, 75%-Quantil, Minimum, Maximum
#   und Standardabweichung)

num_stat <- function(data) {
  var_name <- c("Gesamte Lebenserwartung", "Lebenserwartung der Männer", 
                "Lebenserwartung der Frauen", "Fertilitätsrate")
  result <- matrix(NA, nrow = length(data), ncol = 8)
  colnames(result) <- c("Merkmal", "Arithm. Mittel", "Median", "25% Quantil", 
                        "75% Quantil", "Minimum", "Maximum", "sd")
  for(i in 1:length(data)) {
    var_data <- data[[i]]
    
    q1 <- quantile(var_data, 0.25, type = 2)
    q3 <- quantile(var_data, 0.75, type = 2)
    
    result[i, 1] <- var_name[i]
    result[i, 2] <- sprintf("%.3f", mean(var_data))
    result[i, 3] <- sprintf("%.3f", median(var_data))
    result[i, 4] <- sprintf("%.3f", q1)
    result[i, 5] <- sprintf("%.3f", q3)
    result[i, 6] <- sprintf("%.3f", min(var_data))
    result[i, 7] <- sprintf("%.3f", max(var_data))
    result[i, 8] <- sprintf("%.3f", sd(var_data))
  } 
  return(as.data.frame(result))
}
num_stat(data2022[,3:6])

#Korrelationskoeffizient nach Bravais-Pearson

# berechne_pearson - Funktion zur Berechnung des Korrelationskoeffizienten nach
#                   Bravais-Pearson
#
# Eingabe:
#   x         - Datenvektor mit n Beobachtungen
#   y         - Datenvektor mit n Beobachtungen
#
# Ausgabe:
#   Skalar: Korrelationskoeffizient nach Bravais-Pearson

berechne_pearson <- function(x, y) {
  
  # Überprüfen, ob die Vektoren die gleiche Länge haben
  if (length(x) != length(y)) {
    stop("Die Vektoren müssen die gleiche Länge haben.")
  }
  
  # Überprüfen, ob die Vektoren genügend Datenpunkte enthalten
  if (length(x) < 2) {
    stop("Die Vektoren müssen mindestens zwei Elemente enthalten.")
  }
  
  # Berechnung der Mittelwerte
  x_mean <- mean(x)
  y_mean <- mean(y)
  
  # Zähler und Nenner berechnen
  zaehler <- sum((x - x_mean) * (y - y_mean))
  nenner <- sqrt(sum((x - x_mean)^2) * sum((y - y_mean)^2))
  
  # Verhindern von Division durch Null
  if (nenner == 0) {
    stop("Achtung: Division durch Null.")
  }
  
  # Pearson-Korrelationskoeffizient berechnen
  r <- zaehler / nenner
  
  return(r)
}

#Lebenserwartung vs. Fertilitätsrate
round(berechne_pearson(Life_Expectancy_Overall, Total_Fertility_Rate), 3)
#-0.767

#Lebenserwartung Männer vs. Lebenserwartung Frauen
round(berechne_pearson(Life_Expectancy_Male, Life_Expectancy_Female), 3)
#0.969

##########################

# Boxplots der Lebenserwartung (+ Unterschiede zwischen den Geschlechtern)
par(mar = c(4.2, 4, 1, 1))
boxplot(Life_Expectancy_Overall,
        Life_Expectancy_Male,
        Life_Expectancy_Female,
        names = c("Gesamt", "Männer", "Frauen"),
        ylab = "Lebenserwartung (in Jahren)",
        xlab = "Merkmale", horizontal = FALSE, col = "lightblue")



# Differenz der Lebenserwartung (Frauen - Männer)
Diff_Lebenserwartung <- Life_Expectancy_Female - Life_Expectancy_Male
hist(Diff_Lebenserwartung, xlab = "Differenz der Lebenserwartung (in Jahren)",
     ylab = "Relative Häufigkeit",probability = TRUE,
     ylim = c(0, 0.30), breaks = -2:12, main = "")

# Histogramm für Fertilitätsrate
hist(Total_Fertility_Rate, xlab = "Fertilitätsrate", 
     ylab = "Relative Häufigkeit", probability = TRUE, main = "")

# Verteilung der Region mit Tabelle

table(Region)

# Verteilung der Subregion
table(Subregion)

################

#2 - bivariate Zusammenhaenge zwischen den Merkmalen

# Subregionen nach Region
table(Subregion, Region)

# Zusammenhang zwischen gesamten Lebenserwartung und Fertilitätsrate
# Streudiagramm
ggplot(data2022, aes(x = Life_Expectancy_Overall, y = Total_Fertility_Rate)) +
  geom_point() +
  labs(x = "Gesamte Lebenserwartung (in Jahren)",
       y = "Fertilitätsrate") +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Zusammenhang zwischen Lebenserwartung von Frauen und Männer
# Streudiagramm
ggplot(data2022, aes(x = Life_Expectancy_Female, y = Life_Expectancy_Male)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "solid") +
  labs(x = "Lebenserwartung der Frauen (in Jahren)",
       y = "Lebenserwartung der Männer (in Jahren)") +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14) 
  )

# Boxplot der Lebenserwartung nach Region
region_name = c("Afrika", "Amerika", "Asien", "Europa", "Ozeanien")

boxplot(Life_Expectancy_Overall ~ Region,
        ylab = "Gesamte Lebenserwartung (in Jahren)",
        xlab = "Region", horizontal = FALSE,
        names = region_name, col = "lightblue")

# Boxplot der Fertilitätsrate nach Region
boxplot(Total_Fertility_Rate ~ Region, 
        ylab = "Fertilitätsrate",
        xlab = "Region", horizontal = FALSE,
        col = "lightblue", names = region_name)


#3 - Unterschiede der Merkmale zwischen den Subregionen

# Subregionen ins Deutsche übersetzen und Region hinzufügen
data2022$Subregion[data2022$Subregion == "Eastern Africa"] <- "Ostafrika"
data2022$Subregion[data2022$Subregion == "Middle Africa"] <- "Zentralafrika"
data2022$Subregion[data2022$Subregion == "Northern Africa"] <- "Nordafrika"
data2022$Subregion[data2022$Subregion == "Southern Africa"] <- "Südafrika"
data2022$Subregion[data2022$Subregion == "Western Africa"] <- "Westafrika"
data2022$Subregion[data2022$Subregion == "Caribbean"] <- "Karibik"
data2022$Subregion[data2022$Subregion == "Central America"] <- "Mittelamerika"
data2022$Subregion[data2022$Subregion == "Northern America"] <- "Nordamerika"
data2022$Subregion[data2022$Subregion == "South America"] <- "Südamerika"
data2022$Subregion[data2022$Subregion == "Eastern Asia"] <- "Ostasien"
data2022$Subregion[data2022$Subregion == "South-Central Asia"] <- "Südzentralasien"
data2022$Subregion[data2022$Subregion == "South-Eastern Asia"] <- "Südostasien"
data2022$Subregion[data2022$Subregion == "Western Asia"] <- "Westasien"
data2022$Subregion[data2022$Subregion == "Eastern Europe"] <- "Osteuropa"
data2022$Subregion[data2022$Subregion == "Northern Europe"] <- "Nordeuropa"
data2022$Subregion[data2022$Subregion == "Southern Europe"] <- "Südeuropa"
data2022$Subregion[data2022$Subregion == "Western Europe"] <- "Westeuropa"
data2022$Subregion[data2022$Subregion == "Australia/New Zealand"] <- "Australien/Neuseeland"
data2022$Subregion[data2022$Subregion == "Melanesia"] <- "Melanesien"
data2022$Subregion[data2022$Subregion == "Micronesia"] <- "Mikronesien"
data2022$Subregion[data2022$Subregion == "Polynesia"] <- "Polynesien"

# Region hinzufügen basierend auf den Subregionen
data2022$Region <- ifelse(data2022$Subregion %in% c("Ostafrika", "Zentralafrika", "Nordafrika", "Südafrika", "Westafrika"), "Afrika",
                          ifelse(data2022$Subregion %in% c("Karibik", "Mittelamerika", "Nordamerika", "Südamerika"), "Amerika",
                                 ifelse(data2022$Subregion %in% c("Ostasien", "Südzentralasien", "Südostasien", "Westasien"), "Asien",
                                        ifelse(data2022$Subregion %in% c("Osteuropa", "Nordeuropa", "Südeuropa", "Westeuropa"), "Europa",
                                               ifelse(data2022$Subregion %in% c("Australien/Neuseeland", "Melanesien", "Mikronesien", "Polynesien"), "Ozeanien", 
                                                      NA)))))

# Sortieren der Daten nach Region und Subregion in neuer Variable year2022
year2022 <- data2022[order(Region, Subregion), ]


#########################
#Boxplots

# Gesamte Lebenserwartung in den Subregionen (2022)
ggplot(year2022, aes(x = factor(Subregion, levels = unique(Subregion)), y = Life_Expectancy_Overall, fill = Region)) +
  geom_boxplot(coef = 1.5, size = 0.6) +
  labs(x = "Subregion",
       y = "Gesamte Lebenserwartung (in Jahren)",
       fill = "Region:") +  # Legende für die Regionen hinzufügen
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14), 
    axis.title.y = element_text(size = 14, margin = margin(r = 10)), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 14),
    legend.position = "top",
    legend.direction = "horizontal")


# Fertilitätsrate in den Subregionen (2022)
ggplot(year2022, aes(x = factor(Subregion, levels = unique(Subregion)), y = Total_Fertility_Rate, fill = Region)) +
  geom_boxplot(coef = 1.5, size = 0.6) +
  labs(x = "Subregion",
       y = "Fertilitätsrate",
       fill = "Region:") +  # Legende für die Regionen hinzufügen
  theme(
    plot.title = element_text(size = 16, face = "bold"), 
    axis.title.x = element_text(size = 14), 
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),  
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 14),
    legend.position = "top",
    legend.direction = "horizontal")


#4 - Vergleich von Merkmalen in 2002 und 2022

detach(data2022)

# Scatterplot der Differenz der Lebenserwartung

# Differenz der Lebenserwartung (2022 - 2002) (Jahre)
data_2002 <- subset(data, Year == 2002)
data_2022 <- subset(data, Year == 2022)
lebenserwartung_differenz = na.omit(data_2022$Life_Expectancy_Overall - data_2002$Life_Expectancy_Overall)

par(mar = c(4.2, 4, 2, 1))
plot(
  data2002$Life_Expectancy_Overall, 
  lebenserwartung_differenz,
  xlab = "Lebenserwartung in 2002 (in Jahren)", 
  ylab = "Differenz der Lebenserwartung (in Jahren)", 
  main = "Veränderung der allgemeinen Lebenserwartung",
  pch = 1)

abline(h = 0, col = "red", lwd = 1, lty = 2) 

# Scatterplot der Differenz der Fertilitätsrate

# Differenz der Fertilitätsrate (2022 - 2002) (Jahre)
fertilitaetsrate_differenz = na.omit(data_2022$Total_Fertility_Rate - data_2002$Total_Fertility_Rate)

plot(
  data2002$Total_Fertility_Rate, 
  fertilitaetsrate_differenz,
  xlab = "Fertilitätsrate in 2002", 
  ylab = "Differenz der Fertilitätsrate", 
  main = "Veränderung der Fertilitätsrate",
  pch = 1)

abline(h = 0, col = "red", lwd = 1, lty = 2) 


#Boxplot für die Veränderung der allgemeinen Lebenserwartung (2022 - 2002)

data2002$Lebenserwartung_Differenz = lebenserwartung_differenz

#Regionnamen ins Deutsch
data2002$Region[data2002$Region == "Africa"] = "Afrika"
data2002$Region[data2002$Region == "Americas"] = "Amerika"
data2002$Region[data2002$Region == "Asia"] = "Asien"
data2002$Region[data2002$Region == "Europe"] = "Europa"
data2002$Region[data2002$Region == "Oceania"] = "Ozeanien"

par(mar = c(4.2, 4, 1, 1))

boxplot(data2002$Lebenserwartung_Differenz ~ data2002$Region,
        xlab = "Region", ylab = "Veränderung der Lebenserwartung", col = "lightblue")
abline(h = 0, col = "red", lwd = 1, lty = 2)


#Boxplot für die Veränderung der Fertilitätsrate (2022 - 2002)

data2002$Fertilitaetsrate_Differenz = fertilitaetsrate_differenz

boxplot(data2002$Fertilitaetsrate_Differenz ~ data2002$Region,
        xlab = "Region", ylab = "Veränderung der Fertilitätsrate", col = "lightblue")
abline(h = 0, col = "red", lwd = 1, lty = 2)
