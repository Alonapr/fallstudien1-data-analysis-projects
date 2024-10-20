# Bibliotheken laden
library(ggplot2)

# Daten einlesen
setwd("C:/Users/Hanji/OneDrive/Studium/WS 24-25/Fallstudien/Projekt 1")
data = read.csv("census_2022_2002.csv")

# Daten filtern für das Jahr 2022 und fehlende Werte entfernen
na_rows <- data[apply(is.na(data), 1, any), ]
print(na_rows)

data_noNA = na.omit(data)

year2002 = subset(data_noNA, Year == 2002)

year2022 = subset(data_noNA, Year == 2002)

#Variablen überprüfen
year = table(data_noNA$Year)

country = table(data_noNA$Country)

region = table(data_noNA$Region)

subregion = table(data_noNA$Subregion)

################################
#Aufgabe 1

#Year Tabelle

#Country Tabelle

#Subregion Tabelle

#Region Pie-Diagramm

prozent = round((table(data_noNA$Region)/sum(table(data_noNA$Region)))*100)
region_name = c("Afrika", "Amerika", "Asien", "Europa", "Ozeanien")
beschriftung = paste(region_name, " ", prozent, "%", sep="")

plot.new()
plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
opar = par(mar = c(0, 0, 1.4, 0.4), lwd = 2, cex = 1.4, las = 1)

pie(table(data_noNA$Region), labels = beschriftung, main = "Häufigkeitsverteilung der Region", 
    col = c("lightsalmon", "cornsilk", "lightgreen", "lightblue", "plum1"), radius = 0.8)

par(opar)


#Aufgabe 3

# Subregionen ins Deutsche übersetzen und Region hinzufügen
year2022$Subregion[year2022$Subregion == "Eastern Africa"] <- "Ostafrika"
year2022$Subregion[year2022$Subregion == "Middle Africa"] <- "Zentralafrika"
year2022$Subregion[year2022$Subregion == "Northern Africa"] <- "Nordafrika"
year2022$Subregion[year2022$Subregion == "Southern Africa"] <- "Südafrika"
year2022$Subregion[year2022$Subregion == "Western Africa"] <- "Westafrika"
year2022$Subregion[year2022$Subregion == "Caribbean"] <- "Karibik"
year2022$Subregion[year2022$Subregion == "Central America"] <- "Mittelamerika"
year2022$Subregion[year2022$Subregion == "Northern America"] <- "Nordamerika"
year2022$Subregion[year2022$Subregion == "South America"] <- "Südamerika"
year2022$Subregion[year2022$Subregion == "Eastern Asia"] <- "Ostasien"
year2022$Subregion[year2022$Subregion == "South-Central Asia"] <- "Südzentralasien"
year2022$Subregion[year2022$Subregion == "South-Eastern Asia"] <- "Südostasien"
year2022$Subregion[year2022$Subregion == "Western Asia"] <- "Westasien"
year2022$Subregion[year2022$Subregion == "Eastern Europe"] <- "Osteuropa"
year2022$Subregion[year2022$Subregion == "Northern Europe"] <- "Nordeuropa"
year2022$Subregion[year2022$Subregion == "Southern Europe"] <- "Südeuropa"
year2022$Subregion[year2022$Subregion == "Western Europe"] <- "Westeuropa"
year2022$Subregion[year2022$Subregion == "Australia/New Zealand"] <- "Australien/Neuseeland"
year2022$Subregion[year2022$Subregion == "Melanesia"] <- "Melanesien"
year2022$Subregion[year2022$Subregion == "Micronesia"] <- "Mikronesien"
year2022$Subregion[year2022$Subregion == "Polynesia"] <- "Polynesien"

# Region hinzufügen basierend auf den Subregionen
year2022$Region <- ifelse(year2022$Subregion %in% c("Ostafrika", "Zentralafrika", "Nordafrika", "Südafrika", "Westafrika"), "Afrika",
                          ifelse(year2022$Subregion %in% c("Karibik", "Mittelamerika", "Nordamerika", "Südamerika"), "Amerika",
                                 ifelse(year2022$Subregion %in% c("Ostasien", "Südzentralasien", "Südostasien", "Westasien"), "Asien",
                                        ifelse(year2022$Subregion %in% c("Osteuropa", "Nordeuropa", "Südeuropa", "Westeuropa"), "Europa",
                                               ifelse(year2022$Subregion %in% c("Australien/Neuseeland", "Melanesien", "Mikronesien", "Polynesien"), "Ozeanien", 
                                                      NA)))))

# Sortieren der Daten nach Region und Subregion
year2022 <- year2022[order(year2022$Region, year2022$Subregion), ]


#########################
#Boxplots


#Boxplot Lebenserwartung der Männer in den Subregionen
# Boxplot mit ggplot2, nach Region gruppiert und mit farblicher Unterscheidung der Regionen
ggplot(year2022, aes(x = factor(Subregion, levels = unique(Subregion)), y = Life_Expectancy_Male, fill = Region)) +
  geom_boxplot(coef = 1.5, size = 0.6) +
  labs(title = "Lebenserwartung der Männer in den Subregionen (2022)",
       x = "Subregion",
       y = "Lebenserwartung",
       fill = "Region") +  # Legende für die Regionen hinzufügen
  theme(
    plot.title = element_text(size = 16, face = "bold"),  # Überschrift größer und fett
    axis.title.x = element_text(size = 14),  # x-Achsenbeschriftung größer
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # y-Achsenbeschriftung größer
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(size = 12),  # Legendentext größer
    legend.title = element_text(size = 14)  # Legendentitel größer
  )


# Boxplot Lebenserwartung der Frauen in den Subregionen
ggplot(year2022, aes(x = factor(Subregion, levels = unique(Subregion)), y = Life_Expectancy_Female, fill = Region)) +
  geom_boxplot(coef = 1.5, size = 0.6) +
  labs(title = "Lebenserwartung der Frauen in den Subregionen (2022)",
       x = "Subregion",
       y = "Lebenserwartung",
       fill = "Region") +  # Legende für die Regionen hinzufügen
  theme(
    plot.title = element_text(size = 16, face = "bold"),  # Überschrift größer und fett
    axis.title.x = element_text(size = 14),  # x-Achsenbeschriftung größer
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # y-Achsenbeschriftung größer mit Abstand
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(size = 12),  # Legendentext größer
    legend.title = element_text(size = 14)  # Legendentitel größer
  )


# Boxplot Fertilitätsrate in den Subregionen
ggplot(year2022, aes(x = factor(Subregion, levels = unique(Subregion)), y = Total_Fertility_Rate, fill = Region)) +
  geom_boxplot(coef = 1.5, size = 0.6) +
  labs(title = "Fertilitätsrate in den Subregionen (2022)",
       x = "Subregion",
       y = "Fertilitätsrate",
       fill = "Region") +  # Legende für die Regionen hinzufügen
  theme(
    plot.title = element_text(size = 16, face = "bold"),  # Überschrift größer und fett
    axis.title.x = element_text(size = 14),  # x-Achsenbeschriftung größer
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # y-Achsenbeschriftung größer mit Abstand
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(size = 12),  # Legendentext größer
    legend.title = element_text(size = 14)  # Legendentitel größer
  )



##################
##Boxplot mit Basic R Funktion, die X-Achse lassen sich nicht drehen, deshalb sieht sie nicht gut aus.
# plot.new()
# plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
# opar = par(mar = c(10, 4, 4, 1), lwd = 1.4, cex = 0.6, las = 1)
# 
# boxplot(Life_Expectancy_Male ~ Subregion, data = year2022,
#         main = "Lebenserwartung der Männer in den Subregionen (2022)",
#         xlab = "Subregion",
#         ylab = "Lebenserwartung",
#         las = 2,  # Labels auf der x-Achse drehbar machen
#         col = "lightblue")
# 
# par(opar)

