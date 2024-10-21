# Bibliotheken laden
library(ggplot2)

# Durchschnittliche Lebenserwartung für 2002 und 2022 berechnen
avg_life_expectancy_2002 <- tapply(data2002$Life_Expectancy_Overall, data2002$Region, mean)
avg_life_expectancy_2022 <- tapply(data2022$Life_Expectancy_Overall, data2022$Region, mean)

# Kombiniere die Werte in einer Matrix für die Darstellung
avg_life_expectancy <- rbind(avg_life_expectancy_2002, avg_life_expectancy_2022)
colnames(avg_life_expectancy) <- c("Afrika", "Amerika", "Asien", "Europa", "Ozeanien")

# Balkendiagramm für den Vergleich der Lebenserwartung
barplot(avg_life_expectancy, beside = TRUE, col = c("lightblue", "lightgreen"), 
        main = "Vergleich der durchschnittlichen Lebenserwartung (2002 vs 2022)", 
        ylab = "Durchschnittliche Lebenserwartung", ylim = c(0, max(avg_life_expectancy) + 5))
legend("topright", legend = c("2002", "2022"), fill = c("lightblue", "lightgreen"))

########

# Durchschnittliche Fertilitätsrate für 2002 und 2022 berechnen
avg_fertility_rate_2002 <- tapply(data2002$Total_Fertility_Rate, data2002$Region, mean)
avg_fertility_rate_2022 <- tapply(data2022$Total_Fertility_Rate, data2022$Region, mean)

# Kombiniere die Werte in einer Matrix für die Darstellung
avg_fertility_rate <- rbind(avg_fertility_rate_2002, avg_fertility_rate_2022)
colnames(avg_fertility_rate) <- c("Afrika", "Amerika", "Asien", "Europa", "Ozeanien")

# Balkendiagramm für den Vergleich der Fertilitätsrate
barplot(avg_fertility_rate, beside = TRUE, col = c("lightpink", "lightyellow"), 
        main = "Vergleich der durchschnittlichen Fertilitätsrate (2002 vs 2022)", 
        ylab = "Durchschnittliche Fertilitätsrate", ylim = c(0, max(avg_fertility_rate) + 1))
legend("topright", legend = c("2002", "2022"), fill = c("lightpink", "lightyellow"))

###########

# Daten für die Jahre 2002 und 2022 vorbereiten
years <- c(2002, 2022)
regions <- c("Afrika", "Amerika", "Asien", "Europa", "Ozeanien")

# Durchschnittliche Lebenserwartung für jede Region für die Jahre 2002 und 2022 berechnen
avg_life_expectancy_2002 <- tapply(data2002$Life_Expectancy_Overall, data2002$Region, mean)
avg_life_expectancy_2022 <- tapply(data2022$Life_Expectancy_Overall, data2022$Region, mean)

# Kombiniere die Werte in einer Matrix für die Darstellung (jede Spalte entspricht einer Region)
life_expectancy_data <- cbind(avg_life_expectancy_2002, avg_life_expectancy_2022)

# Überprüfen, ob die Daten vollständig sind
print(life_expectancy_data)

# Grafikparameter anpassen, um Platz für die Legende rechts vom Plot zu schaffen
par(mar = c(4,4,2,10), xpd = TRUE)

# Linienplot erstellen (x-Achse sind die Jahre, y-Achse ist die Lebenserwartung)
matplot(years, t(life_expectancy_data), type = "b", pch = 19, lty = 1, 
        col = c("#619CFF", "#00BA38", "#F8766D", "#C77CFF", "#B79F00"),
        xlab = "Jahr", ylab = "Durchschnittliche Lebenserwartung",
        main = "Veränderung der gesamten Lebenserwartung in den Regionen (2002 vs. 2022)",
        xlim = c(2000, 2025), ylim = c(55, 80))

# Legende außerhalb des Plots platzieren (rechts)
legend("topright", inset = c(-0.46, 0), legend = regions, 
       col = c("#619CFF", "#00BA38", "#F8766D", "#C77CFF", "#B79F00"), 
       pch = 19, lty = 1, cex = 1, box.lty = 0)

#Speichern mit 830 x 500

#########

# Unterschiede der Lebenserwartung berechnen (2022 - 2002)
avg_life_expectancy_diff <- avg_life_expectancy_2022 - avg_life_expectancy_2002
avg_fertility_rate_diff <- avg_fertility_rate_2002 - avg_fertility_rate_2022

# Tabelle der Unterschiede
diff_data <- data.frame(Region = names(avg_life_expectancy_2002),
                        Lebenserwartung_Differenz = round(avg_life_expectancy_diff, 2),
                        Fertilitätsrate_Differenz = round(avg_fertility_rate_diff, 2))

# Tabelle anzeigen
print(diff_data)


#Scatterplot

# Scatterplot: Lebenserwartung 2002 vs. Lebenserwartung 2022

data2002 <- subset(data, Year == 2002)
dim(data2002)
data2002_noNA <- subset(data_noNA, Year == 2002)

data2022 <- subset(data, Year == 2022)
dim(data2022)
data2022_noNA <- subset(data_noNA, Year == 2022)

# Lebenserwartung in 2002 und 2022 extrahieren
life_2002 <- data2002$Life_Expectancy_Overall
life_2022 <- data2022$Life_Expectancy_Overall

# Region für jedes Land aus dem Jahr 2022 extrahieren
regions_2022 <- data2022$Region

# Farben für jede Region definieren
region_colors <- c("Africa" = "#619CFF", "Americas" = "#00BA38", 
                   "Asia" = "#F8766D", "Europe" = "#C77CFF", "Oceania" = "#B79F00")

# Scatterplot erstellen: Lebenserwartung 2002 vs Lebenserwartung 2022
plot(life_2002, life_2022,
     main = "Lebenserwartung 2002 vs 2022",
     xlab = "Lebenserwartung in 2002",
     ylab = "Lebenserwartung in 2022",
     pch = 19, xlim = c(45, 90), ylim = c(45, 90))

# Hinzufügen einer Linie y = x als Referenz (zeigt, wo es keine Veränderung gab)
abline(a = 0, b = 1, col = "red", lty = 2)
abline(v = mean(data2002_noNA$Life_Expectancy_Overall), col = "blue", lty = 2)
abline(h = mean(data2022_noNA$Life_Expectancy_Overall), col = "blue", lty = 2)

# Legende hinzufügen
legend("topleft", legend = names(region_colors), col = region_colors, pch = 19, cex = 0.8, box.lty = 0)

#####
#Stripchart

# Daten vorbereiten
life_2002 <- data2002$Life_Expectancy_Overall
life_2022 <- data2022$Life_Expectancy_Overall

# Kombiniere die Daten und füge eine Gruppenvariable für die Jahre hinzu
lebenserwartung <- c(life_2002, life_2022)
jahre <- factor(c(rep("2002", length(life_2002)), rep("2022", length(life_2022))))

# Stripchart erstellen
stripchart(lebenserwartung ~ jahre,
           main = "Verteilung der gesamten Lebenserwartung (2002 und 2022)",
           xlab = "Jahr",
           ylab = "Gesamte Lebenserwartung",
           vertical = TRUE,
           pch = 1,
           method = "stack",
           jitter = 0.1)

# Boxplots hinzufügen, um die zentralen Tendenzen zu zeigen
boxplot(lebenserwartung ~ jahre, add = TRUE, at = c(1, 2), col = NA, border = "darkgray", lty = 1, lwd = 1.5, outline = FALSE)

