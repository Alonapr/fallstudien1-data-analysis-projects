library(ggplot2)

# Datensatzimport
df <- read.csv("census_2022_2002.csv")


# Ein kurzer Überblick über den Datensatz
head(df)
names(df)
# Variablen des Datensatzes:
# Year, Country, Life_Expectancy_Overall, Life_Expectancy_Male,   
# Life_Expectancy_Female, Total_Fertility_Rate, Subregion, Region


# Datenvorverarbeitung

# a) Behandlung fehlender Werte
colSums(is.na(df))
# Life_Expectancy_Overall, Life_Expectancy_Male, Life_Expectancy_Female, 
# Total_Fertility_Rate haben jeweils 7 fehlende Werte

# Es gibt 7 Zeilen im Datensatz, bei denen die Werte der 4 Variablen gleichzeitig fehlen:
df[which(is.na(df$Life_Expectancy_Overall)), ] 
# Diese Zeilen koennen leider nicht in der Analyse verwendet werden, 
# da zu viele Informationen fehlen. 

# Fehlende Werte werden aus dem Datensatz geloescht:
df <- df[!is.na(df$Life_Expectancy_Overall), ]

# b) Überprüfung der Datentypen
summary(df)

# Die Variablen Country, Region, Subregion sind von Typ "character".
# Fuer weitere Analysen ist es sinnvoll, sie in Faktorvariablen umzuwandeln:
df$Region <- as.factor(df$Region)
df$Subregion <- as.factor(df$Subregion)
df$Country <- as.factor(df$Country)

# Die Variable Year hat nur 2 Auspraefgungen, 2002 und 2022. Es macht Sinn, 
# sie auch in eine Faktorvariable umzuwandeln:
df$Year <- as.factor(df$Year)

# c) Suche nach Duplikaten
sum(duplicated(df)) # 0


# Aufgabe 1:
# Beschreiben Sie die Häufigkeitsverteilungen der Merkmale. 
# Betrachten Sie auch die Unterschiede zwischen den Geschlechtern.

# Fuer die Aufgaben 1-3 betrachten wir die Daten vom Jahr 2022
df_2022 <- df[df$Year == "2022", ]
df_2002 <- df[df$Year == "2002", ]

# Zentrale Tendenz

# a) Arithmetisches Mittel (fuer numerische Daten)
mean(df_2022$Life_Expectancy_Overall) # 74.38748
mean(df_2022$Life_Expectancy_Male) # 71.93876
mean(df_2022$Life_Expectancy_Female) # 76.96283
mean(df_2022$Total_Fertility_Rate) # 2.415132

# b) Median (fuer numerische Daten)
median(df_2022$Life_Expectancy_Overall) # 75.545
median(df_2022$Life_Expectancy_Male) # 72.785
median(df_2022$Life_Expectancy_Female) # 78.285
median(df_2022$Total_Fertility_Rate) # 1.9634

# c) Modus (fuer kategorielle Daten)

colours <- c("#A6E1C6", "#0DDBC5", "#199ABE", "#60DB7A", "#0D6EDB")
# Die Region, in der die meisten Länder befinden sich, ist Afrika:
table(df_2022$Region) 
barplot_1 <- barplot(table(df_2022$Region), 
        main = "Verteilung der Regionen", 
        xlab = "Regionen", ylab = "Häufigkeit",
        col = colours,
        ylim = c(0, max(table(df_2022$Region) + 2)))
text(x = barplot_1, y = table(df_2022$Region), 
     label = table(df_2022$Region), pos = 1, cex = 0.8, col = "black")

# Die Subregion, in der die meisten Länder befinden sich, ist Caribbean:
table(df_2022$Subregion) 


# Streuungsmasse

# a) Standardabweichung
sd(df_2022$Life_Expectancy_Overall) # 6.832433
sd(df_2022$Life_Expectancy_Male) # 6.692716
sd(df_2022$Life_Expectancy_Female) # 7.103228
sd(df_2022$Total_Fertility_Rate) # 1.115522

# b) Interquartilsabstand (IQR)
IQR(df_2022$Life_Expectancy_Overall) # 9.4575
IQR(df_2022$Life_Expectancy_Male) # 9.1525
IQR(df_2022$Life_Expectancy_Female) # 9.4875
IQR(df_2022$Total_Fertility_Rate) # 1.201325


# Visualisierung
par(mar = c(5, 4, 4, 2))
# 3 Histogramms fuer die Lebenserwartung (+ Unterschiede zwischen den Geschlechtern)

par(mfrow = c(1, 3))  # 1 Reihe, 3 Spalten

# Histogramm fuer die gesamte Lebenserwartung
hist(df_2022$Life_Expectancy_Overall, 
     main = "Lebenserwartung insgesamt", 
     xlab = "Jahre", 
     col = "#669AFA")

# Histogramm fuer die Lebenserwartung der Maenner
hist(df_2022$Life_Expectancy_Male, 
     main = "Lebenserwartung der Maenner", 
     xlab = "Jahre", 
     col = "#66F7FA")

# Histogramm fuer die Lebenserwartung der Frauen
hist(df_2022$Life_Expectancy_Female, 
     main = "Lebenserwartung der Frauen", 
     xlab = "Jahre", 
     col = "#66FACB")

# Rueckkehr zu den Standardeinstellungen
par(mfrow = c(1, 1))

# Histogramm fuer Total Fertility Rate
hist(df_2022$Total_Fertility_Rate, 
     main = "Total Fertility Rate", 
     xlab = "Kinder pro Frau", 
     col = "#669AFA")


# Box-Plots
# Box-Plot fuer Lebenserwartung (+ Unterschiede zwischen den Geschlechtern)
boxplot(df_2022$Life_Expectancy_Overall,
        df_2022$Life_Expectancy_Male,
        df_2022$Life_Expectancy_Female,
        names = c("Overall", "Male", "Female"),
        main = "Life Expectancy by Gender",
        xlab = "Categories",
        ylab = "Life Expectancy (years)")


# Box-Plot fuer Total_Fertility_Rate
boxplot(df_2022$Total_Fertility_Rate,
        main = "Total Fertility Rate",
        ylab = "Kinder pro Frau",
        outline = TRUE)

# Outliers

# a) Outliers von der Life Expectancy Overall Variable
Q1 <- quantile(df_2022$Life_Expectancy_Overall, 0.25)
Q3 <- quantile(df_2022$Life_Expectancy_Overall, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Outliers von der Variable Life_Expectancy_Overall
nrow(df_2022[df_2022$Life_Expectancy_Overall < lower_bound | 
               df_2022$Life_Expectancy_Overall > upper_bound, ]) 
# 3 Outliers: Afghanistan, Central African Republic, Somalia

# b) Outliers von der Total Fertility Rate Variable
Q1 <- quantile(df_2022$Total_Fertility_Rate, 0.25)
Q3 <- quantile(df_2022$Total_Fertility_Rate, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Outliers von der Variable Life_Expectancy_Overall
nrow(df_2022[df_2022$Total_Fertility_Rate < lower_bound | 
               df_2022$Total_Fertility_Rate > upper_bound, ]) 
# 13 Outliers: Alle Laender befinden sich in Afrika

# Verteilung der Regionen ((Pie chart))
pie(table(df_2022$Region), 
    main = "Verteilung der Regionen", 
    col = colours, 
    labels = paste(names(table(df_2022$Region)),  table(df_2022$Region), "%"))

# Aufgabe 2
# Gibt es bivariate Zusammenhänge zwischen den Merkmalen?

# Berechnung der Korrelation zwischen zwei Variablen
cor(df_2022$Life_Expectancy_Overall, df_2022$Total_Fertility_Rate) # -0.8087396
cor(df_2022$Life_Expectancy_Female, df_2022$Life_Expectancy_Male) # 0.9762885

# Scatterplot fuer zwei Variablen Total_Fertility_Rate & Life_Expectancy_Overall
plot(df_2022$Total_Fertility_Rate, df_2022$Life_Expectancy_Overall, 
     main = "Zusammenhang zwischen Lebenserwartung und Fruchtbarkeitsrate", 
     xlab = "Kinder pro Frau", 
     ylab = "Lebenserwartung insgesamt", 
     pch = 19, 
     col = colours[df_2022$Region])
legend("topright", legend = levels(df_2022$Region), col = colours, pch = 19)

# Scatterplot fuer zwei Variablen Life_Expectancy_Female & Life_Expectancy_Male
plot(df_2022$Life_Expectancy_Female, df_2022$Life_Expectancy_Male, 
     main = "Zusammenhang zwischen Lebenserwartung von Frauen und Maennern", 
     xlab = "Lebenserwartung von Frauen", 
     ylab = "Lebenserwartung von Maennern", 
     pch = 19, col = "#0DDB94")

# Mittelwerte der Lebenserwartung Overall nach Region
aggregated_data <- aggregate(df_2022$Life_Expectancy_Overall, 
                             by = list(df_2022$Region), FUN = mean)
colnames(aggregated_data) <- c("Region", "Life_Expectancy_Overall")

barplot_2 <- barplot(aggregated_data$Life_Expectancy_Overall, 
        names.arg = aggregated_data$Region, 
        col = colours,
        main = "Mittelwerte der Lebenserwartung nach Region",
        xlab = "Region",
        ylab = "Lebenserwartung Overall",
        ylim = c(0, max(aggregated_data$Life_Expectancy_Overall) + 20)) 
text(x = barplot_2, 
     y = aggregated_data$Life_Expectancy_Overall, 
     label = round(aggregated_data$Life_Expectancy_Overall, 2), 
     pos = 3, cex = 0.8, col = "black")

# Boxplots, Verteilung der Lebenserwartung nach Region
boxplot(Life_Expectancy_Overall ~ Region, data = df, 
        main = "Verteilung der Lebenserwartung nach Region", 
        ylab = "Lebenserwartung (Jahre)")

# Mittelwerte der Total_Fertility_Rate Overall nach Region
aggregated_data <- aggregate(df_2022$Total_Fertility_Rate, by = list(df_2022$Region), FUN = mean)
colnames(aggregated_data) <- c("Region", "Total_Fertility_Rate")

barplot_3 <- barplot(aggregated_data$Total_Fertility_Rate, 
        names.arg = aggregated_data$Region, 
        col = colours,
        main = "Mittelwerte der Total Fertility Rate nach Region",
        xlab = "Region",
        ylab = "Kinder pro Frau",
        ylim = c(0, max(aggregated_data$Total_Fertility_Rate) + 1)) 
text(x = barplot_3, 
     y = aggregated_data$Total_Fertility_Rate, 
     label = round(aggregated_data$Total_Fertility_Rate, 2), 
     pos = 3, cex = 0.8, col = "black")

# Boxplots, Verteilung der Total_Fertility_Rate nach Region
boxplot(Total_Fertility_Rate ~ Region, data = df, 
        main = "Verteilung der Total Fertility Rate nach Region", 
        ylab = "Kinder pro Frau")

# Aufgabe 3
# Wie unterscheiden sich die Merkmale zwischen den Subregionen?

# Durchschnittliche Lebenserwartung nach Subregionen
aggregated_life <- aggregate(df_2022$Life_Expectancy_Overall, 
                             by = list(df_2022$Subregion), FUN = mean)
colnames(aggregated_life) <- c("Subregion", "Life_Expectancy_Overall")

# Absteigende Reihenfolge:
aggregated_life <- aggregated_life[order(aggregated_life$Life_Expectancy_Overall, 
                                         decreasing = TRUE), ]

par(mar = c(5, 10, 4, 4))  # c(bottom, left, top, right)

barplot_4 <- barplot(aggregated_life$Life_Expectancy_Overall, 
        names.arg = aggregated_life$Subregion, 
        las = 2,  
        col = "#669AFA", 
        horiz = TRUE,  
        main = "Durchschnittliche Lebenserwartung nach Subregionen", 
        xlab = "Lebenserwartung (Jahre)")
text(x = aggregated_life$Life_Expectancy_Overall, 
     y = barplot_4, 
     label = round(aggregated_life$Life_Expectancy_Overall, 1),  
     pos = 2,  
     cex = 0.8, col = "white")


# Durchschnittliche Total_Fertility_Rate nach Subregionen
aggregated_life <- aggregate(df_2022$Total_Fertility_Rate, 
                             by = list(df_2022$Subregion), FUN = mean)
colnames(aggregated_life) <- c("Subregion", "Total_Fertility_Rate")

# Absteigende Reihenfolge:
aggregated_life <- aggregated_life[order(aggregated_life$Total_Fertility_Rate, 
                                         decreasing = TRUE), ]


barplot_4 <- barplot(aggregated_life$Total_Fertility_Rate, 
                     names.arg = aggregated_life$Subregion, 
                     las = 2,  
                     col = "#669AFA", 
                     horiz = TRUE,  
                     main = "Durchschnittliche Total Fertility Rate nach Subregionen", 
                     xlab = "Lebenserwartung (Jahre)")
text(x = aggregated_life$Total_Fertility_Rate, 
     y = barplot_4, 
     label = round(aggregated_life$Total_Fertility_Rate, 1),  
     pos = 2,  
     cex = 0.8, col = "white")

par(mar = c(5, 4, 4, 2))


# Aufgabe 4
# Wie haben sich die Merkmale in den letzten 20 Jahren entwickelt?

# Lebenserwartung und Total Fertility Rate (2002 und 2022)
plot(df$Total_Fertility_Rate, df$Life_Expectancy_Overall, 
     main = "Zusammenhang zwischen Lebenserwartung und Total Fertility Rate (2002 und 2022)", 
     xlab = "Kinder pro Frau", 
     ylab = "Lebenserwartung insgesamt", 
     pch = 19, 
     col = c("#0D6EDB", "#60DB7A")[df$Year])
legend("topright", legend = levels(df$Year), col = c("#0D6EDB", "#60DB7A"), pch = 19)

# Mittelwerte der Life Expectancy Overall nach Region (2002 und 2022)
# Daten nach Region und Jahr aggregieren
aggregated_data <- aggregate(df$Life_Expectancy_Overall, 
                             by = list(df$Region, df$Year), FUN = mean)
colnames(aggregated_data) <- c("Region", "Year", "Life_Expectancy_Overall")

# Daten in breites Format umwandeln
wide_data <- reshape(aggregated_data, idvar = "Region", timevar = "Year", 
                     direction = "wide")
colnames(wide_data) <- c("Region", "Life_Expectancy_Overall_2002", 
                         "Life_Expectancy_Overall_2022")

barplot_heights <- rbind(wide_data$Life_Expectancy_Overall_2002, wide_data$Life_Expectancy_Overall_2022)

barplot_3 <- barplot(barplot_heights, 
                     beside = TRUE,  # Balken nebeneinander
                     names.arg = wide_data$Region, 
                     col = c("#0D6EDB", "#60DB7A"), 
                     main = "Mittelwerte der Life Expectancy Overall nach Region (2002 und 2022)",
                     xlab = "Region", 
                     ylab = "Kinder pro Frau", 
                     ylim = c(0, max(barplot_heights, na.rm = TRUE) + 1))
text(x = barplot_3, 
     y = barplot_heights, 
     label = round(barplot_heights, 2), 
     pos = 3, cex = 0.8, col = "black")
legend("bottomright", legend = c("2002", "2022"), fill = c("#0D6EDB", "#60DB7A"))


# Mittelwerte der Total Fertility Rate nach Region (2002 und 2022)
# Daten nach Region und Jahr aggregieren
aggregated_data <- aggregate(df$Total_Fertility_Rate, 
                             by = list(df$Region, df$Year), FUN = mean)
colnames(aggregated_data) <- c("Region", "Year", "Total_Fertility_Rate")

# Daten in breites Format umwandeln
wide_data <- reshape(aggregated_data, idvar = "Region", 
                     timevar = "Year", direction = "wide")
colnames(wide_data) <- c("Region", "Fertility_2002", "Fertility_2022")

barplot_heights <- rbind(wide_data$Fertility_2002, wide_data$Fertility_2022)

barplot_3 <- barplot(barplot_heights, 
                     beside = TRUE,  # Balken nebeneinander
                     names.arg = wide_data$Region, 
                     col = c("#0D6EDB", "#60DB7A"), 
                     main = "Mittelwerte der Total Fertility Rate nach Region (2002 und 2022)",
                     xlab = "Region", 
                     ylab = "Kinder pro Frau", 
                     ylim = c(0, max(barplot_heights, na.rm = TRUE) + 1))
text(x = barplot_3, 
     y = barplot_heights, 
     label = round(barplot_heights, 2), 
     pos = 3, cex = 0.8, col = "black")
legend("topright", legend = c("2002", "2022"), fill = c("#0D6EDB", "#60DB7A"))
