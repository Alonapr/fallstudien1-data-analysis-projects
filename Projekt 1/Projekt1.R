library(ggplot2)

setwd("C:/Users/alena/Documents/GitHub/Fallstudien-1/Projekt 1")

data <- read.csv("census_2022_2002.csv")
# Ueberblick ueber die Daten
dim(data)
head(data)
str(data)
summary(data)

# Entfernung von NA-Werten
data_noNA <- na.omit(data)

# Datensatz auf 2 abhaengig vom Jahr teilen
data2002 <- subset(data_noNA, Year == 2002)
dim(data2002)

data2022 <- subset(data_noNA, Year == 2022)
dim(data2022)


#1 - Haeufigkeitsverteilungen der Merkmale + Unterschiede zwischen Geschlechter
attach(data2022)

num_stat <- function(data) {
  var_name <- c("Gesamte Lebenserwartung", "Männliche Lebenserwartung", 
                "Weibliche Lebenserwartung", "Fertilitätsrate")
  result <- matrix(NA, nrow = length(data), ncol = 7)
  colnames(result) <- c("Variable", "min", "max", "IQR", "median", 
                        "mean", "sd")
  for(i in 1:length(data)) {
    var_data <- data[[i]]
    Max <- max(var_data)
    Min <- min(var_data)
    q1 <- quantile(var_data, 0.25)
    q3 <- quantile(var_data, 0.75)
    result[i, 1] <- var_name[i]
    result[i, 2] <- round(Min, 2)
    result[i, 3] <- round(Max,2)
    result[i, 4] <- round(q3 - q1,2)
    result[i, 5] <- round(median(var_data),2)
    result[i, 6] <- round(mean(var_data),2)
    result[i, 7] <- round(sd(var_data),2)
  } 
  return(as.data.frame(result))
}
num_data <- 3:6
num_stat(data2022[,num_data])

# Boxplot fuer Lebenserwartung (+ Unterschiede zwischen den Geschlechtern)
boxplot(Life_Expectancy_Overall,
        Life_Expectancy_Male,
        Life_Expectancy_Female,
        names = c("Gesamt", "Männlich", "Weiblich"),
        main = "Boxplots der Lebenserwartung",
        ylab = "Merkmale",
        xlab = "Lebenserwartung (in Jahren)", horizontal = TRUE, col = "lightblue")

# Unterschiede zwischen Geschlechter
Diff_Lebenserwartung <- Life_Expectancy_Female - Life_Expectancy_Male
hist(Diff_Lebenserwartung, xlab = "Differenz der Lebenserwartung (in Jahren)",
     ylab = "Relative Häufigkeit",
     main = "Differenz der Lebenserwartung (Frauen - Männer)", probability = TRUE,
     ylim = c(0, 0.25))

# Boxplot fuer gesamte Fertilitaet
hist(Total_Fertility_Rate, main = "Histogramm für Fertilitätsrate",
        xlab = "Fertilitätsrate", ylab = "Relative Häufigkeit", 
     probability = TRUE)

# Verteilung der Region
freq <- table(Region)
prozent <- round((freq / sum(freq)) * 100)
region_name <- c("Afrika", "Amerika", "Asien", "Europa", "Ozeanien")
beschriftung = paste(region_name, " ")

pie(freq, labels = beschriftung, main = "Häufigkeitsverteilung der Region", 
    col = c("lightsalmon", "cornsilk", "lightgreen", "lightblue", "plum1"))

angles <- cumsum(freq) / sum(freq) * 2 * pi
text_positions <- angles - (freq / sum(freq) * pi)

text(x = cos(text_positions) * 0.5, y = sin(text_positions) * 0.5, 
     labels = paste(prozent, "%"), cex = 0.75)

# Verteilung der Subregion
table(Subregion)

#2 - bivariate Zusammenhaenge zwischen den Merkmalen
# Subregionen nach Region
table(Subregion, Region)

# Streudiagramm der Lebenserwartung vs. Fertilitätsrate
ggplot(data2022, aes(x = Life_Expectancy_Overall, y = Total_Fertility_Rate)) +
  geom_point() +
  labs(title = "Zusammenhang zwischen Lebenserwartung und Fertilitätsrate",
       x = "Gesamte Lebenserwartung",
       y = "Fertilitätsrate")

# Streudiagramm der Lebenserwartung von Maenner vs Frauen
ggplot(data2022, aes(x = Life_Expectancy_Female, y = Life_Expectancy_Male)) +
  geom_point() +
  labs(title = "Zusammenhang zwischen Lebenserwartung von Frauen und Männer",
       x = "Weibliche Lebenserwartung",
       y = "Männliche Lebenserwartung")

# Boxplot der Lebenserwartung nach Region
boxplot(Life_Expectancy_Overall ~ Region,
        main = "Boxplot der Lebenserwartung nach Region",
        ylab = "Region",
        xlab = "Gesamte Lebenserwartung", horizontal = TRUE,
        names = region_name, col = "lightblue")

# Boxplot der Fertilitaet nach Region
boxplot(Total_Fertility_Rate ~ Region,
        main = "Boxplot der Fertilitätsrate nach Region",
        ylab = "Region",
        xlab = "Fertilitätsrate", horizontal = TRUE,
        col = "lightblue", names = region_name)


#3 - Unterschiede der Merkmale zwischen den Subregionen

# Subregionen ins Deutsche übersetzen und Region hinzufügen
Subregion[Subregion == "Eastern Africa"] <- "Ostafrika"
Subregion[Subregion == "Middle Africa"] <- "Zentralafrika"
Subregion[Subregion == "Northern Africa"] <- "Nordafrika"
Subregion[Subregion == "Southern Africa"] <- "Südafrika"
Subregion[Subregion == "Western Africa"] <- "Westafrika"
Subregion[Subregion == "Caribbean"] <- "Karibik"
Subregion[Subregion == "Central America"] <- "Mittelamerika"
Subregion[Subregion == "Northern America"] <- "Nordamerika"
Subregion[Subregion == "South America"] <- "Südamerika"
Subregion[Subregion == "Eastern Asia"] <- "Ostasien"
Subregion[Subregion == "South-Central Asia"] <- "Südzentralasien"
Subregion[Subregion == "South-Eastern Asia"] <- "Südostasien"
Subregion[Subregion == "Western Asia"] <- "Westasien"
Subregion[Subregion == "Eastern Europe"] <- "Osteuropa"
Subregion[Subregion == "Northern Europe"] <- "Nordeuropa"
Subregion[Subregion == "Southern Europe"] <- "Südeuropa"
Subregion[Subregion == "Western Europe"] <- "Westeuropa"
Subregion[Subregion == "Australia/New Zealand"] <- "Australien/Neuseeland"
Subregion[Subregion == "Melanesia"] <- "Melanesien"
Subregion[Subregion == "Micronesia"] <- "Mikronesien"
Subregion[Subregion == "Polynesia"] <- "Polynesien"

# Region hinzufügen basierend auf den Subregionen
Region <- ifelse(Subregion %in% c("Ostafrika", "Zentralafrika", "Nordafrika", "Südafrika", "Westafrika"), "Afrika",
                          ifelse(Subregion %in% c("Karibik", "Mittelamerika", "Nordamerika", "Südamerika"), "Amerika",
                                 ifelse(Subregion %in% c("Ostasien", "Südzentralasien", "Südostasien", "Westasien"), "Asien",
                                        ifelse(Subregion %in% c("Osteuropa", "Nordeuropa", "Südeuropa", "Westeuropa"), "Europa",
                                               ifelse(Subregion %in% c("Australien/Neuseeland", "Melanesien", "Mikronesien", "Polynesien"), "Ozeanien", 
                                                      NA)))))

# Sortieren der Daten nach Region und Subregion in neuer Variable year2022
year2022 <- data2022[order(Region, Subregion), ]


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
    axis.text.x = element_text(angle = 45, hjust = 1), # x-Achsenbeschriftung um 45 Grad drehen
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
    axis.text.x = element_text(angle = 45, hjust = 1), # x-Achsenbeschriftung um 45 Grad drehen
    legend.text = element_text(size = 12),  # Legendentext größer
    legend.title = element_text(size = 14)  # Legendentitel größer
  )

# Boxplot Gesamte Lebenserwartung in den Subregionen
ggplot(year2022, aes(x = factor(Subregion, levels = unique(Subregion)), y = Life_Expectancy_Overall, fill = Region)) +
  geom_boxplot(coef = 1.5, size = 0.6) +
  labs(title = "Gesamte Lebenserwartung in den Subregionen (2022)",
       x = "Subregion",
       y = "Lebenserwartung",
       fill = "Region") +  # Legende für die Regionen hinzufügen
  theme(
    plot.title = element_text(size = 16, face = "bold"),  # Überschrift größer und fett
    axis.title.x = element_text(size = 14),  # x-Achsenbeschriftung größer
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # y-Achsenbeschriftung größer mit Abstand
    axis.text.x = element_text(angle = 45, hjust = 1), # x-Achsenbeschriftung um 45 Grad drehen
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
    axis.text.x = element_text(angle = 45, hjust = 1), # x-Achsenbeschriftung um 45 Grad drehen
    legend.text = element_text(size = 12),  # Legendentext größer
    legend.title = element_text(size = 14)  # Legendentitel größer
  )

#####################
#Zusätzliche Grafik

# Boxplot Differenz der Lebenserwartung (Frauen - Männer) in den Subregionen
ggplot(year2022, aes(x = factor(Subregion, levels = unique(Subregion)), y = Life_Expectancy_Female - Life_Expectancy_Male, fill = Region)) +
  geom_boxplot(coef = 1.5, size = 0.6) +
  labs(title = "Differenz der Lebenserwartung in den Subregionen (2022)",
       x = "Subregion",
       y = "Lebenserwartung",
       fill = "Region") +  # Legende für die Regionen hinzufügen
  theme(
    plot.title = element_text(size = 16, face = "bold"),  # Überschrift größer und fett
    axis.title.x = element_text(size = 14),  # x-Achsenbeschriftung größer
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # y-Achsenbeschriftung größer mit Abstand
    axis.text.x = element_text(angle = 45, hjust = 1), # x-Achsenbeschriftung um 45 Grad drehen
    legend.text = element_text(size = 12),  # Legendentext größer
    legend.title = element_text(size = 14)  # Legendentitel größer
  )


#4 - Vergleich von Merkmalen in 2002 und 2022

detach(data2022)
