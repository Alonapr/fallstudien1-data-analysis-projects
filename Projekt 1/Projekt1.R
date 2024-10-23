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

# Boxplots der Lebenserwartung (+ Unterschiede zwischen den Geschlechtern)
par(mar = c(5, 5, 1, 2))
boxplot(Life_Expectancy_Overall,
        Life_Expectancy_Male,
        Life_Expectancy_Female,
        names = c("Gesamt", "Männlich", "Weiblich"),
        ylab = "Lebenserwartung (in Jahren)",
        xlab = "Merkmale", horizontal = FALSE, col = "lightblue")
axis(2, at = seq(0, 100, by = 5))
means <- c(mean(Life_Expectancy_Overall),
           mean(Life_Expectancy_Male),
           mean(Life_Expectancy_Female))
segments(x0 = 1:3 - 0.4, y0 = means, x1 = 1:3 + 0.4, y1 = means, col = "red", lwd = 2)
legend("bottomright", legend = "mean", fill = "red", bty = "n")

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
  labs(x = "Gesamte Lebenserwartung",
       y = "Fertilitätsrate")

# Zusammenhang zwischen Lebenserwartung von Frauen und Männer
# Streudiagramm
ggplot(data2022, aes(x = Life_Expectancy_Female, y = Life_Expectancy_Male)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "solid") +
  labs(x = "Weibliche Lebenserwartung",
       y = "Männliche Lebenserwartung") +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12) 
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

# Gesamte Lebenserwartung in den Subregionen (2022)
ggplot(year2022, aes(x = factor(Subregion, levels = unique(Subregion)), y = Life_Expectancy_Overall, fill = Region)) +
  geom_boxplot(coef = 1.5, size = 0.6) +
  labs(x = "Subregion",
       y = "Gesamte Lebenserwartung",
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

detach(data2022)

#4 - Vergleich von Merkmalen in 2002 und 2022

par(mfrow = c(1, 2))
# Gesamte Lebenserwartung in 2002 und 2022
boxplot(data2002$Life_Expectancy_Overall, data2022$Life_Expectancy_Overall,
        names = c("2002", "2022"),
        ylab = "Gesamte Lebenserwartung (in Jahren)", 
        xlab = "Jahr", col = "lightblue")

# Fertilitätsrate in 2002 und 2022
boxplot(data2002$Total_Fertility_Rate, data2022$Total_Fertility_Rate,
        names = c("2002", "2022"), ylab = "Fertilitätsrate", 
        xlab = "Jahr", col = "lightblue")


