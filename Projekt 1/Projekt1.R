library(ggplot2)

data <- read.csv("census_2022_2002.csv")
# Ueberblick ueber die Daten
dim(data)
head(data)
str(data)
summary(data)

na_rows <- data[apply(is.na(data), 1, any), ]
print(na_rows)

# Entfernung von NA-Werten
data_noNA <- na.omit(data)

# Datensatz auf 2 abhaengig vom Jahr teilen
data2002 <- subset(data_noNA, Year == 2002)
dim(data2002)

data2022 <- subset(data_noNA, Year == 2022)
dim(data2022)


#1 - Haeufigkeitsverteilungen der Merkmale + Unterschiede zwischen Geschlechter
attach(data2022)

moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

num_stat <- function(data) {
  result <- matrix(NA, nrow = length(data), ncol = 9)
  colnames(result) <- c("variable", "min", "max", "q0.25", "q0.75","median", 
                        "moda", "mean", "var")
  for(i in 1:length(data)) {
    var_name <- names(data)[i]  
    var_data <- data[[i]]
    Max <- max(var_data)
    Min <- min(var_data)
    q1 <- quantile(var_data, 0.25)
    q3 <- quantile(var_data, 0.75)
    result[i, 1] <- var_name
    result[i, 2] <- round(Min, 2)
    result[i, 3] <- round(Max,2)
    result[i, 4] <- round(q1,2)
    result[i, 5] <- round(q3,2)
    result[i, 6] <- round(median(var_data),2)
    result[i, 7] <- round(moda(var_data),2)
    result[i, 8] <- round(mean(var_data),2)
    result[i, 9] <- round(var(var_data),2)
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
        xlab = "Lebenserwartung (in Jahren)", horizontal = TRUE)

# Unterschiede zwischen Geschlechter
Diff_Lebenserwartung <- Life_Expectancy_Female - Life_Expectancy_Male
hist(Diff_Lebenserwartung, xlab = "Weibliche - Männliche Lebenserwartung",
     ylab = "Häufigkeit",
     main = "Differenz der Lebenserwartung (Frauen - Männer)")

# Boxplot fuer gesamte Fertilitaet
boxplot(Total_Fertility_Rate,
        main = "Boxplot der Fertilitätsrate",
        xlab = "Fertilitätsrate",
        horizontal = TRUE)

# Verteilung der Region
freq <- table(Region)
prozent <- round((freq / sum(freq)) * 100)
region_name <- c("Afrika", "Amerika", "Asien", "Europa", "Ozeanien")

pie(freq, labels = NA, main = "Häufigkeitsverteilung der Region", 
    col = c("lightsalmon", "cornsilk", "lightgreen", "lightblue", "plum1"))

angles <- cumsum(freq) / sum(freq) * 2 * pi
text_positions <- angles - (freq / sum(freq) * pi)

text(x = cos(text_positions) * 0.5, y = sin(text_positions) * 0.5, 
     labels = paste(prozent, "%"), cex = 0.75)

legend("topright", legend = region_name, 
       fill = c("lightsalmon", "cornsilk", "lightgreen", "lightblue", "plum1"), 
       title = "Regionen",
       border = "black", bty = "o", cex = 0.7)   


# Verteilung der Subregion
table(Subregion)

ggplot(data.frame(Subregion), aes(x = Subregion)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Häufigkeitsverteilung der Subregionen",
       x = "Subregionen",
       y = "Häufigkeit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#2 - bivariate Zusammenhaenge zwischen den Merkmalen
# Subregionen nach Region
ggplot(data2022, aes(x = Subregion, fill = Region)) +
  geom_bar(position = "dodge") +
  labs(title = "Häufigkeitsverteilung der Subregionen nach Region",
       x = "Subregionen",
       y = "Häufigkeit",
       fill = "Region:") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# Streudiagramm der Lebenserwartung vs. Fertilitätsrate
ggplot(data2022, aes(x = Life_Expectancy_Overall, y = Total_Fertility_Rate)) +
  geom_point() +
  labs(title = "Zusammenhang zwischen Lebenserwartung und Fertilitätsrate",
       x = "Lebenserwartung",
       y = "Fertilitätsrate")

# Streudiagramm der Lebenserwartung von Maenner vs Frauen
ggplot(data2022, aes(x = Life_Expectancy_Female, y = Life_Expectancy_Male)) +
  geom_point() +
  labs(title = "Zusammenhang zwischen Lebenserwartung von Frauen und Männer",
       x = "Weibliche Lebenserwartung",
       y = "Männliche Lebenserwartung")

# Boxplot der Lebenserwartung nach Region
ggplot(data2022, aes(x = Region, y = Life_Expectancy_Overall)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot der Lebenserwartung nach Region",
       x = "Region",
       y = "Gesamte Lebenserwartung") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot der Fertilitaet nach Region
ggplot(data2022, aes(x = Region, y = Total_Fertility_Rate)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot der Fertilitätsrate nach Region",
       x = "Region",
       y = "Fertilitätsrate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3 - Unterschiede der Merkmale zwischen den Subregionen


#4 - Vergleich von Merkmalen in 2002 und 2022

