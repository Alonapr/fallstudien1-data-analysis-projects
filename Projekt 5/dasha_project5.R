# Daten laden
data <- read.csv("US_election_2024.csv", sep = ";")

# Keine fehlende Werte
anyNA(data) 

summary(data)
str(data)

data <- data[, -1]

# Aufgabe 1

# In numerischen Datentyp einige Spalten umwandeln
data$Population_Density <- as.numeric(gsub(",", ".", data$Population_Density))
data$Median_Age <- as.numeric(gsub(",", ".", data$Median_Age))
data$HDI <- as.numeric(gsub(",", ".", data$HDI))
data$Unemployment_Rate <- as.numeric(gsub(",", ".", data$Unemployment_Rate))
data$Health_Insurance_Coverage <- as.numeric(gsub(",", ".", data$Health_Insurance_Coverage))

# In Faktor die Variable Leading_Candidate umwandeln
data$Leading_Candidate <- as.factor(data$Leading_Candidate)  

# Harris = 0, Trump = 1 kodieren (da wir Logistische Regression verwenden, 
# muss die Zielvariable numerisch sein und als 0 und 1 kodiert werden)
levels(data$Leading_Candidate) <- c(0, 1)  

# Prädiktoren und Zielvariable trennen
X <- model.matrix(Leading_Candidate ~ . - 1, data = data) 
y <- as.numeric(data$Leading_Candidate)



