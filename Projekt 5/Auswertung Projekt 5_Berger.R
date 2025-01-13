###############################################################################

# Fallstudien I
# Projekt 5
# Katrin Berger
# Gruppe 3

###############################################################################

#setwd("C:/Studium/Fallstudien I/Projekt 5")
df <- read.csv("US_election_2024.csv", header = TRUE, sep = ";", dec = ",")
summary(df)

# Levels anpassen und Deskription
df$State <- as.factor(df$State)
print(df$State)

df$Leading_Candidate <- as.factor(df$Leading_Candidate)
levels(df$Leading_Candidate) <- c(0, 1)
print(df$Leading_Candidate)
table(df$Leading_Candidate)

df$Total_Area <- log(df$Total_Area)
print(df$Total_Area)
hist(df$Total_Area)

df$Population <- log(df$Population)
print(df$Population)
hist(df$Population)

df$Population_Density <- log(df$Population_Density)
print(df$Population_Density)
hist(df$Population_Density)

print(df$Median_Age)
hist(df$Median_Age)

print(df$Birth_Rate)
hist(df$Birth_Rate)

print(df$HDI)
hist(df$HDI)

print(df$Unemployment_Rate)
hist(df$Unemployment_Rate)

print(df$Health_Insurance_Coverage)
hist(df$Health_Insurance_Coverage)

print(df$Median_Rent)
hist(df$Median_Rent)

################################################################################

Modell <- glm(df$Leading_Candidate ~ df$Total_Area + df$Population
   + df$Median_Age + df$Birth_Rate + df$HDI + df$Unemployment_Rate + df$Median_Rent,
   family = binomial)
plot(Modell)


