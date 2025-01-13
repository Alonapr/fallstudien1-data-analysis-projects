###############################################################################

# Fallstudien I
# Projekt 5
# Katrin Berger
# Gruppe 3

###############################################################################

#setwd("C:/Studium/Fallstudien I/Projekt 5")
df <- read.csv("US_election_2024.csv", header = TRUE, sep = ";", dec = ",")
summary(df)
attach(df)

# Levels anpassen und Deskription
State <- as.factor(State)
print(State)

Leading_Candidate <- as.factor(Leading_Candidate)
print(Leading_Candidate)
table(Leading_Candidate)

Total_Area <- log(Total_Area)
print(Total_Area)
hist(Total_Area)

Population <- log(Population)
print(Population)
hist(Population)

Population_Density <- log(Population_Density)
print(Population_Density)
hist(Population_Density)

print(Median_Age)
hist(Median_Age)

print(Birth_Rate)
hist(Birth_Rate)

print(HDI)
hist(HDI)

print(Unemployment_Rate)
hist(Unemployment_Rate)

print(Health_Insurance_Coverage)
hist(Health_Insurance_Coverage)

print(Median_Rent)
hist(Median_Rent)

################################################################################

Modell <- glm(Leading_Candidate ~ Total_Area + Population + Population_Density
   + Median_Age + Birth_Rate + HDI + Unemployment_Rate + Median_Rent,
   family = binomial)
plot(Modell)


