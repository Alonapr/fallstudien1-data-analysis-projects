library(pROC)
library(caret)

setwd("C:/Users/alena/D/Universität/5Semester/Fallstudien 1/5")

df <- read.csv("US_election_2024.csv", sep = ";", dec = ",")
head(df)
str(df)

dim(df)
#51 11

summary(df)
#keine NA Werte

#Deskriptive Analyse

attach(df)

table(State)
#Eine Beobachung aus jedem State

table(Leading_Candidate)
# Harris  Trump 
# 20     31 

#Statistische Kennwerte für kardinal stetig skalierte Variablen
num_stat <- function(data) {
  var_name <- c("Population_Density", "Median_Age", "HDI", "Unemployment_Rate", 
                "Health_Insurance_Coverage")
  result <- matrix(NA, nrow = length(data), ncol = 8)
  colnames(result) <- c("Merkmal", "min", "q0.25", "q0.5", 
                        "Arith. Mittel", "q0.75", "max", "sd")
  for(i in 1:length(data)) {
    var_data <- data[[i]]
    
    q1 <- quantile(var_data, 0.25, type = 2)
    q3 <- quantile(var_data, 0.75, type = 2)
    
    result[i, 1] <- var_name[i]
    result[i, 2] <- sprintf("%.3f", min(var_data))
    result[i, 3] <- sprintf("%.3f", q1)
    result[i, 4] <- sprintf("%.3f", median(var_data))
    result[i, 5] <- sprintf("%.3f", mean(var_data))
    result[i, 6] <- sprintf("%.3f", q3)
    result[i, 7] <- sprintf("%.3f", max(var_data))
    result[i, 8] <- sprintf("%.3f", sd(var_data))
  } 
  return(as.data.frame(result))
}
num_stat(df[,c(5, 6, 8:10)])

#Säulendiagramme für Birth_Rate
barplot(table(Birth_Rate), main = "", ylab = "Anzahl", xlab = "Birth_Rate", 
        cex.axis = 1.1, cex.lab = 1.3) 

qqnorm(Population, xlab = "Theoretisches Quantil", ylab = "Empirisches Quantil",
       main = "GU Test")
qqline(Population, col = "red", lwd = 2)

#Aufgabe 1 - die Zielvariable Leading_Candidate modellieren

# Harris = 0, Trump = 1 kodieren (da wir Logistische Regression verwenden, 
# muss die Zielvariable numerisch sein und als 0 und 1 kodiert werden)
Leading_Candidate <- ifelse(Leading_Candidate == "Harris", 0, 1)

glm_model <- glm(Leading_Candidate ~ Total_Area + Population + Population_Density + 
                    Median_Age + Birth_Rate + HDI + Unemployment_Rate + 
                    Health_Insurance_Coverage + 
                    Median_Rent, family = binomial, data = df)
summary(glm_model)

#Aufgabe 3 - Modellenbewertung
# Vorhersagen für das Modell generieren
pred_prob <- predict(glm_model, type = "response")

# Manuell die Levels und die Richtung festlegen
roc_curve <- roc(df$Leading_Candidate, pred_prob, levels = c(0, 1), direction = "<")

# ROC-Kurve plotten
plot(roc_curve, col = "blue", main = "ROC Curve")

# AUC berechnen und ausgeben
auc(roc_curve)

