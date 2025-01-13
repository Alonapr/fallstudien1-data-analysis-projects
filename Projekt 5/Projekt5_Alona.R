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

#Aufgabe 1 - die Zielvariable Leading_Candidate modellieren

# Harris = 0, Trump = 1 kodieren (da wir Logistische Regression verwenden, 
# muss die Zielvariable numerisch sein und als 0 und 1 kodiert werden)
df$Leading_Candidate <- ifelse(df$Leading_Candidate == "Harris", 0, 1)

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


# TrainControl für Kreuzvalidierung mit ROC und AUC
train_control <- trainControl(method = "cv",         
                              number = 10,           # 10-fache Kreuzvalidierung
                              classProbs = TRUE,     # Berechnung von Klassenwahrscheinlichkeiten
                              summaryFunction = twoClassSummary)  # Berechnung von ROC und AUC

# Das Modell trainieren (binomiale logistische Regression)
cv_model <- train(Leading_Candidate ~ Total_Area + Population + Population_Density + 
                    Median_Age + Birth_Rate + HDI + Unemployment_Rate + 
                    Health_Insurance_Coverage + Median_Rent, 
                  data = df, 
                  method = "glm", 
                  family = "binomial", 
                  trControl = train_control, 
                  metric = "ROC")  # ROC als Leistungsmaß

# Zusammenfassung der Ergebnisse der Kreuzvalidierung
print(cv_model)

# ROC-Kurve berechnen und plotten
pred_prob <- predict(cv_model, df, type = "prob")[,2]  # Wahrscheinlichkeiten für Klasse 1 (Trump)
roc_curve <- roc(df$Leading_Candidate, pred_prob)

# Plot der ROC-Kurve
plot(roc_curve, col = "blue", main = "ROC Curve")

# AUC berechnen und anzeigen
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))


