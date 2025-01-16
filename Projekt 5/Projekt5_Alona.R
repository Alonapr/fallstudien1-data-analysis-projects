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

df$Total_Area <- log(df$Total_Area)
print(df$Total_Area)
hist(df$Total_Area)

df$Population <- log(df$Population)
print(df$Population)
hist(df$Population)

df$Population_Density <- log(df$Population_Density)
print(df$Population_Density)
hist(df$Population_Density)

glm_model <- glm(Leading_Candidate ~ Total_Area + Population + 
                 + Median_Age + Birth_Rate + HDI + Unemployment_Rate + Health_Insurance_Coverage
                 + Median_Rent, family = binomial, data = df)
summary(glm_model)
plot(glm_model)

#Aufgabe 2 

# Schrittweise Selektion basierend auf AIC
stepwise_model <- step(glm_model, direction = "both", trace = 0)

# Zusammenfassung des reduzierten Modells
summary(stepwise_model)
plot(stepwise_model)

# Konfidenzintervalle der Koeffizienten
confint(stepwise_model)

# Schrittweise Selektion basierend auf BIC
n <- nrow(df)  # Anzahl der Datenpunkte
stepwise_model_bic <- step(glm_model, direction = "both", k = log(n), trace = 0)

# Zusammenfassung des BIC-Modells
summary(stepwise_model_bic)
plot(stepwise_model_bic)

# Konfidenzintervalle der Koeffizienten
confint(stepwise_model_bic)

#Aufgabe 3 - Modellenbewertung
# Vorhersagen für das Modell generieren

pred_prob1 <- predict(glm_model, type = "response")
pred_prob2 <- predict(stepwise_model, type = "response")

# ROC-Kurve berechnen
roc_curve1 <- roc(df$Leading_Candidate, pred_prob1)
roc_curve2 <- roc(df$Leading_Candidate, pred_prob2)

# AUC berechnen und ausgeben
auc1 <- auc(roc_curve1)
auc2 <- auc(roc_curve2)
print(paste("AUC:", auc1, auc2))

plot(roc_curve1, col = "blue", lwd = 2)
lines(roc_curve2, col = "red", lwd = 2)
legend("bottomright", legend = c(paste("Modell 1 (AUC =", round(auc1, 2), ")"),
                                 paste("Modell 2 (AUC =", round(auc2, 2), ")")),
       col = c("blue", "red"), lwd = 2)



# TrainControl für Kreuzvalidierung mit ROC und AUC
train_control <- trainControl(method = "cv",         
                              number = 10,           # 10-fache Kreuzvalidierung
                              classProbs = TRUE,     # Berechnung von Klassenwahrscheinlichkeiten
                              summaryFunction = twoClassSummary)  # Berechnung von ROC und AUC


# Kreuzvalidierung für das erste Modell (Originalmodell)
cv_model1 <- train(Leading_Candidate ~ Total_Area + Population + Population_Density + 
                     Median_Age + Birth_Rate + HDI + Unemployment_Rate + 
                     Health_Insurance_Coverage + Median_Rent, 
                   data = df, 
                   method = "glm", 
                   family = "binomial", 
                   trControl = train_control, 
                   metric = "ROC")




