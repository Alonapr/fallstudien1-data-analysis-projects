library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tibble)

df <- read_excel("Medaillen.xlsx")
head(df)
summary(df)
df$Land <- factor(df$Land)
df$Sportart <- factor(df$Sportart)
 
# Deskriptive Statistiken
data_long <- df %>%
  pivot_longer(cols = NrGold:NrBronze,  
               names_to = "Medal_Type",  
               values_to = "Count")  


data_long$Land <- factor(data_long$Land)
data_long$Sportart <- factor(data_long$Sportart)
data_long$Medal_Type <- factor(data_long$Medal_Type)
summary(data_long)

levels(data_long$Medal_Type) <- c("Bronze", "Gold", "Silber")
data_long$Medal_Type <- factor(data_long$Medal_Type, levels = c("Gold", "Silber", "Bronze"))

# Medaillen pro Land
ggplot(data_long, aes(x = Land, y = Count, fill = Medal_Type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Medaillen pro Land",
    x = "Anzahl an Medaillen",
    fill = "Art der Medaille"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Gold" = "gold", 
                               "Silber" = "#C0C0C0", "Bronze" = "#cd7f32"))

# Medaillen pro Sportart
ggplot(data_long, aes(x = Sportart, y = Count, fill = Medal_Type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Medaillen pro Sportart",
    x = "Anzahl an Medaillen",
    fill = "Art der Medaille"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Gold" = "gold", 
                               "Silber" = "#C0C0C0", "Bronze" = "#cd7f32"))

# Gold Medaillen pro Land
ggplot(df, aes(x = Land, y = NrGold)) +
  geom_bar(stat = "identity", fill = "gold") +
  labs(
    title = "Gold Medaillen pro Land",
    x = "Anzahl an Medaillen",
    fill = "Art der Medaille"
  ) +
  theme_minimal()

# Gold Medaillen pro Land
ggplot(df, aes(x = Land, y = NrSilber)) +
  geom_bar(stat = "identity", fill = "#C0C0C0") +
  labs(
    title = "Gold Medaillen pro Land",
    x = "Anzahl an Medaillen",
    fill = "Art der Medaille"
  ) +
  theme_minimal()

# Bronze Medaillen pro Land
ggplot(df, aes(x = Land, y = NrBronze)) +
  geom_bar(stat = "identity", fill = "#cd7f32") +
  labs(
    title = "Gold Medaillen pro Land",
    x = "Anzahl an Medaillen",
    fill = "Art der Medaille"
  ) +
  theme_minimal()

contingency_table <- xtabs(Total ~ Land + Sportart, data = df)
chisq.test(contingency_table)

# Für den Chi-Quadrat-Test auf Unabhängigkeit sollte die erwartete Häufigkeit 
# in jeder Kategorie mindestens 5 betragen.

test <- chisq.test(contingency_table)
expected_frequencies <- test$expected
print(expected_frequencies)
# 
# # 2)
# for (sport in unique(data_long$Sportart)) {
#   daten_sport <- filter(data_long, Sportart == sport)
#   tabelle <- xtabs(Count ~ Land + Medal_Type, data = daten_sport)
#   chi_test <- chisq.test(tabelle)
#   
#   cat("\nSportart:", sport, "\n")
#   print(tabelle)
#   cat("Chi-Quadrat-Statistik:", chi_test$statistic, "\n")
#   cat("p-Wert:", chi_test$p.value, "\n")
# }
# 
# 
# for (sport in unique(data_long$Sportart)) {
# 
#   friedman_data <- data_long %>%
#     filter(Sportart == sport) %>%
#     select(Land, Medal_Type, Count) %>%
#     pivot_wider(names_from = Medal_Type, values_from = Count, values_fill = 0) %>%
#     column_to_rownames("Land") 
# 
#   if (nrow(friedman_data) > 1 && ncol(friedman_data) > 1) {
#     friedman_result <- friedman.test(as.matrix(friedman_data))
#     
#     cat("\nSportart:", sport, "\n")
#     print(friedman_result)
#   } else {
#     cat("\nSportart:", sport, "не имеет достаточно данных для теста.\n")
#   }
# }
# 
# 
# # 3)
# for (land in unique(data_long$Land)) {
#   daten_sport <- filter(data_long, Land == land)
#   tabelle <- xtabs(Count ~ Sportart + Medal_Type, data = daten_sport)
#   chi_test <- chisq.test(tabelle)
#   
#   cat("\nSportart:", sport, "\n")
#   print(tabelle)
#   cat("Chi-Quadrat-Statistik:", chi_test$statistic, "\n")
#   cat("p-Wert:", chi_test$p.value, "\n")
# }
# 
