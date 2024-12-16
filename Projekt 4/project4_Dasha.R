library(readxl)
library(ggplot2)
library(tidyr)

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