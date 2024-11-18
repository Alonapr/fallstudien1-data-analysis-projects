library(ggplot2)
library(tidyr)

df <- read.table("Kuckuckseier.txt", header=TRUE)
dim(df)
head(df)

par(mar = c(4.2, 4, 1, 1))

boxplot(df, main="", names=c("WP", "BP", "RK", "ZK"))

par(mfrow = c(2, 2))
hist(df$WP, main = "", xlab = "Länge der Eier von einem Kuckuck im Nester von Wiesenpieper")
hist(df$BP, main = "", xlab = "Länge der Eier von einem Kuckuck im Nester von Baumpieper")
hist(df$RK, main = "", xlab = "Länge der Eier von einem Kuckuck im Nester von Rotkehlchen")
hist(df$ZK, main = "", xlab = "Länge der Eier von einem Kuckuck im Nester von Zaunkönig")

par(mfrow = c(2, 2))

for (i in 1:4){
  qqnorm(df[[i]], main = "",  xlab = colnames(df)[i], ylab = "")
  qqline(df[[i]], col = "red", lwd = 2)
}

par(mfrow = c(1, 1))
 
df_long <- stack(df)
colnames(df_long) <- c("value", "species")

one.way <- aov(value ~ species, data = df_long)
summary(one.way)

car::leveneTest(value ~ species, data = df_long)
