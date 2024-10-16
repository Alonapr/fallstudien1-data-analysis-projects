setwd("C:/Users/Hanji/OneDrive/Studium/WS 24-25/Fallstudien/Projekt 1")
data = read.csv("census_2022_2002.csv")

na_rows <- data[apply(is.na(data), 1, any), ]
print(na_rows)

data_noNA = na.omit(data)

year2002 = data_noNA[which(data$Year == 2002), ]
print(year2002)

year2022 = data_noNA[which(data$Year == 2022), ]
print(year2022)

year = table(data_noNA$Year)

country = table(data_noNA$Country)

region = table(data_noNA$Region)

subregion = table(data_noNA$Subregion)

#Methoden zum Ausprobieren

summary(data_noNA$Life_Expectancy_Overall)

summary(data_noNA$Life_Expectancy_Male)

summary(data_noNA$Total_Fertility_Rate)

boxplot(data_noNA$Life_Expectancy_Overall)

boxplot(data_noNA$Life_Expectancy_Male)

boxplot(data_noNA$Life_Expectancy_Female)

boxplot(data_noNA$Total_Fertility_Rate)

hist(data_noNA$Life_Expectancy_Overall, probability = TRUE)

hist(data_noNA$Life_Expectancy_Male, probability = TRUE)

hist(data_noNA$Life_Expectancy_Female, probability = TRUE)

hist(data_noNA$Total_Fertility_Rate, probability = TRUE)

pie(table(data_noNA$Region))

################################
#Aufgabe 1

#Year Tabelle

#Country Tabelle

#Life_Expectancy_Overall Histogramm
hist(data_noNA$Life_Expectancy_Overall, probability = TRUE)

#Life_Expectancy_Male Histogramm
hist(data_noNA$Life_Expectancy_Male, probability = TRUE)

#Life_Expectancy_Female Histogramm
hist(data_noNA$Life_Expectancy_Female, probability = TRUE)

#Total_Fertility_Rate Histogramm
hist(data_noNA$Total_Fertility_Rate, probability = TRUE)

#Subregion Tabelle

#Region Pie-Diagramm

prozent = round((table(data_noNA$Region)/sum(table(data_noNA$Region)))*100)
region_name = c("Afrika", "Amerika", "Asien", "Europa", "Ozeanien")
beschriftung = paste(region_name, " ", prozent, "%", sep="")

plot.new()
plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
opar = par(mar = c(0, 0, 1.4, 0.4), lwd = 2, cex = 1.4, las = 1)

pie(table(data_noNA$Region), labels = beschriftung, main = "Häufigkeitsverteilung der Region", 
    col = c("lightsalmon", "cornsilk", "lightgreen", "lightblue", "plum1"), radius = 0.8)

par(opar)

#Aufgabe 2

#Year-Fertility
yf = data.frame("2002" = mean(year2002$Life_Expectancy_Overall), "2022" = mean(year2022$Life_Expectancy_Overall))
barplot(yf)