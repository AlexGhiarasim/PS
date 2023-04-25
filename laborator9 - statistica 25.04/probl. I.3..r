life.exp = read.csv("life_expect.csv")
intervals_fem = seq(from = min(life.exp$female, na.rm = TRUE), 
    to = max(life.exp$female, na.rm = TRUE), 
    by = (max(life.exp$female, na.rm = TRUE) - min(life.exp$female, na.rm = TRUE)) / 7)
intervals_male = seq(from = min(life.exp$male, na.rm = TRUE), 
    to = max(life.exp$male, na.rm = TRUE), 
    by = (max(life.exp$male, na.rm = TRUE) - min(life.exp$male, na.rm = TRUE)) / 7)

par(mfrow=c(1,2))
hist(life.exp$female, breaks = intervals_fem, xlab = "Life Expectancy (Females)", main = "Histograms of Life Expectancy in EU")
hist(life.exp$male, breaks = intervals_male, xlab = "Life Expectancy (Males)", main = "Histograms of Life Expectancy in EU")


