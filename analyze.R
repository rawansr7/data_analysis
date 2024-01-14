library("psych")

data <- read.csv("cleaned.csv")


# Some descriptive statistics
describe(data)

# Normality tests of some variables
normtest <- shapiro.test(data$stress)
print(paste("H0: Stress data is normally distributed. shapiro test p-value:", normtest$p.value))

normtest <- shapiro.test(data$spends)
print(paste("H0: Monthly spending data is normally distributed. shapiro test p-value:", normtest$p.value))

normtest <- shapiro.test(data$saves)
print(paste("H0: Monthly saving data is normally distributed. shapiro test p-value:", normtest$p.value))


# T-tests
ttest <- t.test(data$stress, mu = 28)
print(paste("H0: The mean stress is 28. one-sample t-test p-value:", ttest$p.value))

ttest <- t.test(subset(data, travelling == 1)$stress, subset(data, travelling == 0)$stress, alternative = "g")
print(paste("H0: The difference of stress means between travelling and non-travelling is zero. two-sample t-test p-value:", ttest$p.value))

ttest <- t.test(subset(data, gender == 0)$stress, subset(data, gender == 1)$stress)
print(paste("H0: The difference of stress means between males and females is zero. two-sample t-test p-value:", ttest$p.value))

# ANOVA
res <- aov(stress ~ news, data = data)
print(paste("H0: The difference of stress means among levels of watching news is zero. ANOVA p-value:", summary(res)[[1]]$`Pr(>F)`[1]))

res <- aov(stress ~ socialize, data = data)
print(paste("H0: The difference of stress means among levels of socializing is zero. ANOVA p-value:", summary(res)[[1]]$`Pr(>F)`[1]))

res <- aov(stress ~ education, data = data)
print(paste("H0: The difference of stress means among education levels is zero. ANOVA p-value:", summary(res)[[1]]$`Pr(>F)`[1]))

# Pearson's correlation
corr <- cor(data$spends, data$stress)
print(paste("Pearson's correlation coefficient between monthly spening and stress:", corr))

corr <- cor(data$saves, data$stress)
print(paste("Pearson's correlation coefficient between monthly saving and stress:", corr))

# Chi-square
tablo <- table(data$travelling, data$news)
chitest <- chisq.test(tablo)
print(paste("Chi-square test on relatedness of watching the news and planning to travel. p-value:", chitest$p.value))


# plots

# Plotting descriptive statistics of stress of each level of watching news
png("stress_and_news.png")
boxplot(stress ~ news, data = data, xlab = "Frequency of Watching News", ylab = "Stress Score", names = c("None", "Low", "Medium", "High"))
dev.off()

# Plotting QQ-diagram to visualize normality of stress scores
png("stress_qq_plot.png")
qqnorm(data$stress, main = "Normal Q-Q Plot of the Stress scores")
qqline(data$stress, col = 2)
dev.off()

# Pie diagram representing education levels in our sample
png("education_levels.png")
freq <- table(data$education)
pie(freq, labels = paste(c("Bachelor", "Master", "PhD"), "\n", sprintf("%.1f%%", freq / sum(freq) * 100)), main = "Sample Education Level Frequencies")
dev.off()

# Histogram of the total (money spending and money saving)
png("spends_and_saves_histo.png")
hist(data$spends + data$saves, xlab = "Monthly Saving + Spending (USD)", main = "Monthly Saving + Spending")
dev.off()



# The value of the correlation coefficient (r) lies between -1 to +1. When the value of –
# r=0; there is no relation between the variable.
# r=+1; perfectly positively correlated.
# r=-1; perfectly negatively correlated.
# r= 0 to 0.30; negligible correlation.
# r=0.30 to 0.50; moderate correlation.
# r=0.50 to 1 highly correlated.
