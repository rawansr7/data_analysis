library("psych")

all_data <- read.csv("all_data.csv")

# calculate stress scores
positive_questions <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7")
negative_questions <- c("N1", "N2", "N3", "N4", "N5", "N6", "N7")
stress_questions <- c(positive_questions, negative_questions)

all_data_with_score <- all_data
all_data_with_score$stress <- rowSums(all_data[, positive_questions]) + 7 * 4 - rowSums(all_data[, negative_questions])

# remove outliers
data_no_outliers <- all_data_with_score[which(
    scale(all_data_with_score$saves) > -3.29 & scale(all_data_with_score$saves) < 3.29 &
        scale(all_data_with_score$spends) > -3.29 & scale(all_data_with_score$spends) < 3.29 &
        scale(all_data_with_score$stress) > -3.29 & scale(all_data_with_score$stress) < 3.29
), ]


# alpha cronbach reliability of stress questions.
# The reliability of stress questions only is tested,
# since these 14 questions have the same scale.
# Alpha is 0.77.
stress_data <- data_no_outliers[names(data_no_outliers) %in% stress_questions]
alpha(stress_data, check.keys = TRUE)

# remove stress questions from the data
data <- data.frame(data_no_outliers[!(names(data_no_outliers) %in% stress_questions)])

# factor analysis on all the data without the stress questions
# (with merging the stress questions to a stress score).
# The p-value of the null-hypothesis test that 1 factor is enough
# is 0.211 > 0.05, which means there is 1 factor affecting the data.
factanal(data, factors = 5)

# descriptive statistics.
# Some observations:
# - The mean stress score is 27.32, which suggests that the sample has average stress. Average stress is the middle between 0 (minimum possible score) and 4*14 (maximum possible score)
# - The mean spending is ~ 282$ and the standard deviation is 222$. This data seems highly variable
# - The median for socializing is 1, which suggests the sample tends to be more non-socializing
# - The median for news is 2, which suggests that the sample tends to watch news in a balanced way
describe(data)

# Normality test for stress score. Given the p-value > 0.05, the data is normal.
shapiro.test(data$stress)

# Normality test for spending and saving amount. Given the p-value < 0.05, the data is not normal.
# However, given that our sample size is > 30, we can assume the data is normal and use tests designed
# for normal data.
shapiro.test(data$spends)
shapiro.test(data$saves)


# Using one-sample T test to test if the mean stress score is 28 ((max+min)/2)
# used T test since the stress data is normal, comparing mean of sample to
# the population mean (h0: hypothesizing that the population stress is average)
# The result is p-value > 0.05, so the sample mean is not significantly different from 28.
t.test(data$stress, mu = 28)

# Using independant two-sample T test to test if the mean stress is the same across
# the sample that wish to travel and the sample that doesn't.
# used this test since the stress data is normal, comparing the mean of two samples
# (hypothesizing that the stress is same across the two samples)
# The result is p-value < 0.05, so the two samples have different mean stress scores.
# This means that the students that want to travel has more stress score compared to
# those who don't want to travel.
t.test(subset(data, travelling == 1)$stress, subset(data, travelling == 0)$stress, alternative = "g")

# Using independant two-sample T test to test if the mean stress is the same across the two genders
# used this test since the stress data is normal, comparing the mean of two samples
# (hypothesizing that the stress is same across two genders)
# The result is p-value > 0.05, so the two genders have same stress.
t.test(subset(data, gender == 0)$stress, subset(data, gender == 1)$stress)

# Using ANOVA test to test if the mean stress is the same across the four levels of watching news
# used this test since the stress data is normal, comparing the mean of four samples (more than two)
# (hypothesizing that the stress is same across the four samples)
# The result is p-value > 0.05, so the watching the news doesn't seem to cause stress
summary(aov(stress ~ news, data = data))

# Using ANOVA test to test if the mean stress is the same across the five levels of socializing
# used this test since the stress data is normal, comparing the mean of five samples (more than two)
# (hypothesizing that the stress is same across the five samples)
# The result is p-value > 0.05, so socializing amount doesn't seem to affect stress levels
summary(aov(stress ~ socialize, data = data))

# Using ANOVA test to test if the mean stress is the same across students who study bachelors, masters, and PhD.
# used this test since the stress data is normal, comparing the mean of three samples (more than two)
# (hypothesizing that the stress is same across the three samples)
# The result is p-value > 0.05, so stress doesn't seem to be related to education level
summary(aov(stress ~ education, data = data))


# Using Pearson's correlation coefficient to test if the stress is correlated with how much a student spends monthly.
# used this test since the data is continuous and assumed to be normal
# The correlation coefficient is between -0.3 and 0.3 (negligible), so stress doesn't seem to be correlated to the amount of spending
cor.test(data$spends, data$stress, method = "pearson")

# Using Pearson's correlation coefficient to test if the stress is correlated with how much a student saves monthly.
# used this test since the data is continuous and assumed to be normal
# The correlation coefficient is between -0.3 and 0.3 (negligible), so stress doesn't seem to be correlated to the amount of saving
cor.test(data$saves, data$stress, method = "pearson")

# Using Chi Square test to test if the need to travel is corelated to how much the student is watching news.
# used this test since both variables are ordinal and not continuous.
# The result is p-value > 0.05, so there doesn't seem to be a relation between travelling and watching news.
tablo <- table(data$travelling, data$news)
chisq.test(tablo)

# plots

# Plotting descriptive statistics of stress of each level of watching news
boxplot(stress ~ news, data = data, xlab = "Frequency of Watching News", ylab = "Stress Score", names = c("None", "Low", "Medium", "High"))

# Plotting QQ-diagram to visualize normality of stress scores
qqnorm(data$stress, main = "Normal Q-Q Plot of the Stress scores")
qqline(data$stress, col = 2)

# Pie diagram representing education levels in our sample
freq <- table(data$education)
pie(freq, labels = paste(c("Bachelor", "Master", "PhD"), "\n", sprintf("%.1f%%", freq / sum(freq) * 100)), main = "Sample Education Level Frequencies")

# Histogram of the total money income (money spending + money saving)
hist(data$spends + data$saves, xlab = "Monthly Saving + Spending (USD)", main = "Monthly Saving + Spending")
