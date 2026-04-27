#Load Data
setwd("C:\\Users\\Lenovo\\Desktop\\TPSM Assigment\\movie-ratings-analysis\\scripts")

# Install packages
install.packages(c("tidyverse", "ggplot2", "corrplot", "caret", "psych"))

# Load libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(caret)
library(psych)


# Load the dataset
movies <- read.csv("C:\\Users\\Lenovo\\Desktop\\TPSM Assigment\\movie-ratings-analysis\\data\\raw\\IMDB.csv")

# Quick look
head(movies)
str(movies)
dim(movies)

# Data Preprocessing

# 1. Select only the columns need
movies_clean <- movies %>% select(name, genre, score, votes, runtime)

# 2. Check missing values
colSums(is.na(movies_clean))

# 3. Remove rows with missing ratings
movies_clean <- movies_clean %>% filter(!is.na(score) & !is.na(votes))

# 4. Check for duplicates
movies_clean <- movies_clean %>% distinct()

# 5. scores are numeric and within valid range (0–10)
movies_clean <- movies_clean %>% filter(score >= 0 & score <= 10)

# 6. remove outliers of score and votes
boxplot(movies_clean$score, main = "Boxplot for Scores")
boxplot(movies_clean$votes, main = "Boxplot for votes")

movies_clean <- movies_clean %>%
  filter(
    # Filter score: within 1.5 * IQR
    score >= (quantile(score, 0.25) - 1.5 * IQR(score)) & 
      score <= (quantile(score, 0.75) + 1.5 * IQR(score)),
    
    # Filter votes: within 1.5 * IQR
    votes >= (quantile(votes, 0.25) - 1.5 * IQR(votes)) & 
      votes <= (quantile(votes, 0.75) + 1.5 * IQR(votes))
  )


# 7. Final check
summary(movies_clean)
nrow(movies_clean)

#Descriptive Analysis

# Summary statistics
summary(movies_clean$score)
summary(movies_clean$votes)

# More detailed (mean, sd, skew, kurtosis)
psych::describe(movies_clean[, c("score", "votes")])

# Histogram of votes
ggplot(movies_clean, aes(x = votes)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Fan Ratings",
       x = "Fans Rating (votes)", y = "Frequency") +
  theme_minimal()

# Histogram of movie score
ggplot(movies_clean, aes(x = score)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  labs(title = "Distribution of Movie Ratings",
       x = "Movie Rating (score)", y = "Frequency") +
  theme_minimal()

# Boxplots side-by-side
movies_long <- movies_clean %>%
  select(votes, score) %>%
  pivot_longer(everything(), names_to = "Type", values_to = "Rating")

ggplot(movies_long, aes(x = Type, y = Rating, fill = Type)) +
  geom_boxplot() +
  labs(title = "Comparison of Fan vs Movie Ratings") +
  theme_minimal()

# Scatter plot — the most important chart for your hypothesis
ggplot(movies_clean, aes(x = votes, y = score)) +
  geom_point(alpha = 0.3, color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Fan Rating vs Movie Rating",
       x = "Fan Rating", y = "Movie Rating") +
  theme_minimal()

#Inferential Analysis

# Step 1: Check normality (to choose Pearson vs Spearman)
shapiro.test(sample(movies_clean$votes, 5000))   # Shapiro caps at 5000
shapiro.test(sample(movies_clean$score, 5000))

qqnorm(movies_clean$votes)
qqline(movies_clean$votes, col = "red")


