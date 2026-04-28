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
movies_clean <- movies %>% select(name, genre, score, votes, runtime, year)
names(movies_clean)[names(movies_clean) == "Yaer"] <- "year"
# 2. Check missing values
colSums(is.na(movies_clean))

# 3. Remove rows with missing ratings
movies_clean <- na.omit(movies_clean)
##movies_clean <- movies_clean %>% filter(!is.na(score) & !is.na(votes))

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

# 1. Prepare the data
genre_summary <- movies_clean %>%
  group_by(genre) %>%                       # Group by the text column
  summarise(Total_Votes = sum(votes)) %>%   # Sum the numeric column
  arrange(desc(Total_Votes))                # Sort from highest to lowest

# 2. Create the plot
ggplot(genre_summary, aes(x = reorder(genre, -Total_Votes), y = Total_Votes, fill = genre)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Audience Engagement by Genre",
    x = "Genre",
    y = "Total Votes"
  ) +
  # This rotates the labels so they don't overlap
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 1. Calculate the average score for each genre
genre_scores <- movies_clean %>%
  group_by(genre) %>%
  summarise(Avg_Score = mean(score, na.rm = TRUE)) %>%
  arrange(desc(Avg_Score))

# 2. Plot the ranking
ggplot(genre_scores, aes(x = reorder(genre, -Avg_Score), y = Avg_Score, fill = genre)) +
  geom_col() +
  # Since scores are 0-10, we zoom in to see the differences better
  coord_cartesian(ylim = c(min(genre_scores$Avg_Score) - 1, max(genre_scores$Avg_Score) + 0.5)) +
  theme_minimal() +
  labs(title = "Which Genre has the Highest Average Score?",
       x = "Genre",
       y = "Average IMDb Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")



#Inferential Analysis

# Step 1: Check normality (to choose Pearson vs Spearman)
shapiro.test(sample(movies_clean$votes, 5000))   # Shapiro caps at 5000
shapiro.test(sample(movies_clean$score, 5000))

qqnorm(movies_clean$votes)
qqline(movies_clean$votes, col = "red") #not normal

qqnorm(movies_clean$score)
qqline(movies_clean$score, col = "yellow") #almost normal

#Spearman correlation was chosen because the data is not normally distributed,
#so a non-parametric method is more appropriate to test the relationship between the variables.
cor.test(movies_clean$votes, movies_clean$score, method = "spearman")

#Reject H₀.
#There is a significant positive relationship between fan ratings (votes) and movie ratings (score).

#Pradictive model
#Step 1 — Confirm the skew visually


# Before transformation
ggplot(movies_clean, aes(x = votes)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Votes — Raw (Heavily Right-Skewed)",
       x = "Number of Votes", y = "Frequency") +
  theme_minimal()

# After log transformation
ggplot(movies_clean, aes(x = log10(votes))) +
  geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
  labs(title = "Votes — Log10 Transformed (Approximately Normal)",
       x = "log10(Votes)", y = "Frequency") +
  theme_minimal()

#Step 2 — Create the transformed variable
# Use log1p (= log(1+x)) to safely handle any zero-vote rows
movies_clean$log_votes <- log1p(movies_clean$votes)

# Quick check
summary(movies_clean$log_votes)

#Step 3 — Re-check normality and correlation

# Shapiro on transformed votes
safe_shapiro <- function(x) {
  x <- x[!is.na(x)]
  shapiro.test(sample(x, min(5000, length(x))))
}

safe_shapiro(movies_clean$log_votes)   # W should now be much closer to 1

# Correlation tests
cor.test(movies_clean$log_votes, movies_clean$score, method = "pearson")
cor.test(movies_clean$log_votes, movies_clean$score, method = "spearman")

#Step 4 — Build the predictive model

set.seed(123)
train_index <- createDataPartition(movies_clean$score, p = 0.8, list = FALSE)
train_data  <- movies_clean[train_index, ]
test_data   <- movies_clean[-train_index, ]

##########################################
# Model 1: Simple — score predicted by log(votes) only
model1 <- lm(score ~ log_votes, data = train_data)
summary(model1)

# Model 2: Multiple regression — add other predictors if available
# (e.g. runtime, year, genre)
model2 <- lm(score ~ log_votes + runtime, data = train_data)
summary(model2)

# Compare the two models
anova(model1, model2)

#########################################
train_data_clean <- na.omit(train_data)

model1 <- lm(score ~ log_votes, data = train_data_clean)
model2 <- lm(score ~ log_votes + runtime, data = train_data_clean)

anova(model1, model2)

#Step 5 — Evaluate on the test set

predictions <- predict(model1, newdata = test_data)
actual <- test_data$score

RMSE <- sqrt(mean((predictions - actual)^2))
MAE  <- mean(abs(predictions - actual))
R2   <- cor(predictions, actual)^2

cat("RMSE:", round(RMSE, 3),
    "\nMAE :", round(MAE, 3),
    "\nR²  :", round(R2, 3))


#Step 6 — Visualize predictions
results <- data.frame(actual = actual, predicted = predictions)

ggplot(results, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.3, color = "purple") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual IMDb Score",
       x = "Actual Score", y = "Predicted Score") +
  theme_minimal()

# Residual plot — to check model assumptions
ggplot(data.frame(fitted = model1$fitted.values,
                  residuals = model1$residuals),
       aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()


#Step 7 — non-linear model
library(randomForest)
set.seed(123)
rf_model <- randomForest(
  score ~ votes + runtime + genre,
  data = train_data_clean,
  ntree = 500,
  importance = TRUE,
  na.action = na.omit
)
test_data_clean <- na.omit(test_data)
rf_predictions <- predict(rf_model, newdata = test_data_clean)
actual <- test_data_clean$score
RMSE_rf <- sqrt(mean((rf_predictions - actual)^2, na.rm = TRUE))
R2_rf   <- cor(rf_predictions, actual, use = "complete.obs")^2

cat("Random Forest RMSE:", round(RMSE_rf, 3),
    "\nRandom Forest R²  :", round(R2_rf, 3))

varImpPlot(rf_model)


















































