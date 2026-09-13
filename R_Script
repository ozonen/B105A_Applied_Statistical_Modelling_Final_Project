# B105 Applied Statistical Modelling — Final Project
# Dataset: https://www.kaggle.com/datasets/fronkongames/steam-games-dataset


# 1. Setup & Import 
# setwd("C:/Users/Ozgur/OneDrive/Masaüstü/Applied_Statistical_Modelling_Final_Project")   
games <- read.csv("games.csv", header = TRUE)

str(games)                 
colSums(is.na(games))     


# 2. Data Cleaning
# Score.rank and Movies are almost entirely empty -> drop them
games$Score.rank <- NULL
games$Movies <- NULL

# Proportion of positive reviews
games$review_score <- games$Positive / (games$Positive + games$Negative)
summary(games$review_score)   # NAs here = games with zero reviews

# Remove games with no reviews 
games_clean <- games[!is.na(games$review_score), ]
nrow(games_clean)             # 82,956 games remain
summary(games_clean$Price)    # median $0, mean $23.44, max $100


# 3. Exploratory Data Analysis 
hist(games_clean$review_score,
     main = "Distribution of Review Scores", xlab = "Review Score")

hist(games_clean$Price,
     main = "Distribution of Price", xlab = "Price (USD)")

boxplot(games_clean$Price,
        main = "Boxplot of Price")

# Scatterplot with transparency to handle overplotting (~83k points)
plot(games_clean$Price, games_clean$review_score,
     main = "Price vs Review Score", xlab = "Price (USD)", ylab = "Review Score",
     pch = 16, col = rgb(0, 0, 0, 0.05))


# 4. Business Question 1: Price vs Review Score
# H0: no correlation between price and review score (rho = 0)
# H1: there is a correlation (rho != 0)
cor.test(games_clean$Price, games_clean$review_score)
# Result: r = 0.029, p < .001 -> statistically significant but practically negligible 


# 5. Preparing Genre for Business Question 2 
# Genres column is comma-separated (e.g. "Casual,Indie,Simulation")
games_clean$primary_genre <- sapply(strsplit(games_clean$Genres, ","), `[`, 1)
table(games_clean$primary_genre)

# Remove non-game software categories that leaked into the Genres field
non_games <- c("Accounting", "Animation & Modeling", "Audio Production",
               "Design & Illustration", "Education", "Game Development",
               "Photo Editing", "Software Training", "Utilities",
               "Video Production", "Web Publishing")
games_genre <- games_clean[!(games_clean$primary_genre %in% non_games), ]
games_genre <- games_genre[games_genre$primary_genre != "", ]

# Remove content-descriptor tags that aren't real genres
non_genre_tags <- c("Early Access", "Gore", "Nudity", "Sexual Content", "Violent")
games_genre <- games_genre[!(games_genre$primary_genre %in% non_genre_tags), ]

games_genre$primary_genre <- factor(games_genre$primary_genre)
table(games_genre$primary_genre)   # 11 clean genres, 81,310 games


# 6. Business Question 2: Genre vs Review Score 
# H0: mean review score is equal across all genres
# H1: at least one genre differs

boxplot(review_score ~ primary_genre, data = games_genre, las = 2,
        main = "Review Score by Genre", xlab = "", ylab = "Review Score")

# Check ANOVA assumptions before trust the result
bartlett.test(review_score ~ primary_genre, data = games_genre)  # (p < .001)

anova_genre <- aov(review_score ~ primary_genre, data = games_genre)
summary(anova_genre)                                             # F = 32.47, p < .001

qqnorm(residuals(anova_genre))                                   # residuals not normal
qqline(residuals(anova_genre), col = "red")

# Assumptions were violated -> confirm with robust alternatives
oneway.test(review_score ~ primary_genre, data = games_genre, var.equal = FALSE)  # Welch's ANOVA
kruskal.test(review_score ~ primary_genre, data = games_genre)                    # Kruskal-Wallis

# All three tests agree: genre has a significant effect on review score

# Post-hoc: which specific genres differ from which?
pairwise.wilcox.test(games_genre$review_score, games_genre$primary_genre,
                      p.adjust.method = "bonferroni")

# Summary table: mean/median review score per genre, sorted best to worst
genre_summary <- aggregate(review_score ~ primary_genre, data = games_genre,
                            FUN = function(x) c(mean = mean(x), median = median(x), n = length(x)))
genre_summary <- do.call(data.frame, genre_summary)
genre_summary[order(-genre_summary$review_score.mean), ]
