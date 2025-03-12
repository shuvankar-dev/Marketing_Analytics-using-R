
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("psych")
install.packages("factoextra")
install.packages("caret")
install.packages("corrplot")
install.packages("NbClust")


# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(psych)
library(cluster)
library(factoextra)
library(caret)
library(corrplot)
library(NbClust)

# Load the dataset
df <- read.csv("SmartFresh Retail.csv")

# Null Value Check
colSums(is.na(df))
sum(is.na(df))

#Replace missing values with Mean
df$Annual_Income[is.na(df$Annual_Income)] <- mean(df$Annual_Income, na.rm = TRUE)

# Null Value Check
colSums(is.na(df))
sum(is.na(df))

# Convert categorical variables to factors
df$Education_Level <- as.factor(df$Education_Level)
df$Marital_Status <- as.factor(df$Marital_Status)

# Convert Date columns
df$Dt_Customer <- as.Date(df$Dt_Customer, format="%Y-%m-%d")

# Summary Statistics
summary_stats <- df %>%
  summarise(
    Mean_Age = mean(2025 - Year_Birth, na.rm = TRUE),
    Median_Income = median(Annual_Income, na.rm = TRUE),
    SD_Income = sd(Annual_Income, na.rm = TRUE),
    Variance_Income = var(Annual_Income, na.rm = TRUE),
    Mean_Online_Purchases = mean(Purchases_Online, na.rm = TRUE)
  )
print(summary_stats)

# Data Visualization
## Histogram for Annual Income
ggplot(df, aes(x = Annual_Income)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Annual Income", x = "Annual Income (£)", y = "Count")

## Boxplot for Spending Categories
df_long <- df %>%
  select(Spend_Wine, Spend_OrganicFood, Spend_Meat, Spend_WellnessProducts, Spend_Treats, Spend_LuxuryGoods) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Amount")

ggplot(df_long, aes(x = Category, y = Amount, fill = Category)) +
  geom_boxplot() +
  labs(title = "Spending Distribution by Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Hypothesis Testing (T-tests)
t_test_result <- t.test(df$Spend_Wine[df$Accepted_Offer1 == 1],
                        df$Spend_Wine[df$Accepted_Offer1 == 0],
                        alternative = "two.sided")
print(t_test_result)

# Correlation Mat
correlation_matrix <- cor(df %>% select(Spend_Wine:Spend_LuxuryGoods), use = "complete.obs")
corrplot(correlation_matrix, method = "color")

# Factor Analysis (PCA)
fa_result <- fa(df %>% select(Spend_Wine:Spend_LuxuryGoods), nfactors = 3, rotate = "varimax")
print(fa_result$loadings)

# Clustering Analysis
## Scale numerical variables
df_scaled <- scale(df %>% select(Spend_Wine:Spend_LuxuryGoods))

## Determine optimal clusters
fviz_nbclust(df_scaled, kmeans, method = "wss")

## Apply K-Means
set.seed(123)
kmeans_result <- kmeans(df_scaled, centers = 3, nstart = 25)
df$Cluster <- as.factor(kmeans_result$cluster)

## Visualizing Clusters
fviz_cluster(kmeans_result, data = df_scaled)

# Hierarchical Clustering
windows(10,5)
dist_matrix <- dist(df_scaled, method = "euclidean")
hclust_result <- hclust(dist_matrix, method = "ward.D2")
plot(hclust_result, labels = FALSE, main = "Dendrogram of Customers")

# Interpretation and Business Recommendations
cat("Summary of Customer Segments:\n")
table(df$Cluster)
      