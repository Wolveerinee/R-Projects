# Project: Advanced Analysis and Visualization of the Iris Dataset

# Load required libraries
library(ggplot2)
library(dplyr)

# Load the Iris dataset
data(iris)

# Explore the dataset
summary(iris)
str(iris)

# Data preprocessing: Standardize numeric columns
iris_scaled <- iris %>%
  mutate(across(where(is.numeric), scale))

# Perform hierarchical clustering on the standardized data
iris_clusters <- hclust(dist(iris_scaled[, 1:4]))

# Plot the dendrogram for hierarchical clustering
plot(iris_clusters, main = "Hierarchical Clustering Dendrogram", sub = NULL)

# Cut the dendrogram to form clusters
iris_cut_clusters <- cutree(iris_clusters, k = 3)

# Add cluster information to the dataset
iris_with_clusters <- cbind(iris, Cluster = as.factor(iris_cut_clusters))

# Visualize the clusters using a scatter plot
ggplot(iris_with_clusters, aes(x = Sepal.Length, y = Sepal.Width, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "Clustered Iris Dataset - Sepal Length vs. Sepal Width",
       x = "Sepal Length", y = "Sepal Width") +
  theme_minimal()

# Conduct PCA analysis for dimensionality reduction
iris_pca <- prcomp(iris_scaled[, 1:4], scale. = TRUE)

# Plot the scree plot for PCA
screeplot(iris_pca, type = "lines", main = "Scree Plot for PCA")

# Plot the biplot for the first two principal components
biplot(iris_pca, scale = 0, cex = 0.8, main = "Biplot for PCA")

# Perform k-means clustering on the original data
set.seed(123)  # Set seed for reproducibility
kmeans_result <- kmeans(iris[, 1:4], centers = 3, nstart = 20)

# Add k-means cluster information to the dataset
iris_with_kmeans <- cbind(iris, Cluster_kmeans = as.factor(kmeans_result$cluster))

# Visualize k-means clusters using a scatter plot
ggplot(iris_with_kmeans, aes(x = Sepal.Length, y = Sepal.Width, color = Cluster_kmeans)) +
  geom_point(size = 3) +
  labs(title = "K-means Clustering - Sepal Length vs. Sepal Width",
       x = "Sepal Length", y = "Sepal Width") +
  theme_minimal()
