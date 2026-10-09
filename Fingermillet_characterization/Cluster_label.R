
data <- read.csv(file.choose())


#-------------
# Observe data
#-------------


# Return the first or last part of an object

head(data)
tail(data)

# View the structure of variables

str(data)

#--------------------
# Formating variables
#--------------------

#------
# Change variable to character type
#------
# Create or test for objects of type "character"
# is.character = return TRUE or FALSE depending on whether its arguments is of character type or not.

FM$Landraces <- as.character(x = FM$Landraces)
str(FM)
head(data)

#------
# Change column variable as rownames for first variable
#------
# rownames = Retrieve or set the row or column names of a matrix-like object.
# c = Combine values into a vector or list

rownames(FM) <- c(FM$Landraces)
head(data)
# Convert tibble to a data frame
FM <- as.data.frame(FM)
# Use Landraces column directly without row names
FM$Landraces <- as.character(FM$Landraces)


# Set row names as Landraces
rownames(FM) <- FM$Landraces

#------
# Delete first column
#------
# [ = Extract or replace parts of a data frame.
# -1 = delete first column conatainig rownames

newdata <- FM[ ,-1]
head(newdata)

# Assign short name to data set

df <- newdata

#------
# replace any missing values
#------
# na.omit() = returns the object with incomplete cases removed.na.fail returns the object if it does not contain any missing values, and signals an error otherwise.na.pass returns the object unchanged.

df <- na.omit(df)
df
#------
# scaling data to standardize it
#------
# Scale = scale is generic function whose default method centers and/or scales the columns of a numeric matrix. scale data so that clustering do not depend on arbitrary variable value


library(factoextra)

# Scale data
df.scaled <- scale(df)
head(df.scaled)

# Elbow method (within-cluster sum of squares)
fviz_nbclust(df.scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Choosing Optimal k")

# Silhouette method
fviz_nbclust(df.scaled, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method")

# Gap statistic
set.seed(123)
fviz_nbclust(df.scaled, kmeans, nstart = 25, method = "gap_stat") +
  labs(title = "Gap Statistic Method")


#----------------------------
# Distance matrix computation 
#----------------------------

# dist() = This function computes and returns the distance matrix computed by using the specified distance measure to compute the distances between the rows of a data matrix.
# x = a numeric matrix, data frame or "dist" object.
# method = ("euclidean", "maximum", "manhattan", "canberra", 
#           "binary", "minkowski", "pearson", "spearman" or "kendall" )

require(stats)
res.dist <- dist(x = df.scaled, 
                 method = "euclidean")

# View the euclidean distance measurements

x <- as.matrix(res.dist)[1:14, 1:14]
x

# To round values in distance matrix use as.matrix function. Round first 3 column and 3 rows to three decimal place

round(x, digits = 3)



####Vizualization of Dendogram#####


AA <- fviz_dend(res.hc, 
                cex = 0.7,                # Size of labels
                lwd = 0.8,                # Line width of dendrogram
                k = 4,                    # Number of clusters
                rect = TRUE,              # Draw rectangles
                rect_fill = TRUE,         # Fill rectangles
                rect_border = "jco",      # Rectangle border colors
                k_colors = "jco",         # Cluster colors
                rect_h = 0.07,            # Decrease rectangle height
                label_cols = "black",     # Set label color to black
                label_font = list(face = "bold"), # Make labels bold
                main = " ", 
                xlab = "Landraces") +      # Add x-axis label
        
  theme(axis.title.x = element_text(size = 14))  # Customize x-axis title size
AA
ggsave(filename = "Cluster Dendogram1.png", plot = AA,width = 20, height = 20, dpi = 2500, units = "cm")




# ##############Calculate average mean value of each parameter in each cluster (for k-means or hierarchical clustering)#######

# If you have the cluster labels in the `Cluster` column (from k-means or hierarchical clustering):
cluster_means <- aggregate(FM[, -c(1, ncol(FM))],  # Exclude the first and last columns (Landraces and Cluster)
                           by = list(Cluster = FM$Cluster),  # Group by the cluster
                           FUN = mean)  # Calculate mean for each cluster

# Display the results
print(cluster_means)


#####INTRA-INTER CLUSTER DISTANCES###### 
Assuming 'FM$Cluster' contains cluster labels, and 'pea.s' contains the scaled data (excluding non-numeric columns)

# 1. Calculate intra-cluster distance:
intra_cluster_distances <- sapply(unique(FM$Cluster), function(cluster_id) {
  # Get the points in this cluster
  cluster_points <- pea.s[FM$Cluster == cluster_id, ]
  
  # Compute the pairwise distance matrix for the points in this cluster
  dist_matrix <- dist(cluster_points)
  
  # Calculate the average distance for the cluster (intra-cluster distance)
  mean(dist_matrix)
})

# Print intra-cluster distances
cat("Intra-cluster distances:\n")
print(intra_cluster_distances)

# 2. Calculate inter-cluster distance (distance between cluster centroids):
# First, calculate the centroids of each cluster
centroids <- aggregate(pea.s, by = list(Cluster = FM$Cluster), FUN = mean)

# Compute the pairwise distances between the centroids
centroid_distances <- dist(centroids[, -1])  # Exclude the cluster column from centroid data

# Print inter-cluster distances
cat("Inter-cluster distances (between centroids):\n")
print(centroid_distances)


# Assuming FM$Cluster contains the cluster labels assigned by your clustering algorithm

# Count how many genotypes are in each cluster
cluster_counts <- table(FM$Cluster)

# Print the count of genotypes in each cluster
cat("Number of genotypes in each cluster:\n")
print(cluster_counts)



  

