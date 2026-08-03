library(factoextra)
library(ggplot2)
library(NbClust)

FM <- foxtail

FM$GEN <- as.character(FM$GEN)
rownames(FM) <- FM$GEN

df <- FM[, -1]
df <- na.omit(df)
df.scaled <- scale(df)

set.seed(123)

elbow_plot <- fviz_nbclust(df.scaled, FUN = hcut,
                           method = "wss",
                           hc_method = "ward.D2") +
  labs(title = "Elbow Method") + theme_bw()
print(elbow_plot)

sil_plot <- fviz_nbclust(df.scaled, FUN = hcut,
                         method = "silhouette",
                         hc_method = "ward.D2") +
  labs(title = "Silhouette Method") + theme_bw()
print(sil_plot)

gap_plot <- fviz_nbclust(df.scaled, FUN = hcut,
                         method = "gap_stat",
                         hc_method = "ward.D2",
                         nboot = 500) +
  labs(title = "Gap Statistic") + theme_bw()
print(gap_plot)

nb <- NbClust(data=df.scaled,
              distance="euclidean",
              min.nc=2,
              max.nc=10,
              method="ward.D2",
              index="all")

print(table(nb$Best.nc[1,]))
optimal_k <- as.numeric(names(sort(table(nb$Best.nc[1,]), decreasing=TRUE)[1]))
cat("Recommended number of clusters =", optimal_k, "\n")

#---------------------------------------------------------
# Euclidean distance and Ward.D2 clustering
#---------------------------------------------------------
res.dist <- dist(df.scaled, method="euclidean")
res.hc <- hclust(res.dist, method="ward.D2")


k <- optimal_k

FM$Cluster <- factor(cutree(res.hc, k=k))

AA <- fviz_dend(
  res.hc,
  k=k,
  rect=TRUE,
  rect_fill=TRUE,
  rect_border="jco",
  k_colors="jco",
  cex=0.8,
  lwd=0.8,
  label_cols="black",
  main="",
  xlab="Genotypes"
) +
theme_bw() +
theme(axis.title.x=element_text(size=14, face="bold"))

print(AA)

ggsave("Cluster_Dendrogram.png",
       AA,
       width=20,
       height=20,
       units="cm",
       dpi=600,
       bg="white")

cluster_membership <- data.frame(
  GEN=FM$GEN,
  Cluster=FM$Cluster
)
write.csv(cluster_membership,
          "Cluster_Membership.csv",
          row.names=FALSE)

cluster_counts <- table(FM$Cluster)
print(cluster_counts)

cluster_means <- aggregate(
  df,
  by=list(Cluster=FM$Cluster),
  FUN=mean
)
print(cluster_means)
write.csv(cluster_means,
          "Cluster_Means.csv",
          row.names=FALSE)

intra_cluster <- sapply(levels(FM$Cluster), function(cl){
  pts <- df.scaled[FM$Cluster==cl, , drop=FALSE]
  if(nrow(pts)<2) return(0)
  mean(dist(pts))
})
print(intra_cluster)
write.csv(data.frame(Cluster=names(intra_cluster),
                     IntraDistance=as.numeric(intra_cluster)),
          "Intra_Cluster_Distance.csv",
          row.names=FALSE)

centroids <- aggregate(df.scaled,
                       by=list(Cluster=FM$Cluster),
                       FUN=mean)

inter_cluster <- as.matrix(dist(centroids[,-1]))
print(inter_cluster)
write.csv(inter_cluster,
          "Inter_Cluster_Distance.csv")
