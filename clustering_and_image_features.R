# install.packages("BiocManager")
# BiocManager::install("EBImage")
library(EBImage)

# Load your image
img <- readImage("path_to_your_image.jpg")

# Convert the image to grayscale
gray_img <- channel(img, "gray")

# Compute Haralick features
features <- haralick(gray_img, symmetric = TRUE, distance = 1, angles = 0:3)

# Accessing Haralick features
contrast <- features["Contrast"]
correlation <- features["Correlation"]
energy <- features["Energy"]
homogeneity <- features["Homogeneity"]

###########################
data("USArrests")
distance <- dist(USArrests, method = "euclidean")
# method: "euclidean", "manhattan", "minkowski"
hc <- hclust(distance, method = "single")
# method: "single", "complete", "average", "centroid", "ward.D"
plot(hc, hang = -1)

hc <- hclust(distance, method = "complete")
plot(hc, hang = -1)

method = c("single", "complete", "average", "centroid", "ward.D")
overall_mean = apply(USArrests, 2, mean)
tot_ss = 0
for (i in 1:nrow(USArrests)) tot_ss = tot_ss + sum((USArrests[i,]-overall_mean)^2)
g_plot = data.frame(group = 1:10, withinss = rep(0,10), betweenss = rep(0,10))
for (m in method){
  hc <- hclust(dist(USArrests, method = "euclidean"), method = m)
  for(g in 2:nrow(g_plot)){
    ## within SS
    withinss = 0
    group_mean = sapply(USArrests, tapply, cutree(hc, k=g), mean)
    for (i in 1:g){
      group = USArrests[cutree(hc, k=g)==i,]
      for(j in 1:nrow(group)){
        withinss = withinss + sum((group[j,]-group_mean[i,])^2)
      }
    }
    g_plot$withinss[g] = withinss
    ## between SS
    g_plot$betweenss[g] = tot_ss - withinss
  }
  per = round(g_plot$betweenss/tot_ss,3)
  windows()
  plot(g_plot[2:10, "withinss"], type = "n", ylab = "Sum of squares", xlab = "Number of cluster",xlim = c(2,10), ylim = c(2000, tot_ss), main = m)
  points(g_plot$group[2:10], g_plot[2:10, "withinss"], col = 1, lty = 1, pch = 1)
  lines(g_plot$group[2:10], g_plot[2:10, "withinss"], col = 1, lty = 1, pch = 1)
  points(g_plot$group[2:10], g_plot[2:10, "betweenss"], col = 2, lty = 2, pch = 2)
  lines(g_plot$group[2:10], g_plot[2:10, "betweenss"], col = 2, lty = 2, pch = 2)
  text(g_plot$group[2:10], g_plot[2:10, "betweenss"]-10000, per[2:10], col = 4)
  
  legend("topleft", legend = c("within SS", "between SS"), col = 1:2, lty = 1:2, pch = 1:2)
  
}

distance <- dist(USArrests, method = "euclidean")
hc <- hclust(distance, method = "complete")
plot(hc, hang = -1)
rect.hclust(hc, 3)
cutree(hc, 3)

## K-means
set.seed(1111)
g_plot = data.frame(group = 1:10, withinss = rep(0:10), betweenss = rep(0:10))
for(g in 2:nrow(g_plot)){
  km = kmeans(USArrests, centers = g)
  g_plot$withinss[g] = km$tot.withinss
  g_plot$betweenss[g] = km$tot.betweenss
}
per = round(g_plot$betweenss/km$totss, 3)
plot(g_plot[2:10, "withinss"], type="n", ylab = "Sum of squares", xlab = "Number of cluster",xlim = c(2,10), ylim = c(12000, 350000), main = "k-means")

points(g_plot$group[2:10], g_plot[2:10, "withinss"], col = 1, lty = 1, pch = 1)
lines(g_plot$group[2:10], g_plot[2:10, "withinss"], col = 1, lty = 1, pch = 1)
points(g_plot$group[2:10], g_plot[2:10, "betweenss"], col = 2, lty = 2, pch = 2)
lines(g_plot$group[2:10], g_plot[2:10, "betweenss"], col = 2, lty = 2, pch = 2)
text(g_plot$group[2:10], g_plot[2:10, "betweenss"]-10000, per[2:10], col = 4)

legend("topleft", legend = c("within SS", "between SS"), col = 1:2, lty = 1:2, pch = 1:2)

# 展示 k-means 分群結果
kmeans(USArrests, centers = 3)$cluster

