setwd("E:/KULIAH/Tugas/Semester 6 (T)/PDT/Tugas 2 - Klastering")
library(readr)
data <- read_csv("Tugas Klp #2 Data 1.csv")
colnames(data) <- c('sex','length','diameter','height','whole_weight',
                    'shucked_weight','viscera_weight','shell_weight','rings')

#=== Summarization Data ===
sapply(data, class)
for(i in 1:9){
  print(colnames(data[i]))
  print(length(unique(data[,i])))
}
str(data)
dim(data)
head(data)
tail(data)
summary(data)

#=== Visualisasi Data ===
for(i in 2:9){
  dev(new)
  hist(data[,i], xlab = colnames(data[i]), main = paste("Histogram of" , colnames(data[i])))
}
table(data$Sex)
pie(data$Sex)

#=== Praproses ===
#mengubah atribut sex menjadi NUMERIC

#datanum <- data
#datanum$Sex <- factor(datanum$Sex,
                             # levels = c("M", "F", "I"),
                             # labels = c("1", "2", "3"))
#datanum$Sex <- as.numeric(datanum$Sex)
#datanosex <- data[,2:9]
library(dplyr)
datafix  <- data
datafix <- datafix %>%
  mutate(sex_num =case_when(
    sex %in% 'M' ~ 0,
    sex %in% "F"  ~ 1,
    sex %in% "I" ~ 2
  ))
datafix <- datafix[-1]    #drop variable sex

datafix <- datafix %>%
  mutate(age=case_when(
    rings %in% 1:5 ~ 1,
    rings %in% 6:13 ~ 2,
    rings %in% 14:30 ~ 3
  ))

#=== Outlier ===
#----identifikasi outlier w/ Klustering Based (DBSCAN)----
library(dbscan)
outlier.dbscan <- dbscan(datafix[1:9], eps=0.42, MinPts = 5)
outlier.db <- which(outlier.dbscan$cluster == 0)

dev.new()
cat("Jumlah Outlier :", length(outlier.db))
print(outlier.db)
print(datafix[outlier.db,])
View(datafix[outlier.db,]) #melihat tabel data outlier

dev.new()
fviz_cluster(list(data = datafix[1:7], cluster = outlier.dbscan$cluster))

for (i in 1:7) {
  for (j in i:7) {
    #menampilkan plot
    plot(datafix [c(i,j)],
         col=1,
         pch = 1)
    #menampilkan titik outlier pada tabel
    points(datafix[outlier.db, c(i,j)],
           col = "red", 
           pch = 19, 
           cex = 1)
  }
}

#plot(datafix[1:9], 
#col=outlier.db,
#pch = 20)
#points(datafix[outlier.db,1:9], col = "red", pch = "x", cex = 5)
#print(datafix[outlier.db,1:9])


#----LOF----
library(DMwR)
outlier.scores <- lofactor(datafix, k=3)
plot(density(outlier.scores))

outlier.lof <- order(outlier.scores, decreasing=TRUE)[1:length(outlier.db)]
print(outlier.lof)
print(data[outlier.lof,])

#Plot outlier DBscan & LOF
n = nrow(datafix)                #menandai semua point dengan simbol .
pch <- rep(".", n)               #menandai semua outlier dengan warna black
pch[outlier.db] <- "+"           #menandai semua outlier dengan simbol +
col <- rep("black", n)           #menandai semua outlier dengan simbol *
col[outlier.db] <- "red"         #mengubah tanda outlier LOF menjadi warna blue
pairs(datafix, pch=pch, col=col) #mengubah tanda outlier LOF menjadi warna red

library(plyr)
outlier.fix <- join(datafix[outlier.db,], datafix[outlier.lof,], type="inner")
outlier.fix

#fviz_cluster(list(data = datafix[1:7], cluster = data[outlier.lof,]))

data.clean <- anti_join(datafix, outlier.fix, by=NULL)

#===Clustering===
abalone_cluster <- data.clean[1:7]
abalone_scale <- scale(abalone_cluster)
#----KMeans----
clusterkmeans <- kmeans(abalone_scale, center = 4, nstart=20)
clusterkmeans$cluster

#Plot KMeans
plot(abalone_cluster,
     col = clusterkmeans$cluster)
fviz_cluster(clusterkmeans, data=abalone_scale)

#----VQ----


#----Hierarchy----
#~~~~Aggrlomerative~~~~
library(cluster)
library(factoextra)
#link_method <- c("single", "complete", "average", "ward")

#MIN (single)
cluster.agnes_min <- agnes(x=abalone_cluster,
                       stand = TRUE,
                       metric = "euclidean",
                       method = "single")

library(dendextend)
#plot dendogram2
aglo_dend_min <- as.dendrogram(cluster.agnes_min)
aglo_dend_col <- color_branches(aglo_dend_min, k = 4, groupLabels = TRUE)
dev.new()
plot(aglo_dend_col)

#plot per kolom
cluster.cut_min <- cutree(cluster.agnes_min, k=4)
dev.new()
plot(abalone_cluster, col=cluster.cut_min, 
     main="Agglomerative - MIN")

#plot
dev.new()
fviz_cluster(list(data = abalone_cluster, cluster = cluster.cut_min))

#plot dendogram
fviz_dend(cluster.agnes_min, cex=0.4, k=4)

#MAX (complete)
cluster.agnes_max <- agnes(x=abalone_cluster,
                       stand = TRUE,
                       metric = "euclidean",
                       method = "complete")

#plot dendogram2
aglo_dend_max <- as.dendrogram(cluster.agnes_max)
aglo_dend_col <- color_branches(aglo_dend_max, k = 4)
dev.new()
plot(aglo_dend_col)

#plot per kolom
cluster.cut_max <- cutree(cluster.agnes_max, k=4)
dev.new()
plot(abalone_cluster, col=cluster.cut_max, 
     main="Agglomerative - MAX")

#plot
dev.new()
fviz_cluster(list(data = abalone_cluster, cluster = cluster.cut_max))

#pltree(cluster.agnes_max, hang = -1, cex = 0.6, main = "Dendrogram of agnes")
#plot dendogram1
fviz_dend(cluster.agnes_max, cex=0.4, k=4)

#AVERAGE (average)
cluster.agnes_avg <- agnes(x=abalone_cluster,
                       stand = TRUE,
                       metric = "euclidean",
                       method = "average")

#plot dendogram2
aglo_dend_avg <- as.dendrogram(cluster.agnes_avg)
aglo_dend_col <- color_branches(aglo_dend_avg, k = 4, groupLabels =TRUE)
dev.new()
plot(aglo_dend_col)

#plot per kolom
cluster.cut_avg <- cutree(cluster.agnes_avg, k=4)
dev.new()
plot(abalone_cluster, col=cluster.cut_avg, 
     main="Agglomerative - Average")

#plot
dev.new()
fviz_cluster(list(data = abalone_cluster, cluster = cluster.cut_avg))


#plot dendogram
fviz_dend(cluster.agnes_avg, cex=0.4, k=4)

#~~~~Divisive~~~~
cluster.diana <- diana(x=abalone_cluster,
                       stand = TRUE,
                       metric = "euclidean")

#plot dendogram2
div_dend <- as.dendrogram(cluster.diana)
div_dend_col <- color_branches(div_dend, k = 4, groupLabels =TRUE)
dev.new()
plot(div_dend_col)

#plot per kolom
cluster.cut.diana <- cutree(cluster.diana, k=4)
dev.new()
plot(abalone_cluster, col=cluster.cut.diana, 
     main="Divisive")

#plot
dev.new()
fviz_cluster(list(data=abalone_cluster, cluster=cluster.cut.diana))


#plot dendogram
dev.new()
fviz_dend(cluster.diana, cex=0.4, k=4)

#----DBSCAN----
library(dbscan)
dbscan::kNNdistplot(abalone_cluster, k = 4)
abline(h=0.4, lty = 2)

library(fpc)
cluster_dbscan <- dbscan(abalone_cluster, eps=0.4, MinPts = 5)
cluster_dbscan
plot(cluster_dbscan, abalone_cluster)
fviz_cluster(cluster_dbscan, data=abalone_cluster, geom = "point")

library(fpc)
cluster_dbscan <- dbscan(abalone_cluster, eps=0.01, MinPts = 3)
cluster_dbscan
plot(cluster_dbscan, abalone_cluster)
fviz_cluster(cluster_dbscan, data=abalone_cluster, geom = "point")

library(fpc)
cluster_dbscan <- dbscan(abalone_cluster, eps=0.03, MinPts = 7)
cluster_dbscan
plot(cluster_dbscan, abalone_cluster)
fviz_cluster(cluster_dbscan, data=abalone_cluster, geom = "point")

#===Evaluasi Model / Perbandingan===

#----Similarity Matrix----
abalone.dist <- dist(abalone_cluster)
library(seriation)
pimage(abalone.dist)
#---- Dissplot Agglo - MAX----
pimage(abalone.dist, order=order(cluster.cut_max))
dissplot(abalone.dist, labels=cluster.cut_max,
         options=list(main="DBSCAN with eps=0,4 minpts = 5"))

#---- Dissplot Dbscan----
pimage(abalone.dist, order=order(cluster_dbscan$cluster))
dissplot(abalone.dist, labels=cluster_dbscan.cluster1,
         options=list(main="DBSCAN with eps=0,4 minpts = 5"))
dissplot(abalone.dist, labels=cluster_dbscan.cluster2,
         options=list(main="DBSCAN with eps=0,01 minpts = 3"))

abalone.dist2 <- dist(abalone_cluster[which(cluster_dbscan$cluster != 0),])
label <- cluster_dbscan$cluster[which(cluster_dbscan$cluster != 0)]
dissplot(abalone.dist2, 
         labels=label,
         options=list(main="DBSCAN with eps=0,03 minpts = 7 without Noise Point"))

cluster_dbscan.cluster1 <- cluster_dbscan$cluster
cluster_dbscan.cluster1[cluster_dbscan.cluster1 == 0] <- 2
cluster_dbscan.cluster2 <- cluster_dbscan$cluster
cluster_dbscan.cluster2[cluster_dbscan.cluster2 == 0] <- 26
cluster_dbscan.cluster3 <- cluster_dbscan$cluster
cluster_dbscan.cluster3[cluster_dbscan.cluster3 == 0] <- 14

#---- SSE KMEANS----
SSE <- sapply(1:10, function(k) {
  kmeans(abalone_scale, centers = k, nstart = 20)$tot.withinss
})

plot(1:10, SSE, type="b", xlab = "k", 
     ylab = "Within grops sum of squares")

fviz_nbclust(abalone_scale, FUN = kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + 
  labs(subtitle = "Elbow Method")

#---- SSE Agglo----
SSE <- sapply(1:10, function(k) {
  cluster.agnes_avg$tot.withinss
})

plot(1:10, SSE, type="b", xlab = "k", 
     ylab = "Within grops sum of squares")

fviz_nbclust(abalone_cluster, FUN = hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + 
  labs(subtitle = "Elbow Method")

#---- Silhouette KMEANS----
library(cluster)
km.sil <- silhouette(clusterkmeans$cluster, dist(abalone_scale))
head(km.sil[,1:3],10)

#plot
plot(km.sil, main="Silhouette plot k-means")
fviz_nbclust(abalone_scale, kmeans, method = "silhouette")
fviz_silhouette(km.sil)

#---- Silhouette Agglo----
library(cluster)
aglomax.sil <- silhouette(cluster.cut_max, dist(abalone_cluster))
head(aglomax.sil[,1:3],10)
aglomin.sil <- silhouette(cluster.cut_min, dist(abalone_cluster))
head(aglomax.sil[,1:3],10)
agloavg.sil <- silhouette(cluster.cut_avg, dist(abalone_cluster))
head(agloavg.sil[,1:3],10)
div.sil <- silhouette(cluster.cut.diana, dist(abalone_cluster))
head(div.sil[,1:3],10)

#plot
plot(aglomax.sil, main="Silhouette plot Agglomerative - MAX ")
fviz_silhouette(aglomax.sil)
plot(aglomin.sil, main="Silhouette plot Agglomerative - MIN ")
dev.new()
fviz_silhouette(aglomin.sil)
plot(agloavg.sil, main="Silhouette plot Agglomerative - AVG ")
dev.new()
fviz_silhouette(agloavg.sil)
plot(div.sil, main="Silhouette plot Divisive ")
dev.new()
fviz_silhouette(div.sil)
