library(ISLR)
library(clValid)
library(plotrix)
library(devtools)

data(College)
College_private <- College[, 1]
College_x <- College[, -1]

scaled_college_x <- scale(College_x, center=T, scale=T)
clvalid_college <- clValid(scaled_college_x, 2:10, clMethods='kmeans', validation=c('internal', 'stability'), maxitems=1000)
summary(clvalid_college)

kmeans_college3 <- kmeans(scaled_college_x, 3)
kmeans_college3$centers
kmeans_college3$size
kmeans_college3$cluster

scaled_data <- data.frame(scaled_college_x, clusterID = as.factor(kmeans_college3$cluster))
scaled_chart <- c()
for (i in 1:ncol(scaled_college_x)) {
  scaled_chart <- rbind(scaled_chart, 
                        tapply(scaled_data[, i], scaled_data$clusterID, mean))
}
colnames(scaled_chart) <- paste("Cluster", c(1:3))
rownames(scaled_chart) <- colnames(scaled_college_x)
par(mfrow = c(1,3))
for (i in 1:3){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(scaled_chart[, i], labels = rownames(scaled_chart), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
}

kmeans_college10 <- kmeans(scaled_college_x, 10)
kmeans_college10$centers
kmeans_college10$size

kmc_cluster1 <- scaled_college_x[kmeans_college3$cluster==1, ]
kmc_cluster2 <- scaled_college_x[kmeans_college3$cluster==2, ]
kmc_cluster3 <- scaled_college_x[kmeans_college3$cluster==3, ]

kmc_t_tst12 <- matrix(0, 17, 4)
colnames(kmc_t_tst12) <- c('two.sided', 'greater', 'less', 'min.p-value')
rownames(kmc_t_tst12) <- t(colnames(scaled_college_x))
kmc_t_tst12 <- data.frame(kmc_t_tst12)
for (i in 1:17) {
  kmc_t_tst12[i, 1] <- t.test(kmc_cluster1[, i], kmc_cluster2[, i], alternative='two.sided')$p.value
  kmc_t_tst12[i, 2] <- t.test(kmc_cluster1[, i], kmc_cluster2[, i], alternative='greater')$p.value
  kmc_t_tst12[i, 3] <- t.test(kmc_cluster1[, i], kmc_cluster2[, i], alternative='less')$p.value
  kmc_t_tst31[i, 4] <- ifelse (min(kmc_t_tst12[i, 1:3]) <= 0.05, colnames(kmc_t_tst12[which.min(kmc_t_tst12[i, 1:3])]), NA)
  
}

kmc_t_tst23 <- matrix(0, 17, 4)
colnames(kmc_t_tst23) <- c('two.sided', 'greater', 'less', 'min.p-value')
rownames(kmc_t_tst23) <- t(colnames(scaled_college_x))
kmc_t_tst23 <- data.frame(kmc_t_tst23)
for (i in 1:17) {
  kmc_t_tst23[i, 1] <- t.test(kmc_cluster2[, i], kmc_cluster3[, i], alternative='two.sided')$p.value
  kmc_t_tst23[i, 2] <- t.test(kmc_cluster2[, i], kmc_cluster3[, i], alternative='greater')$p.value
  kmc_t_tst23[i, 3] <- t.test(kmc_cluster2[, i], kmc_cluster3[, i], alternative='less')$p.value
  kmc_t_tst23[i, 4] <- ifelse (min(kmc_t_tst23[i, 1:3]) <= 0.05, colnames(kmc_t_tst23[which.min(kmc_t_tst23[i, 1:3])]), NA)
  
}

kmc_t_tst31 <- matrix(0, 17, 4)
colnames(kmc_t_tst31) <- c('two.sided', 'greater', 'less', 'min.p-value')
rownames(kmc_t_tst31) <- t(colnames(scaled_college_x))
kmc_t_tst31 <- data.frame(kmc_t_tst31)
for (i in 1:17) {
  kmc_t_tst31[i, 1] <- t.test(kmc_cluster3[, i], kmc_cluster1[, i], alternative='two.sided')$p.value
  kmc_t_tst31[i, 2] <- t.test(kmc_cluster3[, i], kmc_cluster1[, i], alternative='greater')$p.value
  kmc_t_tst31[i, 3] <- t.test(kmc_cluster3[, i], kmc_cluster1[, i], alternative='less')$p.value
  kmc_t_tst31[i, 4] <- ifelse (min(kmc_t_tst31[i, 1:3]) <= 0.05, colnames(kmc_t_tst31[which.min(kmc_t_tst31[i, 1:3])]), NA)
}

dev.off()

cor_mat <- cor(t(scaled_college_x), method='spearman')
dist_college <- as.dist(1-cor_mat)
hier_clust <- hclust(dist_college, method='complete', members=NULL)
plot(hier_clust, hang=-1)

clvalid_hier_college <- clValid(scaled_college_x, 2:10, clMethods='hierarchical', validation=c('internal', 'stability'), maxitems=1000)
summary(clvalid_hier_college)

single_clust <- hclust(dist_college, method='single', members=NULL)
average_clust <- hclust(dist_college, method='average', members=NULL)
centroid_clust <- hclust(dist_college, method='centroid', members=NULL)
plot(single_clust)
plot(average_clust)
plot(centroid_clust)

complete_clust10 <- cutree(hier_clust, k=10)

plot(hier_clust)
rect.hclust(hier_clust, k=10, border='red')

college_hc <- data.frame(scaled_college_x, clusterID=as.factor(complete_clust10))
college_hc
hc_summary <- data.frame()
for ( i in 1:(ncol(college_hc)-1) ) {
  hc_summary <- rbind(hc_summary, tapply(college_hc[, i], college_hc$clusterID, mean))
}
colnames(hc_summary) <- paste('cluster', c(1:10))
rownames(hc_summary) <- colnames(college_hc)[1:(ncol(college_hc)-1)]
hc_summary

par(mfrow=c(2, 5))
for (i in 1:10){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(hc_summary[, i], labels = rownames(hc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
}

hier_clust2 <- scaled_college_x[college_hc$clusterID==2, ]
hier_clust10 <- scaled_college_x[college_hc$clusterID==10, ]
hier_t_test <- matrix(0, nrow=nrow(hc_summary), ncol=4)
rownames(hier_t_test) <- rownames(hc_summary)
colnames(hier_t_test) <- c('two.sided', 'greater', 'less', 'min.p_value')
hier_t_test <- data.frame(hier_t_test)
for (i in 1:nrow(hc_summary)) {
  hier_t_test[i, 1] <- t.test(hier_clust2[, i], hier_clust10[, i], alternative='two.sided')$p.value
  hier_t_test[i, 2] <- t.test(hier_clust2[, i], hier_clust10[, i], alternative='greater')$p.value
  hier_t_test[i, 3] <- t.test(hier_clust2[, i], hier_clust10[, i], alternative='less')$p.value
  hier_t_test[i, 4] <- ifelse(min(hier_t_test[i, 1:3]) <= 0.5,
                              colnames(hier_t_test[which.min(hier_t_test[i, 1:3])]), NA)
}

personal_loan <- read.csv('Assignment8.csv')

library(factoextra)
library(dbscan)
library(FactoMineR)

df_loan <- personal_loan[, c(2:4, 6:9, 11:14)]
scaled_loan <- scale(df_loan, center=T, scale=T)

eps_list <- c(1, 2, 3, 4, 5, 10)
minPts_list <- c(6, 7, 10, 15, 30)
perf_matrix <- matrix(0, length(eps_list)*length(minPts_list), 4)
for (i in 1:length(eps_list)) {
  for (j in 1:length(minPts_list)) {
    dbscan_loan <- dbscan(scaled_loan, eps=eps_list[i], minPts=minPts_list[j])
    perf_matrix[length(minPts_list)*i-length(minPts_list)+j, 1] <- eps_list[i]
    perf_matrix[length(minPts_list)*i-length(minPts_list)+j, 2] <- minPts_list[j]
    perf_matrix[length(minPts_list)*i-length(minPts_list)+j, 3] <- length(unique(dbscan_loan$cluster))-1
    perf_matrix[length(minPts_list)*i-length(minPts_list)+j, 4] <- length(which(dbscan_loan$cluster==0))
  }
}
perf_matrix

dbscan_loan <- dbscan(scaled_loan, eps=3, minPts=6)
fviz_cluster(dbscan_loan, scaled_loan, ellipse=T, geom='point', show.clust.cent=F)

loan_data <- data.frame(scaled_loan, clusterID = as.factor(dbscan_loan$cluster))
radar_data <- data.frame()
for (i in 1:ncol(scaled_loan)) {
  radar_data <- rbind(radar_data, 
                        tapply(loan_data[, i], loan_data$clusterID, mean))
}
colnames(radar_data) <- c('Cluster', c(1:4))
rownames(radar_data) <- colnames(scaled_loan)

par(mfrow=c(2, 2))
for (i in 1:4){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(radar_data[, i], labels = rownames(radar_data), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
}
dev.off()

pca_loan <- prcomp(df_loan, center=T, scale=T)
plot(pca_loan, type='l')
summary(pca_loan)
biplot(pca_loan)
pca_loan_data <- PCA(df_loan)
fviz_pca_ind(pca_loan_data, geom.ind='point', col.ind=loan_data$clusterID, addEllipses=T, legend.title='Groups')