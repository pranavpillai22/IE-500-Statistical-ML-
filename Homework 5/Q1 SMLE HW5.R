#IE 500 SMLE HW5 Q1 
#part 1
wbdc <- read.table("C:/Users/ppill/Desktop/R files/wdbc.data", header = FALSE,sep = ",")
colnames(wbdc) <- c('ID','Diagnosis','Mean_Radius','Mean_Texture','Mean_Perimeter','Mean_Area',
                    'Mean_Smoothness','Mean_Compactness','Mean_Concavity', 'Mean_ConcavePoints',
                    'Mean_Symmetry','Mean_FractalDimesnsion','Radius_SE','Texture_SE','Perimeter_SE',
                    'Area_SE','Smoothness_SE','Compactness_SE','Concavity_SE','ConcavePoints_SE',
                    'Symmetry_SE','FractalDimension_SE','Worst_Radius','Worst_Texture','Worst_Perimeter',
                    'Worst_Area','Worst_Smoothness','Worst_Compactness','Worst_Concavity','Worst_ConcavePoints',
                    'Worst_Symmetry','Worst_FractalDimesnsion')
wbdc <- na.omit(wbdc)
wbdc$Diagnosis <- ifelse(wbdc$Diagnosis=="M",2,1)
wbdc$Diagnosis <- as.numeric(as.factor(wbdc$Diagnosis))
hist(wbdc$Diagnosis)
# part 2 
#k-means clustering 
set.seed(125)
wbdc_12 <- wbdc[,3:32]
kcluster <- kmeans(wbdc_12,2, nstart = 125)
kcluster

#hierarchical clustering
str(wbdc_12)
summary(wbdc_12)
any(is.na(wbdc_12))
hcluster <- hclust(dist(wbdc_12), method = "complete")
cutree_hcluster <- cutree(hcluster, k=2)
hcluster
table(wbdc$Diagnosis,kcluster$cluster)
table(wbdc$Diagnosis,cutree_hcluster)

#principle component analysis 
wbdc_pc <- prcomp(wbdc_12, center = TRUE, scale. = TRUE)
attributes(wbdc_pc)
print(wbdc_pc)
summary(wbdc_pc)
wbdc_pc$center
wbdc_pc$scale
wbdc_pc$rotation
head(wbdc_pc$x)
kcluster_pca <- kmeans(wbdc_pc$x[,1:17],2, nstart = 125) 
table(kcluster_pca$cluster,wbdc$Diagnosis)
hcluster_pca <- hclust(dist(wbdc_pc$x[,1:17]),method = "complete")
hcluster_clusters <- cutree(hcluster_pca, k=2 )
table(hcluster_clusters,wbdc$Diagnosis)

#data visualization

plot(wbdc_pc$x[,c(1,2)],col = (wbdc$Diagnosis+1),xlab = "PC1",ylab = "PC2")
plot(wbdc_pc$x[,c(1,4)],col = (wbdc$Diagnosis+1),xlab = "PC1",ylab = "PC4")
plot(wbdc_pc$x[,c(1,6)],col = (wbdc$Diagnosis+1),xlab = "PC1",ylab = "PC6")
plot(hcluster_pca)

