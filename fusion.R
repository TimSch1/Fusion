setwd("C:/Users/TimBo/Downloads/R docs and scripts/Fusion")
table = read.csv('firsttable.csv')
head(table)
table = table[,1:7]
head(table)
library(reshape2)
library(tidyr)
library(plyr)
library(dplyr)

#Get counts of visitors to article topics
p = ddply(table, .(fullVisitorId, customDimension9), count)
p = spread(p, customDimension9, n, fill=0)

#Exploratory Factor Analysis
processed = scale(p[,-1], center=TRUE, scale=TRUE)

library(psych)
pca = principal(processed, nfactor=2, covar=FALSE)
pca$loadings

#Visualize Factor Loadings
loadings = as.data.frame(pca$loadings[,1:2])
loadings$topic = rownames(loadings)
loadings_m = melt(loadings, id='topic')

library(ggplot2)
ggplot(loadings_m, aes(x=variable, y=topic, label = round(value,2), fill=value))+
  geom_tile()+xlab('Factor')+ylab('Topic')+geom_text(size=4, alpha = 0.8)+
  scale_fill_continuous(low='yellow', high='red', name='Loadings')+
  theme(axis.text.y = element_text(size=8))+
  theme_bw()

#Cluster Data
PCs = pca$scores
set.seed(400)
cluster=kmeans(processed, 2)

#Visualize Clusters
library(scatterplot3d)
library(rgl)
scatterplot3d(PCs[,2], PCs[,1], PCs[,3], color=cluster$cluster)
plot3d(PCs[,2], PCs[,1], PCs[,3], col=cluster$cluster)

library(cluster)
dissProc = daisy(processed)
dissProc2 = dissProc^2
plot(silhouette(cluster$cluster, dissProc2))


sort(cluster$centers[1,], decreasing=T)[1:5]
sort(cluster$centers[2,], decreasing=T)[1:5]
sort(cluster$centers[3,], decreasing=T)[1:5]
sort(cluster$centers[4,], decreasing=T)[1:5]

