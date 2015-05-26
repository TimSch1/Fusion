SELECT fullVisitorId, visitNumber, visitId, hits.hitNumber, hits.time,
MAX(IF(hits.customDimensions.index=9,
       hits.customDimensions.value,
       NULL)) WITHIN hits AS customDimension9, MAX(IF(hits.customDimensions.index=10,
                                                      hits.customDimensions.value,
                                                      NULL)) WITHIN hits AS customDimension10, 
FROM [79749168.ga_sessions_20150522]
LIMIT 1800;
#customDimension10

setwd("C:/Users/TimBo/Downloads/R docs and scripts/Fusion")
table = read.csv('TimTable1.csv')
table = table[,c(1,6)]


library(tidyr)
library(reshape2)
library(plyr)

#Get counts of visitors to article topics
p = ddply(table, .(fullVisitorId, customDimension9), count)
p = spread(p, customDimension9, n, fill=0)
p = p[-143,-c(2:3)]

#Dimension10 - break up topics into components
h = strsplit(table$customDimension10, ' ')
names(h) = table$fullVisitorId
h = unlist(h)
j = data.frame(fullVisitorId = names(h), topic = h)
q = data.frame(fullVisitorId = unique(j$fullVisitorId), newId = 1:length(unique(j$fullVisitorId)))
j = left_join(j,q)
k = ddply(j, .(topic, newId), count)

k = spread(k, topic, n, fill=0)

#Exploratory Factor Analysis
processed = scale(p[,-1], center=TRUE, scale=TRUE)

library(psych)
library(GPArotation)
pca = principal(processed, nfactor=6, covar=FALSE)
pca$loadings

#Visualize Factor Loadings
loadings = as.data.frame(pca$loadings[,1:6])
loadings$topic = rownames(loadings)
loadings_m = melt(loadings, id='topic')

library(ggplot2)
ggplot(loadings_m, aes(x=variable, y=topic, label = round(value,2), fill=value))+
  geom_tile()+xlab('Factor')+ylab('Topic')+geom_text(size=4, alpha = 0.8)+
  scale_fill_continuous(low='yellow', high='red', name='Loadings')+
  theme(axis.text.y = element_text(size=8))+
  theme_bw()

#Visualize Coordinates
library(rgl)
dist.proc = dist(processed)
cmd.proc = cmdscale(dist.proc, 3)
plot(cmd.proc)
plot3d(cmd.proc[,2], cmd.proc[,1], cmd.proc[,3])

PCs = pca$scores
plot3d(PCs[,2], PCs[,1], PCs[,4])
plot3d(PCs[,2], PCs[,1], PCs[,3])

#Cluster Data
set.seed(400)
cluster=kmeans(processed, 4, nstart=250)

#Visualize Clusters
library(scatterplot3d)
library(rgl)
scatterplot3d(PCs[,2], PCs[,1], PCs[,4], color=cluster$cluster)
plot3d(PCs[,2], PCs[,1], PCs[,4], col=cluster$cluster)
plot3d(PCs[,2], PCs[,1], PCs[,3], col=cluster$cluster)
plot3d(cmd.proc[,2], cmd.proc[,1], cmd.proc[,3], col=cluster$cluster)



library(cluster)
dissProc = daisy(processed)
dissProc2 = dissProc^2
plot(silhouette(cluster$cluster, dist.proc))


sort(cluster$centers[1,], decreasing=T)[1:5]
sort(cluster$centers[2,], decreasing=T)[1:5]
sort(cluster$centers[3,], decreasing=T)[1:5]
sort(cluster$centers[4,], decreasing=T)[1:5]

