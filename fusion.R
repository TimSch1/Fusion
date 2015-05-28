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
library(data.table)
library(plyr)
library(dplyr)
table = fread('TimTable1.csv')
sections = c('news','justice','pop-culture','sex-life','real-future','voices','show')
table = table[table$customDimension9 %in% sections,]
user.topic = select(table, fullVisitorId, customDimension9)


library(tidyr)
library(reshape2)


##Much faster!!
user.counts = user.topic %>% 
              group_by(fullVisitorId, customDimension9) %>% 
              summarize(counts = n())

#Get counts of visitors to article topics - SLOW!! (skip)
#registerDoSNOW(makeCluster(3, type = "SOCK"))
#user.counts = ddply(user.topic, .(fullVisitorId, customDimension9), count, .parallel = TRUE, .progress= 'text')

##Resume
user.counts.wide = spread(ungroup(user.counts), customDimension9, counts, fill=0)


#Dimension10 - break up topics into components - ONLY for DIM 10******
h = strsplit(table$customDimension10, ' ')
names(h) = table$fullVisitorId
h = unlist(h)
j = data.frame(fullVisitorId = names(h), topic = h)
q = data.frame(fullVisitorId = unique(j$fullVisitorId), newId = 1:length(unique(j$fullVisitorId)))
j = left_join(j,q)
k = ddply(j, .(topic, newId), count)

k = spread(k, topic, n, fill=0)

#Exploratory Factor Analysis
processed = as.data.frame(user.counts.wide)
processed = scale(processed[,-1], center=TRUE, scale=TRUE)

library(psych)
library(GPArotation)
pca = principal(processed, nfactor=7, covar=FALSE)
pca$loadings

#Visualize Factor Loadings
loadings = as.data.frame(pca$loadings[,1:7])
loadings$topic = rownames(loadings)
loadings_m = melt(loadings, id='topic')

library(ggplot2)
ggplot(loadings_m, aes(x=variable, y=topic, label = round(value,2), fill=value))+
  geom_tile()+xlab('Factor')+ylab('Topic')+geom_text(size=4, alpha = 0.8)+
  scale_fill_continuous(low='yellow', high='red', name='Loadings')+
  theme(axis.text.y = element_text(size=8))+
  theme_bw()

#Visualize Coordinates - dist matrix too memory intensive for day of data (123Gb)
library(rgl)
PCs = pca$scores
plot3d(PCs[,2], PCs[,1], PCs[,4])
plot3d(PCs[,2], PCs[,1], PCs[,3])

#Cluster Data
set.seed(400)
cluster=kmeans(processed, 7, nstart=1000)

#Visualize Clusters
plot3d(PCs[,2], PCs[,1], PCs[,4], col=cluster$cluster)
plot3d(PCs[,2], PCs[,1], PCs[,3], col=cluster$cluster)


##Doesn't work with a fill-day sample size, dist matrix is 123Gb!
library(cluster)
dissProc = daisy(processed)
dissProc2 = dissProc^2
plot(silhouette(cluster$cluster, dist.proc))

#What do the clusters look like?
sort(cluster$centers[1,], decreasing=T)[1:5]
sort(cluster$centers[2,], decreasing=T)[1:5]
sort(cluster$centers[3,], decreasing=T)[1:5]
sort(cluster$centers[4,], decreasing=T)[1:5]
sort(cluster$centers[5,], decreasing=T)[1:5]
sort(cluster$centers[6,], decreasing=T)[1:5]
sort(cluster$centers[7,], decreasing=T)[1:5]

#Cluster Size
table(cluster$cluster)/length(cluster$cluster)

user.counts.wide$label = cluster$cluster
userId.label = select(user.counts.wide, fullVisitorId, label)
labelled.table = left_join(table, userId.label)
labelled.table$shares = ifelse(labelled.table$hits_social_socialInteractionAction == 'share', 1, 0)

shares = labelled.table %>% group_by(label) %>% 
          summarize(count = n(), shares = sum(shares)) %>% 
          mutate(percent = shares/count*100)

shares = shares[complete.cases(shares),]

ggplot(shares, aes(x=count, y=shares, label=round(percent,3), color=factor(label)), size=20)+
        geom_point()+
        theme_bw()+
        xlab('Visits')+
        ylab('Shares')+
        scale_color_discrete(name='Class')+
        geom_text()

labelled.table$trafficSource_source = gsub('[[:lower:]]*.*facebook.*[[:lower:]]*', 'facebook', labelled.table$trafficSource_source)

origin = labelled.table %>% group_by(label, trafficSource_source) %>%
          summarize(by.source = n()) %>%
          mutate(by.class = sum(by.source), percent = by.source/by.class) %>%
          arrange(label, desc(percent)) %>%
          slice(1:5)

origin = origin[complete.cases(origin),]

ggplot(origin, aes(x=trafficSource_source, y=percent, fill = factor(trafficSource_source)))+
        geom_bar(stat='identity')+
        theme_bw()+
        facet_wrap(~label)+
        xlab('Traffic Origin')+
        ylab('Percentage of Traffic')+
        theme(axis.text.x = element_blank())+
        scale_fill_discrete(name='Source')

##Filter by fusion 'fans', people with > 3 visits
fanIds = table %>% filter(visitNumber > 3) %>% select(fullVisitorId)
fan.table = table %>% filter(fullVisitorId %in% fanIds[,1])
