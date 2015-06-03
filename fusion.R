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
sections = c('news','justice','pop-culture','sex-life','real-future','voices','show','latin-america', 'soccer-gods','homepage')
table = table[table$customDimension9 %in% sections,]
user.topic = select(table, fullVisitorId, customDimension)


library(tidyr)
library(reshape2)

#Get counts of visitors to article topics - SLOW!! (skip)
registerDoSNOW(makeCluster(3, type = "SOCK"))
user.counts = ddply(user.topic, .(fullVisitorId, customDimension9), count, .parallel = TRUE, .progress= 'text')


##Much faster!!
user.counts = user.topic %>% 
              group_by(fullVisitorId, customDimension9) %>% 
              summarize(counts = n())

user.counts.wide = spread(ungroup(user.counts), customDimension9, counts, fill=0)


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
  geom_tile()+
  xlab('Factor')+
  ylab('Topic')+
  geom_text(size=4, alpha = 0.8)+
  scale_fill_continuous(low='yellow', high='red', name='Loadings')+
  theme(axis.text.y = element_text(size=8))+
  theme_bw()+
  ggtitle('Factor Correlations')

#Visualize Coordinates - dist matrix too memory intensive for day of data (123Gb)
library(rgl)
PCs = pca$scores
plot3d(PCs[,2], PCs[,1], PCs[,4])
plot3d(PCs[,2], PCs[,1], PCs[,3])

#Cluster Data
set.seed(400)
cluster = kmeans(processed, 5, nstart=1000)

#Visualize Clusters
plot3d(PCs[,2], PCs[,1], PCs[,4], col=cluster$cluster)
plot3d(PCs[,2], PCs[,1], PCs[,3], col=cluster$cluster)


##Doesn't work with a fill-day sample size, dist matrix is 123Gb!
library(cluster)
dissProc = daisy(processed)
dissProc2 = dissProc^2
plot(silhouette(cluster$cluster, dist.proc))

#What do the clusters look like?
ctrs = cluster$centers
ctrs_m = melt(ctrs)
ggplot(ctrs_m, aes(x=Var1, y=Var2, label = round(value,2), fill=value))+
  geom_tile()+
  xlab('Cluster Number')+
  ylab('Topic')+
  geom_text(size=4, alpha = 0.8)+
  scale_fill_continuous(low='yellow', high='red', name='Z-scores')+
  theme(axis.text.y = element_text(size=8))+
  theme_bw()+
  ggtitle('K-Means Cluster Centers')


#Cluster Size
rel.size = as.data.frame(table(cluster$cluster)/length(cluster$cluster))

ggplot(rel.size, aes(x=Var1, y=Freq, fill=factor(Var1)))+
      geom_bar(stat='identity')+
      theme_bw()+
      ylab('Proportion')+
      xlab('Cluster')+
      scale_fill_discrete(name='Cluster')+
      ggtitle('Relative Segment Size')



#How often do they share?
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
        geom_text()+
        ggtitle('Proportion of Users Sharing by Class')

#Where do they share?
shares2 = labelled.table %>% filter(shares == 1)%>%
  group_by(label, hits_social_socialInteractionNetwork) %>% 
  summarize(count = n()) %>% 
  mutate(by.class = sum(count), percent = count/by.class*100)

ggplot(shares2, aes(x=hits_social_socialInteractionNetwork, y=percent, fill=hits_social_socialInteractionNetwork))+
  geom_bar(stat='identity')+
  facet_wrap(~label)+
  theme_bw()+
  xlab('Social Network')+
  ylab('Percentage')+
  scale_fill_discrete('Network')+
  theme(axis.text.x=element_blank())+
  ggtitle("Social Network Preferences by Segment")


#How do users find fusion?

#take out random m. or l.facebook and .coms & condense t.co and twitter to twitter
labelled.table$trafficSource_source = gsub('[[:lower:]]*.*facebook.*[[:lower:]]*', 'facebook', labelled.table$trafficSource_source)
labelled.table$trafficSource_source = gsub('t.co', 'twitter', labelled.table$trafficSource_source)


origin = as.data.frame(labelled.table) %>% group_by(label, trafficSource_source) %>%
          summarize(by.source = n()) %>%
          mutate(by.class = sum(by.source), percent = by.source/by.class) %>%
          arrange(label, desc(percent)) %>% slice(1:5)

origin = origin[complete.cases(origin),]

ggplot(origin, aes(x=trafficSource_source, y=percent, fill = factor(trafficSource_source)))+
        geom_bar(stat='identity')+
        theme_bw()+
        facet_wrap(~label)+
        xlab('Traffic Origin')+
        ylab('Percentage of Traffic')+
        theme(axis.text.x = element_blank())+
        scale_fill_discrete(name='Source')+
        ggtitle('Traffic Origin by Segment')

#How long do users stay?

sources = unique(origin$trafficSource_source)

time = as.data.frame(labelled.table) %>% filter(trafficSource_source %in% sources) %>%
  group_by(label, trafficSource_source) %>%
  summarize(ave.min = mean(hits_time/60000)) %>%
  arrange(desc(ave.min)) %>%
  slice(1:5)


ggplot(time, aes(x=trafficSource_source, y=ave.min, fill = factor(trafficSource_source)))+
  geom_bar(stat='identity')+
  theme_bw()+
  facet_wrap(~label)+
  xlab('Traffic Origin')+
  ylab('Length of Stay (min)')+
  theme(axis.text.x = element_blank())+
  scale_fill_discrete(name='Source')+
  ggtitle('User Engagement by Segment')


##Filter by fusion 'fans', people with > 3 visits
fanIds = table %>% filter(visitNumber > 3) %>% select(fullVisitorId)
fan.table = table %>% filter(fullVisitorId %in% fanIds[,1])

#Dimension10 - break up topics into components - ONLY for DIM 10******
table.justice = filter(table, customDimension9 == 'justice')
tags = strsplit(table.justice$customDimension10, ' ')
names(tags) = table.justice$fullVisitorId
tags = unlist(tags)
#167
tags = gsub('transgender|same-sex-marriage|anti-gay-laws|gay|lgbt-marriage', 'lgbt', tags)
tags = gsub('marijuana-legalization|cannabusiness-report|marijuana-law', 'marijuana', tags) #163
tags = gsub('drug-wars-confidential|cartel-confidential|drug-war|cartel', 'drugs', tags) #160
tags = gsub('immigration-policy|immigration-bill|immigration-youth|immigrant-youth|immigration-reform|undocumented-immigrants', 'immigration', tags) #156 
tags = gsub('race-and-racism|race-relations|racism', 'race', tags)
tags = gsub('gun-violence', 'guns', tags)
tags = gsub('electronic-music', 'music', tags)
tags = gsub('gun-violence', 'guns', tags)
tags = gsub('hispanic', 'latino', tags)
tags = gsub('dogs', 'animals', tags)
tags = gsub('police-2|police-brutality', 'police', tags)
tags = gsub('college-life', 'college', tags)
tags = gsub('freddie-gray|baltimore|ferguson|riseup|baltimore-riots', 'protests', tags)
tags = gsub('environment|climate-change', 'green', tags)
tags = gsub('millennials', 'youth', tags)
tags = gsub('streaming-video|viral|trending', 'social-media', tags)
tags = gsub('television', 'tv', tags)

justice.topic = data.frame(fullVisitorId = names(tags), topic = tags)

justiceuser.counts = justice.topic %>% 
  group_by(fullVisitorId, topic) %>% 
  summarize(counts = n())

library(tidyr)
justice.counts.wide = spread(as.data.frame(justiceuser.counts), topic, counts, fill=0)

#Exploratory Factor Analysis
processed = as.data.frame(justice.counts.wide)
processed = scale(processed[,-1], center=TRUE, scale=TRUE)

library(psych)
library(GPArotation)
pca = principal(processed, nfactor=9, covar=FALSE)
pca$loadings

#Visualize Factor Loadings
library(reshape2)
loadings = as.data.frame(pca$loadings[,1:9])
loadings$topic = rownames(loadings)
loadings_m = melt(loadings, id='topic')

library(ggplot2)
ggplot(loadings_m, aes(x=variable, y=topic, label = round(value,2), fill=value))+
  geom_tile()+
  xlab('Factor')+
  ylab('Topic')+
  geom_text(size=4, alpha = 0.8)+
  scale_fill_continuous(low='yellow', high='red', name='Loadings')+
  theme(axis.text.y = element_text(size=8))+
  theme_bw()+
  ggtitle('Factor Correlations')

#Visualize Coordinates - dist matrix too memory intensive for day of data (123Gb)
library(rgl)
PCs = pca$scores
plot3d(PCs[,2], PCs[,1], PCs[,4])
plot3d(PCs[,2], PCs[,1], PCs[,3])

#Cluster Data
set.seed(400)
library(cluster)
#cluster = kmeans(processed, 5, nstart=1000)

wss <- (nrow(processed)-1)*sum(apply(processed,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(processed,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


library(NbClust)
nb <- NbClust(processed, distance = "euclidean", 
              min.nc=2, max.nc=15, method = "kmeans", 
              index = "all")
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

library(fpc)
pamk.best <- pamk(processed)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(processed, pamk.best$nc))

set.seed(400)
cluster = kmeans(processed, 9, nstart=100)

sort(cluster$centers[1,], decreasing=T)[1:5]
sort(cluster$centers[2,], decreasing=T)[1:5]
sort(cluster$centers[3,], decreasing=T)[1:5]
sort(cluster$centers[4,], decreasing=T)[1:5]
sort(cluster$centers[5,], decreasing=T)[1:5]
sort(cluster$centers[6,], decreasing=T)[1:5]
sort(cluster$centers[7,], decreasing=T)[1:5]
sort(cluster$centers[8,], decreasing=T)[1:5]
sort(cluster$centers[9,], decreasing=T)[1:5]
