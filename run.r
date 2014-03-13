#!/usr/local/bin/Rscript

library(ape)
#library(cluster) 

title <- "Hierarchical Clusters (ward method)"

mydata <- read.csv('./scores-main.csv',header=TRUE,stringsAsFactors=FALSE)

mydata.dist <- dist(mydata,method = "euclidean")

mydata.fit <- hclust(mydata.dist, method="ward")
pdf(file='clusters-clydo-ward.pdf', height=11, width=8, onefile=TRUE, family='Helvetica', paper='letter', pointsize=10)
plot(as.phylo(mydata.fit),cex=0.6,  main = title, type="cladogram", label.offset=0.1)


title <- "Hierarchical Clusters (complete method)"
mydata.fit <- hclust(mydata.dist, method="complete")
pdf(file='clusters-clydo-complete.pdf', height=11, width=8, onefile=TRUE, family='Helvetica', paper='letter', pointsize=10)
plot(as.phylo(mydata.fit),cex=0.6,  main = title, type="cladogram", label.offset=0.05)


title <- "K-Means Clusters (k=2)"
mydata.fit <- kmeans(mydata, 2)
print(mydata.fit)
pdf(file='clusters-kmeans-2.pdf', height=8, width=11, onefile=TRUE, family='Helvetica', paper='letter', pointsize=10)
clusplot(mydata, mydata.fit$cluster, color=TRUE, shade=FALSE, labels=2, lines=0, cex=0.5, main = title)


title <- "K-Means Clusters (k=3)"
mydata.fit <- kmeans(mydata, 3)
print(mydata.fit)
pdf(file='clusters-kmeans-3.pdf', height=8, width=11, onefile=TRUE, family='Helvetica', paper='letter', pointsize=10)
clusplot(mydata, mydata.fit$cluster, color=TRUE, shade=FALSE, labels=2, lines=0, cex=0.5, main = title)


title <- "K-Means Clusters (k=4)"
mydata.fit <- kmeans(mydata, 4)
print(mydata.fit)
pdf(file='clusters-kmeans-4.pdf', height=8, width=11, onefile=TRUE, family='Helvetica', paper='letter', pointsize=10)
clusplot(mydata, mydata.fit$cluster, color=TRUE, shade=FALSE, labels=2, lines=0, cex=0.5, main = title)



dev.off()