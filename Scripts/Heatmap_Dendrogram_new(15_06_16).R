Heatmap_Dendrogram_new <- function () {
  
  #adding libraries
  #if you have installed and loaded them, you can comment the lines below.
  # install.packages('wmtsa', dependencies=TRUE)
  library(wmtsa)
  #install.packages('ifultools', dependencies=TRUE)
  library(ifultools)
  #install.packages('htmlwidgets', dependencies=TRUE)
  library(htmlwidgets)
  #install.packages('TSdist', dependencies=TRUE)
  library(TSdist)
  #install.packages('TSclust')
  library(TSclust)
  #install.packages("d3heatmap")
  library("d3heatmap")
  #install.packages("plyr")
  library("plyr")
  #heatmap.2
  #install.packages("gplots")
  library("gplots")
  #distfun & hclustfun
  #install.packages("spatstat")
  library(spatstat)
  #dendrogram libraries
  #install.packages("cluster")
  library(cluster)
  #install.packages("dendextend")
  library(dendextend)
  
  #reading data from csv file
  ImpF <- read.csv("D:/Desktop/dendrograms/cluster_rem_01_12_17.csv")
  
  #cluster data
  z <-ImpF[,c(2,3)]
  
  # calculating distances
  d <-dist(log10(z$UP))
  
  # diana cluster
  dv <- diana(d, metric = "manhattan", stand = TRUE)
  
  # plot dendrogram
  pltree(dv,labels=ImpF$STAY.dev, cex = 0.6, hang = -1, main = "Dendrogram of diana")

  # heatmap data
  df <- ImpF[,c(1,3)]
  
  
  #generating empty matrix NxN
  newVar <- matrix( nrow = nrow(df), ncol = nrow(df))
  
  #generating matrix of dissimilarities
  for (i in 1:nrow(df))
  {
    for (j in 1:nrow(df)) 
    {
      newVar[i, j] <- sqrt( sum ( (log10(df[i,2])-log10(df[j,2]) )^2 ) ) 
      
    }
    
  }
  
  #variable containing names of proteins, so heatmap is labeled
  c123 <- df[,1]
  
  colnames(newVar) <- c123
  rownames(newVar) <- c123
  
  #export to EPS
  #if you want to export to EPS -> remove the '#' from the 2 lines below and comment the PDF export line
  #setEPS()
  #postscript("D:/Data/R_clusters/heatmap_dendrogram.eps", width=15, height=15)
  
  #export to PDF
  pdf("D:/Desktop/dendrograms/res/heatmap_dendrogram(2017-12-01)_down.pdf", width=15, height=15)

  #color palette
  my_palette <- colorRampPalette(c("darkblue", "white"))
  
  #generating heatmap
  #if you want to play with the colors... you have to play with 'breaks' argument. 
  heatmap.2(as.matrix ( newVar ), trace='none', breaks=c(seq(0,0.5,0.004),  seq(0.51, 2.94, 0.015) ), col=my_palette, dendrogram='col', Rowv=rev(as.dendrogram(dv)), Colv= as.dendrogram(dv),labRow = ImpF$STAY.dev,cexRow = 0.6, cexCol = 0.6, labCol = ImpF$STAY.dev , main='Dendrogram and Heatmap' )
  
  #export ends after dev.off()
  dev.off()
}