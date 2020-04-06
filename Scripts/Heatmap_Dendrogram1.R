Heatmap_Dendrogram1  <- function () {

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
  ImpF <- read.csv("D:/Data/R_clusters/2017-04-17-prot_dend.csv")
  
  #cluster data
  z <-ImpF[,c(2,3)]
  
  # calculating distances
  d <-dist(log10(z$UP))
  
  # diana cluster
  dv <- diana(d, metric = "manhattan", stand = TRUE)
  
  #export to EPS
  #if you want to export to EPS -> remove the '#' from the 2 lines below and comment the PDF export line
  #setEPS()
  #postscript("your_path_to_exported_file", width=15, height=15)
  
  #export to PDF
  pdf("D:/Data/R_clusters/dendrogram(2017-04-17).pdf", width=15, height=15)
  
  # plot dendrogram
  pltree(dv,labels=ImpF$STAY.dev, cex = 0.6, hang = -1, main = "Dendrogram of diana")
  
  #export ends after dev.off()
  dev.off()
  
  # heatmap data
  df <- ImpF[,c(2,3)]
  
  
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
  #postscript("D:/Data/R_clusters/heatmap.EPS", width=15, height=15)
  
  #export to PDF
  pdf("D:/Data/R_clusters/heatmap(2017-04-17).pdf", width=15, height=15)
  
  #color palette
  my_palette <- colorRampPalette(c("darkblue", "white"))
  
  #generating heatmap
  #if you want to play with the colors... you have to play with 'breaks' argument. 
  heatmap.2(as.matrix ( newVar ),trace='none', breaks=c(seq(0,1.0,0.0005),  seq(1.01, 2.94, 0.0008)), col=my_palette, dendrogram='none', Rowv=FALSE, Colv=FALSE, main='Pure Heatmap' )
  
  
  #export ends after dev.off()
  dev.off()
}