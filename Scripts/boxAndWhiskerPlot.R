boxAndWhisker <- function () {
  
  #reading data from csv file
  ImpF <- read.csv("C:/Users/GeorgiDanovski/Desktop/RPlot1/RAD1_All.csv")
  for(i in 1:ncol(ImpF))
  {
    #find right columns
    if (!i %% 2){
      next
    }
    j <- i+1
    #SingleData
    df <- ImpF[,c(i,j)]
    #name
    name <- colnames(df)[2]
    
    name <- paste("C:/Users/GeorgiDanovski/Desktop/RPlot1/",name ,".pdf", sep = "")
    
    #export to PDF
    pdf( name, width=15, height=15)
    
    #plot
    boxplot(df, ylim=c(0,2500), main = colnames(df)[2])
    
    #export ends after dev.off()
    dev.off()
    #message
    print(paste(name,"done",sep = " - "))
  }
  
}