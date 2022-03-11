#Jenny Smith

#March 31,2017 

#Purpose: To create a waterfall (barplot) of patient expression data, or LSC17 scores. 
setwd(file.path(PROJHOME,"2018.04.05_ROR1_MEN1_Waterfallplots_Expression"))

waterfallPlot <- function(df, geneName, genesColumn, colnames, color){
  #df is the gene expression data frame with patient IDs as a column names. 
  #NOTE: ENSURE all strings in the expn data frame are character class, not factors. 
  #geneName is the vector of gene name(s)
  
  library(colorspace)
  library(RColorBrewer)
  require(ggplot2)
  
  if (length(geneName) == 1){
    expn <- df[which(grepl(geneName, df[,genesColumn])),]
    expn <- expn[,which(! grepl("[[:alpha:]]", expn[1,]))]
    expn <- data.frame(t(expn))
    expn <- cbind(rownames(expn), expn)
    colnames(expn) <- colnames
  }else{
    print("Please input only one gene at a time")
  }
  
  plot <- ggplot(data = expn, aes(x=reorder(expn[,colnames[1]],expn[,colnames[2]]), y=expn[,colnames[2]])) +
    geom_bar(stat ="identity", color = "black", fill=color) +
    theme(plot.title = element_text(hjust = 0.5, size = 18),
          panel.background = element_rect(fill="white"),
          panel.grid.major = element_line(colour = "gray"),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45,hjust = 1, vjust = 0.5, size = 3),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 15)) +
    labs(x=colnames[1], y=colnames[2], title="")
  
  list <- list(expn, plot)
  return(list)
}


