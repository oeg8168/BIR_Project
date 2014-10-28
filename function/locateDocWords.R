####################
# 
#   Draw words in a document on the plot
#   !!! Can be use only if there is a chart !!!
# 

locateDocWords <- function(doc, DF){
    require(stringdist)    
    
    x <- which(DF$word %in% doc@words)
    y <- freqDF[["freq"]][x]
    color <- "red"
    
    points(x, y, col = "red")
}
