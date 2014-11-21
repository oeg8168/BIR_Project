####################
# 
#   Count TF-IDF for single document
#

getTFIDF <- function(doc, wordFreq, processed = T){
    
    if(processed)
        docWord <- doc$wordProcessed
    else
        docWord <- doc$word
    
    result <- data.frame(table(docWord))
    
    result$IDF <- wordFreq$IDF[sapply(result$docWord, function(term) which(wordFreq$word %in% term))]
        
    TF_WIKI <- result$Freq / sum(result$Freq)
    
    TF_PPT <- 1 + log10(result$Freq)
    
    TF_OTHER <- result$Freq # need to be modify
    
    result$TFIDF_WIKI <- TF_WIKI * result$IDF
    
    result$TFIDF_PPT <- TF_PPT * result$IDF
    
    result$TFIDF_OTHER <- TF_OTHER * result$IDF
    
    return(result)    
}
