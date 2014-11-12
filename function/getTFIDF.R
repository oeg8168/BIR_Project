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
    
    result$Freq <- result$Freq/sum(result$Freq)
    
    colnames(result) <- c("word", "TF")
    
    result$IDF <- wordFreq$IDF[sapply(result$word, function(term) which(wordFreq$word %in% term))]
    
    result$TFIDF <- result$TF * result$IDF
    
    return(result)    
}
