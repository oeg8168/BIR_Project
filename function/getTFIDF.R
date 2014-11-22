####################
# 
#   Count TF-IDF for single document
#

getTFIDF <- function(doc, wordFreq, processed = T){
    
    # Determine what word will be use (origin / processed)
    if(processed)
        docWord <- doc$wordProcessed
    else
        docWord <- doc$word
    
    # Collect words and construct freq table
    result <- data.frame(table(docWord))
    
    # Save IDF
    result$IDF <- wordFreq$IDF[sapply(result$docWord, function(term) which(wordFreq$word %in% term))]
    
    # Count TF-IDF by formula from WIKI
    result$TFIDF_WIKI <- (result$Freq / sum(result$Freq)) * result$IDF
    
    # Count TF-IDF by formula from course PPT
    result$TFIDF_PPT <- (1 + log10(result$Freq)) * result$IDF
    
    # Count TF-IDF by formula from paper
    result$TFIDF_OTHER <- (log10(1 + (1 + log10(result$Freq)))) * result$IDF
    
    return(result)    
}
