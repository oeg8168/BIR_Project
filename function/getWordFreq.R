####################
# 
#   Compute term frequency(TF) and doc frequency(DF)
# 

getWordFreq <- function(docSet,             # Input document set
                        processed = TRUE,   # Compute (origin / processed) words
                        atLeast = 5,        # Filter of low TF terms
                        sortBy = "TF",      # Sort the result by (TF / DF)
                        relations = FALSE   # Compute word-doc relation or not
                        )
{
    # Determine what word will be use (origin / processed)
    if(processed)
        wordSet = docSet$wordProcessed
    else
        wordSet = docSet$word
    
    # Count term frequency (TF)
    termFreq <- table(unlist(wordSet))

    # Count document frequency (DF)
    docFreq <- table(unlist(sapply(wordSet, unique)))
    
    # Gather TF and DF together
    freq <- merge(termFreq, docFreq, by = "Var1")

    # Set column names
    colnames(freq) <- c("word", "TF", "DF")
    
    # Filter of low TF terms
    freq <- freq[freq$TF>atLeast, ]
    
    # (optional) Count word-doc relation
    if(relations){
        wordRelation <- lapply(freq$word, 
                               FUN = function(word) 
                                   inWhichDocPMID(word, docSet, processed))
        freq <- cbind(freq, cbind(wordRelation))
    }
    
    # Sort frequency table
    freq <- freq[order(freq[sortBy], decreasing = T), ]
    
    # Index rearrangement
    row.names(freq) <- 1:nrow(freq)
        
    return(freq)
}
