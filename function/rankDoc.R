rankDoc <- function(query,
                    docSet,
                    wordFreq,
                    TFmethod = "TFIDF_WIKI",
                    processQuery = TRUE,
                    topN = 10
                    )
{
    # Convert words encoding
    query <- iconv(query, to="ASCII", sub = "byte")
    
    # Split content into words
    query <- strsplit(tolower(query), "[[:space:]]+|[[:punct:]]+")
    
    # Remove empty words
    query <- query[query!=""]
    
    # Process words
    queryProcessed <- data.frame(table(processWord(query)))
    
    colnames(queryProcessed) <- c("word", "Freq")
    
    queryProcessed$IDF <- sapply(queryProcessed$word, 
                                 function(term) {
                                     if(term %in% wordFreq$word) 
                                         wordFreq$IDF[which(wordFreq$word %in% term)]
                                     else
                                         return(as.numeric(0))
                                    }
                                 )
    
    # Count TF-IDF by formula from WIKI
    queryProcessed$TFIDF_WIKI <- (queryProcessed$Freq / sum(queryProcessed$Freq)) * queryProcessed$IDF
    
    # Count TF-IDF by formula from course PPT
    queryProcessed$TFIDF_PPT <- (1 + log10(queryProcessed$Freq)) * queryProcessed$IDF
    
    # Count TF-IDF by formula from paper
    queryProcessed$TFIDF_OTHER <- (log10(1 + (1 + log10(queryProcessed$Freq)))) * queryProcessed$IDF
    
    vQuery <- unlist(queryProcessed[TFmethod])
    
    similarity <- apply(docSet, 
                        MARGIN = 1,
                        FUN = function(doc) {
                            vDoc <- lapply(queryProcessed$word,
                                           function(word) {
                                               if(word %in% doc$wordProcessed)
                                                   switch(TFmethod,
                                                          TFIDF_WIKI = (doc$TFIDF$TFIDF_WIKI[doc$TFIDF$docWord %in% word]),
                                                          TFIDF_PPT = (doc$TFIDF$TFIDF_PPT[doc$TFIDF$docWord %in% word]),
                                                          TFIDF_PAPER = (doc$TFIDF$TFIDF_PAPER[doc$TFIDF$docWord %in% word])
                                                          )    
                                               else
                                                   return(as.numeric(0))
                                           }
                                           )
                            
                            vDoc <- unlist(vDoc)
                            dot <- vDoc %*% vQuery
                            score <- dot / (sqrt(sum(vDoc^2)) * sqrt(sum(vQuery^2)))
                            
                            return(score)
                            }
                        )
                                                    
    similarity <- data.frame(cbind(docSet$PMID, similarity))
    similarity <- similarity[order(unlist(similarity[2]), decreasing = T),  ]
    colnames(similarity) <- c("PMID", "score")
    # Index rearrangement
    row.names(similarity) <- 1:nrow(similarity)
    
    return(similarity[1:topN, ])

}
