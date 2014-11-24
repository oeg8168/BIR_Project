SPIMI <- function(docSet,
                  dictSize = 1000,
                  processed = TRUE,   # Compute (origin / processed) words
                  intermediatePath = "./intermediate_SPIMI"
                  )
{
    n <- 0
    
    intermediatePath <- paste(intermediatePath, 
                              paste(Sys.Date(), format(Sys.time(), "%H%M%S")),
                              sep = "/")
    
    dir.create(intermediatePath)
    
    if(processed)
        docSet <- subset(docSet, select = c(PMID, wordProcessed))
    else
        docSet <- subset(docSet, select = c(PMID, word))
    
    colnames(docSet) <- c("PMID", "word")
    
    dictionary <- list()
    
    for(i in 1:nrow(docSet))
    {
        for(word in unique(docSet$word[[i]]))
            dictionary[[word]] <- c(dictionary[[word]], docSet$PMID[[i]])
        
        if(length(dictionary) > dictSize || i==nrow(docSet)){
            dput(x = dictionary, file = file.path(intermediatePath, (n = n+1)))
            dictionary <- list()
        }
    }

    # MERGEBLOCKS(f1,...,fn;f merged)
    {
        intermediateFiles <- list.files(intermediatePath, full.names = T)
        indexList <- unname((lapply(intermediateFiles, dget)))
        
        result <- list()
        
        for(i in 1:length(indexList))
            result <- c(result, indexList[[i]])
        
        words <- sort(unique(row.names(cbind(result))))
        
        result <- sapply(words, 
                         FUN = function(word)
                         {
                             unlist(result[which(row.names(cbind(result)) %in% word)], use.names = F)
                         }
        )
    }
    
    return(result)
}
