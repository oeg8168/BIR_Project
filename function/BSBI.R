BSBI <- function(docSet,
                 blockSize = 10,    #
                 processed = TRUE,   # Compute (origin / processed) words
                 intermediatePath = "./BSBI_intermediate"
                 )
{
    n <- 0
    i <- 0
    j <- 0
    
    intermediatePath <- paste(intermediatePath, 
                              paste(Sys.Date(), format(Sys.time(), "%H%M%S")),
                              sep = "/")
    
    dir.create(intermediatePath)
    
    if(processed)
        docSet <- subset(docSet, select = c(PMID, wordProcessed))
    else
        docSet <- subset(docSet, select = c(PMID, word))
    
    colnames(docSet) <- c("PMID", "word")
    
    while(j < nrow(docSet))
    {
        n <- n + 1
        i <- (n-1)*blockSize + 1
        j <- min(n*blockSize, nrow(docSet))
        
        # block<-PARSENEXTBLOCK()  
        {   
            docBlock <- docSet[i:j, ]
            
            pairs <- apply(docBlock, MARGIN = 1,
                           FUN = function(doc)
                               { rbind(sort(doc$word), doc$PMID) }
                           )
            
            pairs <- data.frame(matrix(unlist(pairs), ncol = 2, byrow = T), stringsAsFactors = F)
        }
        
        # BSBI-INVERT(block)
        {
            pairs <- pairs[order(pairs[[1]], pairs[[2]]), ]
            uniquePairs <- unique(pairs)
            uniquePairs[[1]] <- iconv(uniquePairs[[1]], to="ASCII", sub = "byte")
            invertedIndex <<- sapply(unique(uniquePairs[[1]]), 
                                    FUN = function(word)
                                        { c(uniquePairs[[2]][which(uniquePairs[[1]]==word)]) }
                                    )
        }
        
        # WRITEBLOCKTODISK(block,fn) 
        dput(x = invertedIndex, file = file.path(intermediatePath, n))
        
    }
    
    # MERGEBLOCKS(f1,...,fn;f merged)
    {
        intermediateFiles <- list.files(intermediatePath, full.names = T)
        indexList <- unname((lapply(intermediateFiles, dget)))
        
        result <- list()
        
        for(i in 1:length(indexList))
            result <- c(result, indexList[[i]])
        
        words <- unique(row.names(cbind(result)))
        
        result <- sapply(words, 
                         FUN = function(word)
                             {
                             unlist(result[which(row.names(cbind(result)) %in% word)], use.names = F)
                             }
                         )
        
    }
    
    return(result)
}
